/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.io.sourceobject;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.compress.utils.IOUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.StopWatch;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.gson.Gson;

import brave.Tracer;
import brave.Tracer.SpanInScope;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.Logging;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.CachingSupplier;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.configuration.GenericConfiguration;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.io.MiningFileIndex;
import innowake.mining.shared.io.MiningFileIndex.File;
import innowake.mining.shared.io.SecuredZipInputStream;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Service for importing Source object from a zipped {@link InputStream} into Mining server.
 */
@Service
public class SourceObjectImportService {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.IO);

	@Autowired
	private SourceService sourceService;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private ProjectService projectService;
	@Autowired
	private Tracer tracer;
	@Autowired
	private GenericConfiguration configProperties;

	/**
	 * Imports SourceObject from a zipped {@link InputStream} into the given project ID.
	 *
	 * @param progressMonitor the {@link ProgressMonitor}
	 * @param projectId the ID of the project
	 * @param streamId the identification of the {@link InputStream}
	 * @param inputStream the zipped {@link InputStream} of the source object code to import
	 * @return {@link MiningFileIndex} the miningFileIndex object with updated properties
	 * @throws IOException in case of an error while accessing the {@link InputStream}
	 */
	public MiningFileIndex importSourceObjects(final ProgressMonitor progressMonitor, final EntityId projectId, final String streamId,
			final InputStream inputStream) throws IOException {
		progressMonitor.checkCanceled();

		final StopWatch watch = new StopWatch();
		if (LOG.isInfoEnabled()) {
			watch.start();
			LOG.info(String.format("Importing source objects '%s' into project %s", StringUtils.isNotBlank(streamId) ? streamId : "files", projectId));
		}
		final long sourceCodeRevision = projectService.incrementSourceCodeRevision(projectId);

		try (final SecuredZipInputStream zipIn = new SecuredZipInputStream(inputStream, StandardCharsets.UTF_8)) {
			zipIn.limitSize(configProperties.getDiscoverySourceUploadMaximumSize())
				 .limitEntries(configProperties.getDiscoverySourceUploadMaximumEntries())
				 .limitRatio(configProperties.getDiscoverySourceUploadMaximumRatio());

		    final MiningFileIndex miningFileIndex = retrieveMiningFileIndex(zipIn);
		    processReceivedContent(progressMonitor, miningFileIndex, zipIn, projectId, sourceCodeRevision); /* Performs the upload operation */
		    miningFileIndex.setSourceCodeRevision(sourceCodeRevision);
		    miningFileIndex.setScope("/");
		    miningFileIndex.setFiles(sourceService
		    		.find(q -> q.ofProject(projectId))
		    		.stream()
		    		.map(MiningFileIndex.File::setFromSourceObject)
		    		.collect(Collectors.toList())); 

		    if (LOG.isInfoEnabled()) {
		        watch.stop();
		        LOG.info(String.format("Overall import of '%s' took %s (H:mm:ss.SSS)", StringUtils.isNotBlank(streamId) ? streamId : "files", watch.toString()));
		    }
		    return miningFileIndex;
		}
	}

	private void processReceivedContent(final ProgressMonitor progressMonitor, final MiningFileIndex receivedMiningFileIndex, final ZipInputStream zipIn,
			final EntityId projectId, final long sourceCodeRevision) throws IOException {
		final int availableProcessors = Runtime.getRuntime().availableProcessors();
		final int poolSize = availableProcessors / 2;
		LOG.info("Creating ExecutorService with pool size: {}", poolSize);
		if (poolSize <= 0) {
			LOG.error("Invalid pool size: {}. Pool size should be greater than 0.", poolSize);
			throw new IllegalArgumentException("Please ensure that available processor count is at least 2. Currently the available number of "
					+ "processors are : " + availableProcessors);
		}
		final List<File> receivedIndexFiles = receivedMiningFileIndex.getFiles();
		final ProjectPojo project = projectService.get(projectId);
		final Map<String, File> receivedIndexFilesAsMap = receivedIndexFiles.stream()
				.filter(File::isContentIncluded)
				.collect(Collectors.toConcurrentMap(File::getPath, f -> f));
		final Map<String, SourcePojo> sourceObjectsInScope = sourceObjectsInScope(project.identity(), receivedMiningFileIndex);
		ZipEntry entry;
		final ExecutorService executorService = Executors.newFixedThreadPool(poolSize);
		try {
			final List<Future<?>> tasks = new ArrayList<>();

			/* Process the files that are already present in server in a new thread */
			tasks.add(executorService.submit(() -> processFilesNotIncluded(receivedMiningFileIndex, sourceObjectsInScope)));

			final Integer total = Integer.valueOf(receivedIndexFilesAsMap.size());
			int cnt = 0;
			final AtomicInteger taskCnt = new AtomicInteger();
			while ((entry = zipIn.getNextEntry()) != null) {
				if (cnt++ % 10_000 == 1) {
					progressMonitor.checkCanceled();
					progressMonitor.setStepDescription(String.format("Processing source files (%d/%d)", Integer.valueOf(cnt), total));
				}

				/* The actual copy of the zip entry cannot be done in multiple threads,
				 * therefore this is being excluded from the parallelization. */
				final String path = entry.getName();
				final BinaryString content;
				try {
					final ByteArrayOutputStream entryStream = new ByteArrayOutputStream();
					IOUtils.copy(zipIn, entryStream);
					if (entryStream.size() > SourceService.CONTENT_SIZE_LIMIT) {
						LOG.error(String.format("SourceObject: %s is too large (%d>%d)", path, entryStream.size(), SourceService.CONTENT_SIZE_LIMIT));
						/*
						 * The larger files are skipped from upload. Its's MiningFileIndex entry is not removed because if an existing SourceObject exists it
						 * should not be removed
						 */
						continue;
					}
					final var processedContent = removeNullBytes(entryStream.toByteArray());
					content = new BinaryString(processedContent);
				} finally {
					zipIn.closeEntry();
				}

				tasks.add(executorService.submit(() -> {
					progressMonitor.checkCanceled();
					final int taskCounter = taskCnt.incrementAndGet();
					if (taskCounter % 100 == 1) {
						progressMonitor.setStepDescription(String.format("Importing source files (%d/%d)", Integer.valueOf(taskCounter), total));
					}
					try (final SpanInScope spanInScope = tracer.withSpanInScope(tracer.currentSpan())) {
						final String name = FilenameUtils.getBaseName(path);
						
						final ModuleType moduleType = FileExtension.resolve(path);
						final Technology technology = moduleType.getTechnology();
						final Type type  = moduleType.getType();
						SourcePojoPrototype newSourceObject = new SourcePojoPrototype()
								.setProject(project.identity())
								.setName(name)
								.setPath(path)
								.setTechnology(technology)
								.setType(type)
								.setContentRevision(sourceCodeRevision)
								.setContent(content);
						
						final File miningIndexFile = receivedIndexFilesAsMap.remove(path);
						if (miningIndexFile == null) {
							LOG.warn(() -> "MiningFileIndex is missing the file : " + path);
						} else {
							final SourcePojo existingSourceObject = sourceObjectsInScope.remove(path);
							boolean sourceObjectDeleted = false;
							if (existingSourceObject != null) { /* Existing source objects in server that have some changes in received entry */
								if (miningIndexFile.getId() == null || ! existingSourceObject.getId().equals(miningIndexFile.getId())) {
									sourceService.remove(existingSourceObject.identity(), project.identity());
									sourceObjectDeleted = true;
								} else {
									/* Import operation : When content changed and Source object exist */
									sourceService.update(newSourceObject.withId(existingSourceObject.identity())
											.setTechnology(existingSourceObject.getTechnology())
											.setType(existingSourceObject.getType()));
									sourceService.removeReferences(existingSourceObject.identity());
									moduleService.updateModules(q -> q.ofProject(projectId).withPath(path),
																new ModulePojoPrototype().setSource(existingSourceObject.identity()));
								}
							}
							if (existingSourceObject == null || sourceObjectDeleted) {
								/* Import operation : Mostly when new Source object found */
								final EntityId source = sourceService.create(newSourceObject);
								moduleService.updateModules(q -> q.ofProject(projectId).withPath(path), new ModulePojoPrototype().setSource(source));
							}
						}
					}
				}));
			}
			
			progressMonitor.checkCanceled();
			final CachingSupplier<IllegalStateException> errors = new CachingSupplier<>(() -> new IllegalStateException("Failed to import source objects"));
			
			tasks.forEach(task -> {
				try {
					task.get();
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
					LOG.error(() -> "Interrupted while importing source object", e);
					errors.get().addSuppressed(e);
				} catch (final ExecutionException e) {
					LOG.error(() -> "Unable to import source object", e);
					errors.get().addSuppressed(e.getCause());
				} 
			}); 
			
			/* If there are any exceptions while importing source objects, then throw IllegalStateException. */
			if (errors.isPresent()) {
				throw errors.get();
			}
		} finally {
			executorService.shutdownNow();
		}
		/* Deletes the Source objects that are in scope but are missing in the mining file index. It is because these files were removed from the client */
		if ( ! sourceObjectsInScope.isEmpty()) {
			progressMonitor.checkCanceled();
			progressMonitor.setStepDescription("Deleting source objects missing in file index");
			sourceService.remove(sourceObjectsInScope.values().stream().map(SourcePojo::identity).collect(Collectors.toList()), project.identity());
		}
	}

	private byte[] removeNullBytes(final byte[] content) {
		/* If no null byte is present, the content can be return as it is  */
		final var firstNullIndex = ArrayUtils.indexOf(content, (byte) 0);
		if ( firstNullIndex == -1) {
			return content;
		};
		/* Removes null bytes i.e. byte with value 0 from the content */
		int replacingIndex = firstNullIndex;
		for (int index = firstNullIndex; index < content.length; index++) {
			if (content[index] != 0) {
				content[replacingIndex++] = content[index];
			}
		}
		return Arrays.copyOfRange(content, 0, replacingIndex);
	}

	private void processFilesNotIncluded(final MiningFileIndex receivedMiningFileIndex, final Map<String, SourcePojo> sourceObjectInScope) {
		receivedMiningFileIndex.getFiles().stream()
			.filter(file -> ! file.isContentIncluded())
			.forEach(file -> {
				final var fileId = file.getId();
				if (fileId == null) {
					LOG.warn(() -> String.format("Missing contents for new file %s", file.getPath()));
					return;
				}
				final SourcePojo sourceObject = sourceObjectInScope.remove(file.getPath());
				
				if (sourceObject != null && assertNotNull(file.getId()).equals(sourceObject.getId())) {
					updateSourceObjectFromFile(sourceObject, file);
				} else {
					try {
						final Optional<SourcePojo> sourceObjectIdNotMatching = sourceService.findAny(q -> q.byId(EntityId.of(fileId)));
						if (sourceObjectIdNotMatching.isEmpty()) {
							LOG.warn(() -> String.format("No existing Source Object with id %s and path %s found", fileId, file.getPath()));
							return;
						} else {
							/* source object that was matched by id rather than path must be kept, so we remove it from this map -
							 * source objects still in the map at the end of the import process are removed from the server */
							sourceObjectInScope.remove(sourceObjectIdNotMatching.get().getPath());
							updateSourceObjectFromFile(sourceObjectIdNotMatching.get(), file);
						}
					} catch (final MiningEntityNotFoundException e) {
						LOG.warn(() -> String.format("No existing Source Object with id %s and path %s found", file.getId(), file.getPath()));
						return;
					}
				}
			});
	}
	
	/**
	 * Upsert the {@link SourcePojo} from the received {@link File}.
	 *
	 * @param sourceObject to which the {@link File} properties need to be applied.
	 * @param miningIndexFile the File from which properties are taken.
	 */
	private void updateSourceObjectFromFile(final SourcePojo sourceObject, final File miningIndexFile) {
		SourcePojoPrototype changedSource = new SourcePojoPrototype();
		boolean changed = false;
		
		if ( ! sourceObject.getPath().equals(miningIndexFile.getPath())) {
			changedSource.setPath(miningIndexFile.getPath());
			changed = true;
		}

		if (miningIndexFile.getTechnology() != null && ! sourceObject.getTechnology().equals(miningIndexFile.getTechnology()) && miningIndexFile.getTechnology() != Technology.UNKNOWN) {
			changedSource.setTechnology(assertNotNull(miningIndexFile.getTechnology()));
			changed = true;
		}

		if (miningIndexFile.getType() != null && ! sourceObject.getType().equals(miningIndexFile.getType()) && miningIndexFile.getType() != Type.UNKNOWN) {
			changedSource.setType(assertNotNull(miningIndexFile.getType()));
			changed = true;
		}

		if (changed) {
			sourceService.update(changedSource.withId(sourceObject.identity()));
			moduleService.updateModules(q -> q.ofProject(sourceObject.getProject()).withPath(sourceObject.getPath()),
										new ModulePojoPrototype().setSource(sourceObject.identity()));
		}
	}

	/**
	 * It constructs the MiningFileIndex from the received {@link ZipInputStream}.
	 *
	 * @param zipIn the received stream.
	 * @return constructed MiningFileIndex.
	 * @throws IOException exception thrown during the retrieval.
	 */
	private MiningFileIndex retrieveMiningFileIndex(final ZipInputStream zipIn) throws IOException {
		final ZipEntry entry = zipIn.getNextEntry();
		if (entry == null || ! entry.getName().equals(MiningFileIndex.NAME)) {
			throw new IllegalArgumentException("Invalid file provided");
		}
		final Gson g = new Gson();
		return g.fromJson(new InputStreamReader(zipIn, StandardCharsets.UTF_8), MiningFileIndex.class);
	}

	/**
	 * Identifies all the {@link SourcePojo} that are in scope which is specified in the received {@link MiningFileIndex}.
	 *
	 * Important: It is essential that it should return a ConcurrentMap.
	 * 
	 * @param projectId the Project Id.
	 * @param receivedMiningFileIndex the received MiningFileIndex from the client.
	 * @return Map of {@link SourcePojo#getPath()} and {@link SourcePojo} itself.
	 */
	private ConcurrentMap<String, SourcePojo> sourceObjectsInScope(final EntityId projectId, final MiningFileIndex receivedMiningFileIndex) {
		String scope = receivedMiningFileIndex.getScope();
		if (scope != null && StringUtils.isNotBlank(scope) /* for Eclipse compiler sake */) {
			/* trim a leading "/" from scope, because our paths in the database do not start with "/"
			 * and add a trailing "/" to ensure we only match full path segments, and not partial folder names */
			scope = scope.startsWith("/") ? scope.substring(1) : scope;
			scope = scope.endsWith("/") || scope.isEmpty() ? scope : scope + "/";
			final String scopePath = scope + "%";
			return sourceService.find(q -> q.ofProject(projectId).withPath(scopePath)).stream()
					.collect(Collectors.toConcurrentMap(SourcePojo::getPath, s -> s));
		} else {
			return receivedMiningFileIndex.getFiles().stream()
					.map(File::getPath)
					.map(filePath -> sourceService.findAny(q -> q.ofProject(projectId).withPath(filePath)).orElse(null))
					.filter(Objects::nonNull)
					.collect(Collectors.toConcurrentMap(SourcePojo::getPath, s -> s));
		}
	}
}
