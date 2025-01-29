/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.metrics;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import org.apache.commons.collections4.ListUtils;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.util.FileSystemUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.config.DiscoveryConfigAccessor;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.FutureUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * Helper class to encapsulate all methods required for the discover metrics source export.
 * <p>In the current version only Java is supported, means only Java source files can be exported.</p>
 */
@Service
public class SourceExportService {

	private static final int DEFAULT_EXPORT_THRESHOLD = 10_000;
	private static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_COLLECTOR);
	
	/** for deleting exported sources from the file system when the corresponding job is no longer active */
	private static final int CHECK_INTERVAL = 60;

	private final Map<String, SourceExportStatus> jobStatus = new ConcurrentHashMap<>();
	private final GenericConfigProperties configProperties;
	private final SourceService sourceService;
	private final DiscoveryConfigAccessor configAccessor;
	private final JobManager jobManager;

	/**
	 * The constructor.
	 * 
	 * @param configProperties The {@link GenericConfigProperties}
	 * @param sourceService Source Object Service
	 * @param configAccessor The {@link DiscoveryConfigAccessor}
	 * @param jobManager The {@link JobManager}
	 */
	@Autowired /* Spring constructor dependency injection: By adding many parameters we get rid of the sonar warnings and improve testability */
	public SourceExportService(final GenericConfigProperties configProperties, final SourceService sourceService,
			final DiscoveryConfigAccessor configAccessor, final JobManager jobManager) {
		this.configProperties = configProperties;
		this.sourceService = sourceService;
		this.configAccessor = configAccessor;
		this.jobManager = jobManager;

		inspectExportDirectory();
	}
	
	/**
	 * Checks each jobId registered in {@code jobStatus} and deleted exported sources if the job is finished.
	 */
	@Scheduled(fixedDelay = CHECK_INTERVAL, timeUnit = TimeUnit.SECONDS)
	public void pollFinishedJobs() {
		for (final String jobId : new HashSet<>(jobStatus.keySet())) {
			@Nullable
			final JobMonitor jobMonitor = assertNotNull(jobManager).getJobMonitor(jobId);
			if (jobMonitor == null || ! JobStatus.isActive(jobMonitor.getStatus())) {
				deleteSources(jobId);
			}
		}
	}

	/**
	 * Returns the path for the export of all sources for the given {@code jobId}
	 *
	 * @param jobId the ID of the job
	 * @return the source export path
	 */
	public String getSourceExportPath(final String jobId) {
		return configProperties.getTemporaryFolder() + "/" + jobId;
	}

	/**
	 * Tests if source export is required for the given {@code sourceObject} and exports the source(s) for the provided job ID if so.
	 *
	 * @param jobId the ID of the job for which the source(s) are exported
	 * @param sourceObject the {@link SourcePojo} to export
	 */
	public void exportSourceIfRequired(final String jobId, final SourcePojo sourceObject) {
		if (sourceObject.getTechnology() == Technology.JAVA) {
			exportSources(jobId, sourceObject.getProject());
		}
	}

	/**
	 * Exports all the sources for the provided job ID and project ID.
	 *
	 * @param jobId the ID of the job
	 * @param projectId the ID of the project
	 */
	public void exportSources(final String jobId, final EntityId projectId) {
		final SourceExportStatus sourceExportStatus = jobStatus.computeIfAbsent(jobId, key -> new SourceExportStatus(getSourceExportPath(jobId)));
		if (sourceExportStatus.sourceExported) {
			return;
		}

		sourceExportStatus.lock.lock();

		try {
			/* while waiting for the lock, another thread may have already exported the sources, so check again */
			if (sourceExportStatus.sourceExported) {
				return;
			}

			/* we are first, so it's our job to do the export */
			final ExecutorService exportExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
			final List<List<EntityId>> exportIdsPartitioned = getSourceExportIdsPartitioned(projectId, jobId);

			if ( ! exportIdsPartitioned.isEmpty()) {
				
				final List<Future<?>> futures = new ArrayList<>(exportIdsPartitioned.size());
				for (final List<EntityId> sourceObjectIds : exportIdsPartitioned) {
					futures.add(exportExecutor.submit(() -> exportFiles(sourceObjectIds, sourceExportStatus.exportPath)));
				}
				FutureUtil.awaitAll(futures);
			}
			sourceExportStatus.sourceExported = true;

		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException("Interrupted during source export", e);
		} catch (final ExecutionException e) {
			throw new IllegalStateException("Error during source export", e);
		} finally {
			sourceExportStatus.lock.unlock();
		}
	}

	/**
	 * Loads the list of all to be exported {@code SourcePojo} IDs for the given {@code project}, partitions it into consecutive sublists each of the same
	 * size (last list can be shorter) and returns the partitioned list.
	 * <p>In the current version only Java is supported, means only the IDs of Java source files are returned.</p>
	 * <p>The size of the sublists can be configured for the Java technology with property: <code>EXPORT_CHUNK_SIZE</code>. Default size is {@code 10.000}</p>
	 *
	 * @param projectId the id of the project whose sources have to be exported
	 * @param jobId the id of the job
	 * @return list of consecutive sublists of {@code SourcePojo} IDs
	 */
	private List<List<EntityId>> getSourceExportIdsPartitioned(final EntityId projectId, final String jobId) {
		/* Use LinkedList for fast addAll() and later partitioning. There will be no duplicates of SourcePojo IDs */
		final List<EntityId> sourceObjectIds = sourceService.findIDs(q -> q.ofProject(projectId).withTechnology(Technology.JAVA));
		if (sourceObjectIds.isEmpty()) {
			LOG.trace(() -> String.format("Nothing to export for %s", Technology.JAVA));
			return Collections.emptyList();
		}

		final List<EntityId> exportIds = new LinkedList<>();
		LOG.trace(() -> String.format("Found %d sources files to export for %s", Integer.valueOf(sourceObjectIds.size()), Technology.JAVA));
		exportIds.addAll(sourceObjectIds);

		int threshold = configAccessor.getConfig(projectId, jobId).getExportChunkSize(ResolveTarget.JAVA);
		if (threshold < 1) {
			final Integer threshold2 = Integer.valueOf(threshold);
			threshold = DEFAULT_EXPORT_THRESHOLD;
			final Integer def = Integer.valueOf(threshold);
			LOG.trace(() -> String.format("Export threshold was %d. Falling back to default threshold: %d", threshold2, def));
		}

		return ListUtils.partition(exportIds, threshold);
	}

	private void inspectExportDirectory() {
		final String exportPath = configProperties.getTemporaryFolder();
		final Path dir = Paths.get(exportPath);
		if (Files.isDirectory(dir)) {
			try (final DirectoryStream<Path> stream = Files.newDirectoryStream(dir)) {
				stream.forEach(path -> {
					if (Files.isDirectory(path)) {
						LOG.trace(String.format("Found directory %s in source export path: %s. It could be the source export of a previous instance of the "
								+ "mining api-server that wasn't stopped gracefully. Please check if it must be deleted manually.",
								path.getFileName(), exportPath));
					}
				});
			} catch (final IOException e) {
				LOG.error(() -> "Error while reading exporting directory", e);
			}
		}
	}
	
	private void exportFiles(final List<EntityId> sourceObjectIds, final Path exportPath) {
		LOG.trace(() -> String.format("Exported for source path: %s", exportPath.toAbsolutePath().toString()));
		sourceService.findContent(EntityId.allUids(sourceObjectIds), null).forEach(source -> {
			final File file = exportPath.resolve(source.getPath()).toFile();
			/* JavaParseResultProvider uses the same Charset on the ASTParser */
			try {
				FileUtils.writeByteArrayToFile(file, source.getContent().get());
			} catch (final IOException e) {
				LOG.error(() -> "Error while exporting source files", e);
				throw new IllegalStateException(e);
			}
			LOG.trace(() -> "Exported source content of file " + source.getId() + ": " + source.getPath());
		});
	}

	/**
	 * Deletes all exported sources for the given {@code jobId}.
	 *
	 * @param jobId the ID of the job
	 */
	private void deleteSources(final String jobId) {
		final SourceExportStatus exportStatus = jobStatus.remove(jobId);
		if (exportStatus == null) {
			LOG.trace(() -> String.format("Export status is null for jobId: %s", jobId));
		} else {
			LOG.trace(() -> String.format("Deleting export path: %s for jobId: %s", exportStatus.exportPath.toAbsolutePath().toString(), jobId));
			
			/* actual source deletion */
			try {
				FileSystemUtils.deleteRecursively(exportStatus.exportPath);
				LOG.trace(() -> String.format("Deleted jobId : %s and source export path: %s", jobId, exportStatus.exportPath.toAbsolutePath().toString()));
			} catch (final IOException e) {
				LOG.error(() -> "Couldn't delete source export path: " + exportStatus.exportPath.toAbsolutePath().toString(), e);
				throw new IllegalStateException(e);
			}
		}
	}
	
	private static class SourceExportStatus {
		private final ReentrantLock lock = new ReentrantLock();
		private volatile boolean sourceExported = false;
		private final Path exportPath;
		
		private SourceExportStatus(final String exportPath) {
			this.exportPath = Paths.get(exportPath);
		}
	}
}
