/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleType;

/**
 * Provides base methods to test discovery on a project with incremental changes
 */
abstract class BaseIncrementalDiscoveryTest extends BaseDiscoveryTest {
	
	private enum Step { FIRST_STEP, SECOND_STEP }
	
	private Step currentStep = Step.FIRST_STEP;
	
	private static final Path EMPTY_PATH = Paths.get("");
	
	private String testFolderName = "";

	private static final String TEST_FOLDER = "incremental";
	protected static final Path TEST_SOURCE_FOLDER = SOURCE_FOLDER.resolve(TEST_FOLDER);
	private Path testFolder = EMPTY_PATH;
	private Path testFolderUpdated = EMPTY_PATH;
	private Map<String, SourcePojo> sourceObjects = new HashMap<>();
	
	@BeforeAll
	public void before() {
		ff4j.enable(FeatureId.INCREMENTAL_SCAN.getId());
	}
	
	@AfterAll
	public void after() {
		ff4j.disable(FeatureId.INCREMENTAL_SCAN.getId());
	}
	
	@Override
	protected String getTestFolder() {
		return testFolderName;
	}

	@Override
	protected Path getSourcePath() {
		switch (currentStep) {
			case FIRST_STEP:
				return testFolder;
			case SECOND_STEP:
				return testFolderUpdated;
		}
		throw new IllegalStateException("Unknown step: " + currentStep);
	}
	
	/**
	 * Prepares the source object with the given parameters
	 *
	 * @param projectId the project id
	 * @param path the file path
	 * @param content the file content
	 * @param moduleType {@link ModuleType}
	 * @return the source object created from the given parameters
	 */
	@Override
	protected SourcePojoPrototype createSourceObject(final EntityId projectId, final Path path, final String content, final ModuleType moduleType) {
		return new SourcePojoPrototype()
				.setProject(projectId)
				.setName(path.getFileName().toString())
				.setPath(Paths.get("temp").resolve(getSourcePath().relativize(path)).toString())
				.setTechnology(moduleType.getTechnology())
				.setType(moduleType.getType())
				.setMetaDataRevision(1l)
				.setContentRevision(1l)
				.setContent(new BinaryString(content));
	}
	
	@Override
	protected String getFileContent(final Path path) throws IOException {
		return new String(Files.readAllBytes(path), getCharset());
	}

	@Override
	protected final DiscoverMetricsJob createDiscoverMetricsJob(final  EntityId projectId) {
		return new DiscoverMetricsJob(projectId, true);
	}
	
	/**
	 * Tests discovery on a project, changing the source objects of the projects and running discovery again
	 *
	 * @param v0 folder with the files of the initial project
	 * @param v1 folder with the files of the project in its changes state
	 * @param firstRunDiscovery {@link Consumer} that is called after first discovery job finished
	 * @param secondRunDiscovery {@link Consumer} that is called after incremental discovery job finished
	 * @throws IOException in case of error during accessing files
	 */
	protected void doTest(final String v0, final String v1, 
						@Nullable final Consumer<EntityId> firstRunDiscovery,
						@Nullable final Consumer<EntityId> secondRunDiscovery) throws IOException {
		currentStep = Step.FIRST_STEP;
		sourceObjects.clear();
		testFolderName = TEST_FOLDER + "/" + v0;
		testFolder =  assertResource(TEST_SOURCE_FOLDER.resolve(v0));
		testFolderUpdated = assertResource(TEST_SOURCE_FOLDER.resolve(v1));
		sourceService.resetCaches();
		final EntityId projectId = createProject().identity();
		if (isWriteExpected()) {
			/* if isWriteExpected() returns true the updated test folder was used to create the expected output */
			currentStep = Step.SECOND_STEP;
			uploadResources(projectId);
			submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));

			submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false)); /* create test data */
			final Path expectedFile = getExpectedFile(getExpectedFileName());
			writeExpected(expectedFile, getMetricsContentAsCsv(projectId));
			return;
		}
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, createDiscoverMetricsJob(projectId));
		
		if (firstRunDiscovery != null) {
			firstRunDiscovery.accept(projectId);
		}

		updateProject(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, createDiscoverMetricsJob(projectId));
		
		if (secondRunDiscovery != null) {
			secondRunDiscovery.accept(projectId);
		}

		final Path expectedFile = getExpectedFile(getExpectedFileName());
		DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), getMetricsContentAsCsv(projectId));
	}
	
	@Override
	protected SourcePojo uploadSourceObject(final SourcePojoPrototype sourceObject) {
		final SourcePojo createdSourceObject = super.uploadSourceObject(sourceObject);
		/* substring(5) because a "temp/" has been added to the path */
		sourceObjects.put(sourceObject.path.getNonNull().substring(5), createdSourceObject);
		return createdSourceObject;
	}

	private void updateProject(final EntityId projectId) throws IOException {
		currentStep = Step.SECOND_STEP;
		final ProjectPojo project = assertNotNull(projectService.get(projectId));
		final ProjectPojoPrototype projectProtoType = new ProjectPojoPrototype();
		projectProtoType.setSourceCodeRevision(Long.valueOf(100));
		projectProtoType.setId(project.getId().toString());
		projectService.update(projectProtoType);
		final Map<Path,String> v0 = getFiles(new HashMap<>(), testFolder, testFolder);
		final Map<Path,String> v1 = getFiles(new HashMap<>(), testFolderUpdated, testFolderUpdated);
		for (final Entry<Path, String> file : v1.entrySet()) {
			final Path path = file.getKey();
			final String fileContent = v0.remove(path);
			if (fileContent != null && ! (fileContent.equals(file.getValue()))) {
				final SourcePojo sourceObject = sourceObjects.get(path.toString());
				/* Load latest version from DB */
				sourceService.update(new SourcePojoPrototype()
						.withId(sourceObject.identity())
						.setContent(new BinaryString(file.getValue())));
			} else if (fileContent == null) {
				final ModuleType moduleType = FileExtension.resolve(path.toString());
				sourceService.create(createSourceObject(projectId, testFolderUpdated.resolve(path), file.getValue(), moduleType));
			}
		}

		final List<SourcePojo> toBeDeletedSOs = v0.entrySet().stream()
			.map(file -> sourceObjects.get(file.getKey().toString()))
			.collect(Collectors.toList());
		assertFalse(toBeDeletedSOs.stream().anyMatch(so -> so == null), "All SourceObjects must not be null");
		sourceService.remove(toBeDeletedSOs.stream().map(SourcePojo::identity).collect(Collectors.toList()), null);
	}
	
	private Map<Path,String> getFiles(final Map<Path,String> files, final Path dir, final Path root) throws IOException {
		try(DirectoryStream<Path> stream = Files.newDirectoryStream(dir)) {
			for (final Path path : stream) {
				/* ignore hidden files (names starting with '.') */
				if (path.getNameCount() > 0 && ! path.getFileName().toString().startsWith(".")) {
					if (path.toFile().isDirectory()) {
						getFiles(files, path, root);
					} else {
						files.put(root.relativize(path), new String(Files.readAllBytes(path), getCharset()));
					}
				}
			}
		}
		return files;
	}
}
