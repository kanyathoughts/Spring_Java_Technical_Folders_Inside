/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.access.ModelArtifactService;
import innowake.mining.data.io.DiscoveryCsvExportService;
import innowake.mining.data.io.DiscoveryExportOptions;
import innowake.mining.data.io.discovery.config.DiscoveryConfigurationImportService;
import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.integration.discovery.DiscoveryFeatureValidator.Message;
import innowake.mining.server.integration.job.JobUtil;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.Type;
import innowake.mining.tags.DiscoveryTest;
import org.apache.poi.EncryptedDocumentException;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.ff4j.FF4j;
import org.junit.jupiter.api.function.Executable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import software.amazon.awssdk.utils.StringUtils;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static java.nio.file.Files.exists;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Performs Discover code and metrics tests.
 * This class is adapted from code-discovery-test project.
 */
@DiscoveryTest
public abstract class BaseDiscoveryTest extends DatabaseResettingTest implements Executable {

	private static final boolean RESET_DATA = Boolean.getBoolean("innowake.base.discovery.force.reset.data");
	private static final boolean WRITE_EXPECTED = Boolean.getBoolean("innowake.base.discovery.write.expected");
	private static final Charset WORKSPACE_CHARSET = Charset.forName("cp1252");
	private static final String DUMP_FILE = "discovery.xlsx.dump";
	private static final String EFFORT_DUMP_FILE = "effort-summary.xlsx.dump";
	protected static final String DISCOVERY_CONFIG_FOLDER = "config";
	private static final Path BASE_FOLDER = Paths.get("./test-resources/innowake/mining/server/discovery");
	protected static final Path EXPECTED_FOLDER = BASE_FOLDER.resolve("expected");
	private static final Long ONE = Long.valueOf(1);

	protected static final String TEMP_SNAPSHOT = "snapshot";
	protected static final Path SOURCE_FOLDER = BASE_FOLDER.resolve("source");
	private final boolean skipDiscoveryFeatureValidation;

	@Autowired
	private DiscoveryConfigurationImportService configurationImportService;

	@Autowired
	protected SourceCachingService sourceService;

	@Autowired
	protected ApplicationEventPublisher eventPublisher;
	
	/* Do not increase the visibility. Call getMetricsContentAsCsv() or getEffortContentAsCsv() instead! */
	@Autowired
	private DiscoveryCsvExportService csvExportService;

	@Autowired
	protected JobManager jobManager;

	@Autowired
	protected Tracer tracer;

	@Nullable
	protected EntityId projectId;
	private final BiConsumer<EntityId, AssertionValues> postAssertion;

	@Autowired
	protected FF4j ff4j;

	@Autowired
	protected ModuleService moduleService;

	@Autowired
	protected ModelArtifactService modelArtifactService;

	/**
	 * Submits the provided {@code job} for execution by delegating to {@linkplain JobUtil#submitJob}.
	 * Please directly use {@linkplain JobUtil#submitJob} instead of this.
	 *
	 * @param jobManager the {@link JobManager}
	 * @param tracer the {@link Tracer}
	 * @param job the {@link Job} to submit
	 *
	 * @return the jobId
	 */
	public static String submitJob(final JobManager jobManager, final Tracer tracer, final Job<?> job) {
		return JobUtil.submitJob(jobManager, tracer, job);
	}

	public BaseDiscoveryTest() {
		this(false);
	}

	/**
	 * Constructor to set the Thread context for job logs.
	 *
	 * @param skipDiscoveryFeatureValidation setting the flag will skip the discovery feature validation
	 */
	public BaseDiscoveryTest(final boolean skipDiscoveryFeatureValidation) {
		/* sets the default assertions */
		this((final EntityId projectId, final AssertionValues assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent);
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedEffortContent, assertionValues.actualEffortContent);
		}, skipDiscoveryFeatureValidation);
	}

	/**
	 * Constructor to set the Thread context for job logs.
	 *
	 * @param postAssertion the assertions to be done after the discovery is executed
	 * @param skipDiscoveryFeatureValidation setting the flag will skip the discovery feature validation
	 */
	public BaseDiscoveryTest(final BiConsumer<EntityId, AssertionValues> postAssertion, final boolean skipDiscoveryFeatureValidation) {
		this.postAssertion = postAssertion;
		this.skipDiscoveryFeatureValidation = skipDiscoveryFeatureValidation;
	}

	/**
	 * Get the test folder for the discovery to execute.
	 *
	 * @return base folder string.
	 */
	protected abstract String getTestFolder();

	protected String getProjectName() {
		return getTestFolder();
	}

	protected DiscoverMetricsJob createDiscoverMetricsJob(final EntityId projectId) {
		return new DiscoverMetricsJob(projectId, false);
	}

	/**
	 * This method calls and performs:
	 * 1.) Creates a new Project for each run.
	 * 2.) Uploads Source code & Configurations.
	 * 3.) Perform Discover Code
	 * 4.) Perform Discover Metrics
	 * 5.) Exports the snapshot into Excel sheet.
	 * 6.) Asserts with the expected file.
	 */
	@Override
	public void execute() throws EncryptedDocumentException, InvalidFormatException, IOException {
		try {
			if (RESET_DATA) {
				resetData();
			} else {
				sourceService.resetCaches();
			}

			final EntityId projectId = createProject().identity();
			this.projectId = projectId;
			
			final Path expectedFile = getExpectedFile(getExpectedFileName());
			final Path expectedEffortFile = getExpectedFile(getExpectedEffortFileName());

			final Long metricsBaseRevisionBeforeUpload = assertNotNull(projectService.get(projectId).getMetricsBaseRevision());
			uploadResources(projectId);
			submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));

			final Long metricsBaseRevisionAfterDiscovery = assertNotNull(projectService.get(projectId).getMetricsBaseRevision());
			assertEquals(metricsBaseRevisionBeforeUpload, metricsBaseRevisionAfterDiscovery,
					"Project.MetricsBaseRevision should not change after DiscoverCode!");

			submitJob(jobManager, tracer, createDiscoverMetricsJob(projectId));

			final Long metricsBaseRevisionAfterMetrics = assertNotNull(projectService.get(projectId).getMetricsBaseRevision());
			assertTrue("Project.MetricsBaseRevision should increase after DiscoverMetrics!",
					metricsBaseRevisionAfterDiscovery.longValue() < metricsBaseRevisionAfterMetrics.longValue());

			final String actualMetricsContent = getMetricsContentAsCsv(projectId);
			final String actualEffortContent = getEffortContentAsCsv(projectId);
			if (isWriteExpected()) {
				writeExpected(expectedFile, actualMetricsContent);
				writeExpected(expectedEffortFile, actualEffortContent);
			} else {
				testModuleUtilityIsNotPresentInProject0(projectId);
				postAssertion.accept(projectId, new AssertionValues(read(expectedFile), actualMetricsContent, read(expectedEffortFile), actualEffortContent));
				if ( ! skipDiscoveryFeatureValidation) {
					validateModuleProperties(projectId);
				}
			}
		} catch (final Exception e) {
			/* Trying to get a proper stacktrace in case the seemingly random ODatabaseException occurs in the build environment */
			e.printStackTrace();
			throw e;
		}
	}

	private void validateModuleProperties(final EntityId projectId) {
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
		final String message = modules.stream().map(module -> new DiscoveryFeatureValidator(module, moduleService))
				.flatMap(validator -> validator.validate().stream()).map(Message::value).collect(Collectors.joining("\n"));
		if (StringUtils.isNotBlank(message)) {
			fail("Feature validation failure.\n" + message);
		}
	}

	protected String getMetricsContentAsCsv(final EntityId projectId) {
		try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			csvExportService.exportCsv(projectId, out, new DiscoveryExportOptions());
			return out.toString(getCharset());
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

	protected String getEffortContentAsCsv(final EntityId projectId) {
		try (final ByteArrayOutputStream out = new ByteArrayOutputStream()) {
			csvExportService.exportEffortSummaryCsv(projectId, out);
			return out.toString(getCharset());
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

	/**
	 * Returns the {@link Charset} for reading and writing test resources.
	 *
	 * @return {@link Charset} for reading and writing test resources
	 */
	protected Charset getCharset() {
		return WORKSPACE_CHARSET;
	}

	/**
	 * If the expected files of the discover code and metrics tests need to be overwritten return {@code true} or set the system property accordingly.
	 *
	 * @return Flag indicating whether to write the expected file.
	 */
	public static boolean isWriteExpected() {
		return WRITE_EXPECTED;
	}

	protected Path getSourcePath() {
		return assertResource(SOURCE_FOLDER.resolve(getTestFolder()));
	}

	protected Path getExpectedFile(final String fileName) {
		final Path expectedFile = EXPECTED_FOLDER.resolve(getTestFolder()).resolve(fileName);
		if ( ! isWriteExpected() && Files.notExists(expectedFile)) {
			fail("Expected file does not exist : " + expectedFile.toString());
		}
		return expectedFile;
	}

	protected String getExpectedFileName() {
		return DUMP_FILE;
	}

	protected String getExpectedEffortFileName() {
		return EFFORT_DUMP_FILE;
	}

	protected Path getConfigFolder() {
		return EXPECTED_FOLDER.resolve(getTestFolder()).resolve(DISCOVERY_CONFIG_FOLDER);
	}

	protected void uploadResources(final EntityId projectId) {
		uploadResources(projectId, Files::isRegularFile);
	}

	protected void uploadResources(final EntityId projectId, final Predicate<Path> pathFilter) {
		try (final Stream<Path> walk = Files.walk(getSourcePath())) {
			walk.filter(pathFilter)
			.map(path -> {
				final String content;
				try {
					content = getFileContent(path);
				} catch (final IOException e) {
					throw new IllegalStateException(e);
				}
				final ModuleType moduleType = FileExtension.resolve(path.toString());
				return createSourceObject(projectId, path, content, moduleType);
			}).forEach(this::uploadSourceObject);

			importConfiguration(projectId);
		} catch (final Exception e) {
			e.printStackTrace();
			fail("Error while uploading resource: " + e.getMessage(), e);
		}
	}

	protected String getFileContent(final Path path) throws IOException {
		final String content = new String(Files.readAllBytes(path), StandardCharsets.UTF_8);
		return normalizeFileContent() ? content.replace("\r\n", "\n") : content;
	}

	protected boolean normalizeFileContent() {
		return false;
	}
	
	protected SourcePojo uploadSourceObject(final SourcePojoPrototype sourceObject) {
		return sourceService.get(sourceService.create(sourceObject));
	}

	protected SourcePojoPrototype createSourceObject(final EntityId projectId, final Path path, final String content, final ModuleType moduleType) {
		return new SourcePojoPrototype()
			.setProject(projectId)
			.setName(path.getFileName().toString())
			.setPath(Paths.get("temp").resolve(getSourcePath().getParent().relativize(path)).toString())
			.setTechnology(moduleType.getTechnology())
			.setType(moduleType.getType())
			.setMetaDataRevision(Long.valueOf(1))
			.setContentRevision(Long.valueOf(1))
			.setContent(new BinaryString(content));
	}

	protected void importConfiguration(final EntityId projectId) {
		final Map<String, String> configs = new HashMap<>(64);
		/* Load default configuration */
		configs.putAll(getConfiguration(EXPECTED_FOLDER.resolve(DISCOVERY_CONFIG_FOLDER)));
		/* Load Custom configuration */
		configs.putAll(getConfiguration(getConfigFolder()));
		try {
			configurationImportService.importConfigurations(projectId, configs);
		} catch (final Exception e) {
			e.printStackTrace();
			fail("Error while importing configuration: " + e.getMessage(), e);
		}
	}

	private static Map<String, String> getConfiguration(final Path configPath) {
		if (Files.exists(configPath)) {
			try (final Stream<Path> configFiles = Files.walk(configPath)) {
				final Map<String, String> configs = configFiles.filter(Files::isRegularFile).collect(Collectors.toMap(p -> p.getFileName().toString(), p -> {
					try {
						return new String(Files.readAllBytes(p));
					} catch (final IOException e) {
						throw new RuntimeException();
					}
				}));
				return configs;
			} catch (final Exception e) {
				e.printStackTrace();
				fail("Error while reading configuration: " + e.getMessage(), e);
			}
		}
		return Collections.emptyMap();
	}

	protected Path assertResource(final Path resource) {
		assertTrue(String.format("File/Folder %s missing", resource), exists(resource));
		return resource;
	}

	protected String read(final Path path) throws IOException {
		return new String(Files.readAllBytes(path), getCharset());
	}

	protected void writeExpected(final Path dump, final String actual) throws IOException {
		Files.createDirectories(dump.getParent());
		Files.write(dump, actual.getBytes(getCharset()));
	}

	protected ProjectPojo createProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName(getProjectName())
				.setClient(EntityId.of(ONE))
				.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY))));
	}

	private void testModuleUtilityIsNotPresentInProject0(final EntityId projectId) {
		/* checks whether the utility modules are not created when it is already present in project 0 */
		final Set<String> sysProjectUtilities = moduleService.findModulesLightweight(q -> q.ofProject(EntityId.of(0l)).withType(Type.UTILITY)).stream()
																.map(m -> m.getName())
																.collect(Collectors.toSet());
		final List<String> matchingModules = moduleService.findModulesLightweight(q -> q.ofProject(projectId).withType(Type.UTILITY)).stream()
															.map(module -> module.getName())
															.filter(name -> sysProjectUtilities.contains(name))
															.collect(Collectors.toList());
		if ( ! matchingModules.isEmpty()) {
			fail("The following utility module(s) is/are already present in Project 0 which should be referenced instead of creating such utility module(s) "
					+ "in the project "	+ projectId + ". Module name(s): " + String.join(",", matchingModules));
		}
	}

	static class AssertionValues {

		final String expectedMetricsContent;
		final String actualMetricsContent;
		final String expectedEffortContent;
		final String actualEffortContent;

		AssertionValues(final String expectedMetricsContent, final String actualMetricsContent, final String expectedEffortContent,
				final String actualEffortContent) {
			this.expectedMetricsContent = expectedMetricsContent;
			this.actualMetricsContent = actualMetricsContent;
			this.expectedEffortContent = expectedEffortContent;
			this.actualEffortContent = actualEffortContent;
		}
	}
}
