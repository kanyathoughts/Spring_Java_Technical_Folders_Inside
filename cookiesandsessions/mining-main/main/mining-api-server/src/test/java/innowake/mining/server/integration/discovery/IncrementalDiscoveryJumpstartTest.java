/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.jdbc.UncategorizedSQLException;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.EnabledIf;

import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests incremental discovery on the jumpstart sources and compares the results with the non-incremental discovery.
 * <p>This test is disabled by default and gets executed during the maven build which sets the system property {@code innowake.integration.jumpstart.tests.run}
 * to '{@code true}'.</p>
 * <p>In <b>eclipse</b> you have to set the system property in the <b>launch configuration</b> so the test gets executed:</p>
 * <pre>
 * VM arguments: -Dinnowake.integration.jumpstart.tests.run=true
 * <pre>
 */
@WithMockUser
@EnabledIf("#{ 'true'.equals( systemProperties[ 'innowake.integration.jumpstart.tests.run' ] ) }")
@ActiveProfiles({"test", "profiling"})
class IncrementalDiscoveryJumpstartTest extends BaseDiscoveryTest {

	private static final Logger LOG = LoggerFactory.getLogger(IncrementalDiscoveryJumpstartTest.class.getSimpleName());
	private static final FeatureId[] FEATURES = { FeatureId.INCREMENTAL_SCAN };
	private static final String PRINT_LINE = "Collected source: %s";
	private static final String UNDISCOVERED_JAVA_FOLDER = "undiscovered-java";					  
	private static final double UPLOAD_CHANCE = 0.75;
	private static final double CHANGE_CHANCE = 0.5;
	/* Number of test executions. One run takes about 3-4 min. Higher value can result in job timeouts on Jenkins. */
	private static final int TEST_EXECUTIONS = 50;

	private static final String[] COPIES_TO_FIX = { "DPPD004", "DPWS004", "DPWS005", "DPWScpy", "TINVOIC", "BAS001B" };
	
	private static final List<String> FILES_WITH_SAME_DSN = Arrays.asList("MMRS712J", "MMRS712K", "MMRS712U", "MMRS711A");
	
	/* these are all technologies available in Discovery Jumpstart test project */
	private static final Map<Technology, String> TECHNOLOGY_TO_COMMENT = new HashMap<>();
	static {
		TECHNOLOGY_TO_COMMENT.put(Technology.ASSEMBLER, "*");
		TECHNOLOGY_TO_COMMENT.put(Technology.BASIC, "!");
		TECHNOLOGY_TO_COMMENT.put(Technology.COBOL, "      *");
		TECHNOLOGY_TO_COMMENT.put(Technology.CICS, "      *");
		/* comments for CSD are not supported yet */
		TECHNOLOGY_TO_COMMENT.put(Technology.CSD, " *");
		TECHNOLOGY_TO_COMMENT.put(Technology.EASYTRIEVE, "*");
		/* comments for IMS are not supported yet */
		TECHNOLOGY_TO_COMMENT.put(Technology.IMS, "*");
		TECHNOLOGY_TO_COMMENT.put(Technology.JAVA, "//");
		TECHNOLOGY_TO_COMMENT.put(Technology.JCL, "//*");
		TECHNOLOGY_TO_COMMENT.put(Technology.NATURAL, "**");
		TECHNOLOGY_TO_COMMENT.put(Technology.PL1, "/* */");
		TECHNOLOGY_TO_COMMENT.put(Technology.SQL, "--");
		TECHNOLOGY_TO_COMMENT.put(Technology.ORACLE, "--");
		TECHNOLOGY_TO_COMMENT.put(Technology.VMS, " !");
		TECHNOLOGY_TO_COMMENT.put(Technology.ECL, "*");
	}

	private final List<Supplier<String>> logs = new ArrayList<>(256);
	private final Map<String, SourcePojo> collectedSources = new HashMap<>();
	private final List<SourcePojo> changedSources = new ArrayList<>();
	
	private boolean fixResourceModules;

	@Autowired
	private AutowireCapableBeanFactory beanFactory;
	
	private int testRun;
	private Step step = Step.INCREMENTAL;
	private EntityId projectIdForIncrementalScan = EntityId.VOID;

	@Override
	public String getTestFolder() {
		return "discoveryJumpstart";
	}

	@Override
	protected void resetTestData() throws IOException {
		/* Disable reset after test runs: Workaround for com.orientechnologies.orient.core.exception.OStorageException
		 * Problem will be solved soon, when modules are migrated to Postgres */
		if (this.testRun == 0) {
		super.resetTestData();
	}
	}

	@Override
	protected String getProjectName() {
		return getTestFolder() + "_" + step.toString() + "_" + testRun;
	}

	@Override
	protected SourcePojoPrototype createSourceObject(final EntityId projectId, final Path path, final String content, final ModuleType moduleType) {
		final SourcePojoPrototype sourceObject = super.createSourceObject(projectId, path, content, moduleType);

		/* Remove the "undiscovered-java" folder to execute the discovery for Java modules like QEF does.
		 * This is also important for the Java source export which currently only considers the path but not the java packages */
		if (moduleType == ModuleType.JAVA_COMPILATION_UNIT) {
			sourceObject.setPath(Paths.get("temp").resolve(getSourcePath().resolve(UNDISCOVERED_JAVA_FOLDER).relativize(path)).toString());
		}

		return sourceObject;
	}

	/**
	 * <p>This test is disabled by default and gets executed only if the system property {@code innowake.integration.jumpstart.tests.run} is set
	 * to '{@code true}'.</p>
	 * <p>In <b>eclipse</b> you have to set the system property in the <b>launch configuration</b> so the test gets executed:</p>
	 * <pre>
	 * VM arguments: -Dinnowake.integration.jumpstart.tests.run=true
	 * <pre>
	 * @return collection of {@link DynamicTest DynamicTests}
	 */
	@TestFactory
	Collection<DynamicTest> test() {
		final String name = "Incremental discovery jumpstart test run: %d with seed1: %d, seed2: %d";
		final List<DynamicTest> tests = new ArrayList<>();
		for (int i = 1; i <= TEST_EXECUTIONS; i++) {
			/* Set these values accordingly to replay a (failed) test */
			/* final long seed1 = 1;
			final long seed2 = 1011;*/

			final long seed1 = i;
			final long seed2 = i + i * 10 + 1000;
			final String displayName = String.format(name, Integer.valueOf(i), Long.valueOf(seed1), Long.valueOf(seed2));
			final IncrementalDiscoveryJumpstartTest test = createTestInstance(displayName, i, seed1, seed2);
			beanFactory.autowireBean(test);
			tests.add(DynamicTest.dynamicTest(displayName, test));
		}

		return tests;
	}

	private IncrementalDiscoveryJumpstartTest createTestInstance(final String displayName, final int testRun, final long seed1, final long seed2) {
		return new IncrementalDiscoveryJumpstartTest() {

			@Override
			public void execute() {
				LOG.info(() -> "EXECUTE TEST " + displayName);
				internalExecute(testRun, seed1, seed2);
			}
		};
	}

	private void internalExecute(final int testRun, final long seed1, final long seed2) {
		reset();
		
		this.testRun = testRun;

		step = Step.INCREMENTAL;
		final String actual = capitalizeBasicCallName(incrementalClusteredDiscovery(seed1, seed2));
		step = Step.NONINCREMENTAL;
		final String expected = capitalizeBasicCallName(nonIncrementalDiscovery());

		if (LOG.isInfoEnabled()) {
			logSummary(seed1, seed2);
			LOG.info("### PROFILING METRICS ###");
			LOG.info(ProfilingFactory.getProfilingSession().getGlobalMetrics().toString());
		}

		DiscoveryExcelUtil.compareIgnoreOrder(expected, actual);
	}

	/*
	 * In Basic language, we use case-insensitive routine names to create dependencies with call statements.
	 * If there are call statements with the same name but different cases, we create a module and link it twice.
	 * Because dependency processing is highly parallel, we can't predict which case will be created first.
	 * Thus, we validate values irrespective of case sensitivity.
	 * For example, if different scans create dependencies with 'TAC02_NEW' and 'TAC02_New',
	 * both are correct and the test case should pass.
	 * Here is the test source
	 * ./test-resources/innowake/mining/server/discovery/spource/discoveryJumpstart/undiscovered-basic/ben_con_oux_01.bas
	 * 
	 */
	private String capitalizeBasicCallName(String actual) {
		return actual.replaceAll("TAC02_New", "TAC02_NEW");
	}

	private String nonIncrementalDiscovery() {
		final EntityId projectId = assertNotNull(createProject()).identity();
		logs.add(() -> String.format("Starting non-incremental discovery for project: %d", projectId.getNid()));
		for (String path : collectedSources.keySet()) {
			logs.add(() -> "  Uploading sourceObject: " + path);
		}

		/* 8) Disable features under test  */
		Arrays.stream(FEATURES).forEach(feature -> ff4j.disable(feature.getId()));

		/* 9) Upload all sources, which the incremental incremental discovery collected */
		uploadResources(projectId, path -> Files.isRegularFile(path) && collectedSources.containsKey(path.toString()));

		/* 10) Execute non-incremental Discover Code */
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		fixCopies(projectId);

		/* 11) Change sources and execute non-incremental Discover Metrics */
		changeSourceObjectContents(projectId);
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId,false));
		
		/* Here, we are updating module type to RESOURCE_VSAM_FILE to fix the test case failure.*/
		if (fixResourceModules) {
			LOG.info(() -> String
					.format("Second Incremental scan don't have either job or program so, we are updating the respective modules as RESOURCE_VSAM"));
			updateResourceType(projectId, Technology.RESOURCE, Type.FILE, Type.VSAM_FILE);
			fixResourceModules = false;
		}

		/* 12) Return expected workbook */
		return getMetricsContentAsCsv(projectId);
	}

	private String incrementalClusteredDiscovery(final long seed1, final long seed2) {
		final EntityId projectId = assertNotNull(createProject()).identity();
		projectIdForIncrementalScan = projectId;
		logs.add(() -> String.format("Starting incremental discovery for project: %d and seed: %d", projectId.getNid(), Long.valueOf(seed1)));

		/* 1) Enable features under test */
		Arrays.stream(FEATURES).forEach(feature -> ff4j.enable(feature.getId()));

		/* 2) Upload all sources with seed1 */
		addOrDeleteSourceObjects(projectId, new Random(seed1));

		
		/* 3) Execute incremental Discover Code & Discover Metrics */
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		fixCopies(projectId);
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, true));
		
		/* We will identify target module with type RESOURCE_VSAM_FILE based on a set of conditions in WMIN-7595 with the source files 
		 * MMRS7111 and MMRS711J. But we have similar kind of files with same targets as DSN in a set of jobs like in FILES_WITH_SAME_DSN.
		 * There are some invalid test cases occur with below scenario:
		 * #1 If we have MMRS7111, MMRS711J and at least one file from FILES_WITH_SAME_DSN in first incremental scan. 
		 * #2 If MMRS7111 or MMRS711J removed/deleted in second scan,
		 * The difference in the type of the modules with RESOURCE_FILE in normal scan 
		 * and RESOURCE_VSAM_FILE in incremental scan. We need to fix this in test case it self.*/
		
		final String pathDirectory = ".\\src\\test\\resources\\innowake\\mining\\server\\discovery\\source\\discoveryJumpstart\\undiscovered\\";
		final String program = String.format("%s%s", pathDirectory, "MMRS7111");
		final String job = String.format("%s%s", pathDirectory, "MMRS711J");
		
		/* Here we are checking #1 condition.*/
		final boolean resourceModulesToFixExist = isResourceModulesToFixExist(pathDirectory, program, job);
		
		if (resourceModulesToFixExist) {
			LOG.info(() -> String.format("First Incremental Scan has program %s, job %s and one or more jobs like %s to have RESOURCE_VSAM Modules.",
					program, job, String.join(",", FILES_WITH_SAME_DSN)));
		}
		
		/* 4) Again: Upload all sources with seed2 */
		logs.add(() -> String.format("Starting with incremental incremental discovery and seed: %d", Long.valueOf(seed2)));
		final Random random2 = new Random(seed2);
		addOrDeleteSourceObjects(projectId, random2);
		
		/* 5) Execute again incremental Discover Code */
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		fixCopies(projectId);

		/* 6) Change sources with seed2 and execute again incremental Discover Metrics */
		changeSourceObjectContents(random2, projectId);
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, true));
		
		final Set<String> filesInSecondScan = collectedSources.keySet();
		/* Here we are checking #2 condition */
		final boolean isProgramAndJobExist = filesInSecondScan.contains(program) && filesInSecondScan.contains(job);
		/* Here we are checking #1, #2 conditions together, based on which we update the type of resource modules. */
		fixResourceModules = resourceModulesToFixExist && ! isProgramAndJobExist;

		/* 7) Return actual workbook */
		return getMetricsContentAsCsv(projectId);
	}
	
	private boolean isResourceModulesToFixExist(final String pathDirectory, final String program, final String job) {
		final Set<String> filesInFirstScan = collectedSources.keySet().stream().collect(Collectors.toSet());
		final List<String> resourceFilesToCheck = FILES_WITH_SAME_DSN.stream()
														.map(file -> String.format("%s%s", pathDirectory, file))
														.collect(Collectors.toList());
		
		return filesInFirstScan.containsAll(Arrays.asList(program, job)) && filesInFirstScan.retainAll(resourceFilesToCheck);
	}

	private void fixCopies(final EntityId projectId) {
		sourceService.resetCaches();
		Arrays.stream(COPIES_TO_FIX)
				.flatMap(cpy -> sourceService.find(q -> q.ofProject(projectId).withName(cpy)).stream())
				.filter(sourceObject -> sourceObject.getType() != Type.COPYBOOK)
				.forEach(sourceObject -> {
					/*
					 * For non-incremental discovery we also have to switch the paths of the copybooks to the path DiscoverCode would
					 * set it when it is able to categorize them correctly:
					 * 
					 * 1)	src/maybe/sql/discoveryJumpstart/undiscovered-cobol/source2/DPPD004.sql	->
					 * 		src/cobol/discoveryJumpstart/undiscovered-cobol/source2/copies/DPPD004.cpy
					 * 
					 * 2)	src/maybe/sql/discoveryJumpstart/undiscovered-cobol/source/DPPD004.sql	->
					 * 		src/cobol/discoveryJumpstart/undiscovered-cobol/source/copies/DPPD004.cpy
					 * 
					 * 3)	src/maybe/pl1/discoveryJumpstart/undiscovered-pl1/programs/BAS001B.pl1	->
					 * 		src/pl1/discoveryJumpstart/undiscovered-pl1/copies/BAS001B.pcpy
					 */
					final String path = sourceObject.getPath();
					final SourcePojoPrototype changedSource = new SourcePojoPrototype().withId(sourceObject.identity());
					String newPath = null;
					if (path.contains("undiscovered-cobol")) {
						newPath = "src/cobol/discoveryJumpstart/" + path.substring(path.indexOf("undiscovered-cobol"));
						final String directory = newPath.substring(0, newPath.lastIndexOf('/')) + "/copies/";
						newPath = directory + com.google.common.io.Files.getNameWithoutExtension(newPath.substring(newPath.lastIndexOf('/'))) + ".cpy";
						changedSource.setPath(newPath);
						changedSource.setTechnology(Technology.COBOL);
					} else if (path.contains("undiscovered-pl1")) {
						newPath = "src/pl1/discoveryJumpstart/undiscovered-pl1/copies/";
						newPath = newPath + com.google.common.io.Files.getNameWithoutExtension(path.substring(path.lastIndexOf('/'))) + ".pcpy";
						changedSource.setPath(newPath);
						changedSource.setTechnology(Technology.PL1);
					}
					
					changedSource.setType(Type.COPYBOOK);
					sourceService.update(changedSource);
				});
	}

	private void addOrDeleteSourceObjects(final EntityId projectId, final Random random) {
		try (final Stream<Path> walk = Files.walk(getSourcePath())) {
			walk.filter(path -> Files.isRegularFile(path))
				.forEach(path -> {
					final String content;
					try {
						content = getFileContent(path);
					} catch (final IOException exception) {
						LOG.error(() -> "Error while updating resources: " + exception.getMessage(), exception);
						throw new IllegalStateException(exception);
					}

					final ModuleType moduleType  = FileExtension.resolve(path.toString());
					final SourcePojoPrototype sourceObject = createSourceObject(projectId, path, content, moduleType);
					addOrDeleteSourceObject(sourceObject, random, path.toString());
				});

			importConfiguration(projectId);
		} catch (final Exception exception) {
			LOG.error(() -> "Error while updating resources: " + exception.getMessage(), exception);
			fail("Error while updating resources: " + exception.getMessage(), exception);
		}
	}

	private void addOrDeleteSourceObject(final SourcePojoPrototype source, final Random random, final String originalPath) {
		if (random.nextDouble() <= UPLOAD_CHANCE) {
			if (collectedSources.containsKey(originalPath)) {
				logs.add(() -> String.format("  Upload of SourcePojo not required: %s. Path: %s", source.name.get(), originalPath));
			} else {
				logs.add(() -> String.format("  Uploading SourcePojo: %s. Original path: %s. Adjusted path: %s", 
						source.name.get(), originalPath, source.path.get()));
				collectedSources.put(originalPath, uploadSourceObject(source));
			}
		} else if (collectedSources.containsKey(originalPath)) {
			logs.add(() -> String.format("  Deleting SourcePojo: %s. Original path: %s. Adjusted path: %s", source.name.get(), originalPath, source.path.get()));
			sourceService.remove(Assert.assertNotNull(collectedSources).remove(originalPath).identity(), null);
		} else {
			logs.add(() -> String.format("  Deletion of SourcePojo not required: %s Path: %s", source.name.get(), originalPath));
		}
	}

	/* Method must be called after Discover Code to ensure correct technology. */
	private void changeSourceObjectContents(final Random random, final EntityId projectId) {
		final Long projectMetricsBaseRevision = projectService.get(projectId).getMetricsBaseRevision(); 
		final long metricsBaseRevision = projectMetricsBaseRevision == null ? 0 : projectMetricsBaseRevision.longValue();
	
		collectedSources.values().forEach(sourceObject -> {
			if (random.nextDouble() <= CHANGE_CHANCE) {
				/* reread source object because the path was updated by Discover Code */
				final SourcePojo sourceObjectUpdated1 = sourceService.get(sourceObject.getUid());
				/* source is ignored for content change as it leads to mismatch due to race condition */
				if ( ! sourceObjectUpdated1.getName().equals("DPPD004")) {
					logs.add(() -> String.format("  Changing SourcePojo content: %s. Adjusted path: %s", sourceObjectUpdated1.getName(),
							sourceObjectUpdated1.getPath()));
					final SourcePojo sourceObjectUpdated2 = sourceService.get(sourceService.update(changeSourceObject(sourceObjectUpdated1)));
					/*
					 * sourceObjectDao.update() sets contentRevision and metaDataRevision to the project's sourceCodeRevision. However revision has to be
					 * greater than the project revision so that it's fetched as changed SourcePojo for the incremental discovery
					 */
					if ( ! (sourceObjectUpdated2.getContentRevision() > metricsBaseRevision
							|| sourceObjectUpdated2.getMetaDataRevision() > metricsBaseRevision)) {
						sourceService.update(new SourcePojoPrototype().withId(sourceObjectUpdated2.identity()).setContentRevision(metricsBaseRevision + 1));
					}
					changedSources.add(sourceObjectUpdated2);
				}
			}
		});
	}
	
	/* Method must be called after Discover Code to ensure correct technology. */
	private void changeSourceObjectContents(final EntityId projectId) {
		changedSources.forEach(sourceObject -> {
			/* reread source object to ensure it is up-to-date */
			final SourcePojo sourceObjectUpdated = sourceService.get(q -> q.ofProject(projectId).withPath(sourceObject.getPath()));
			sourceService.put(sourceObjectUpdated.getUid(), null, sourceObject.getContent());
		});
	}

	private void logSummary(final long seed1, final long seed2) {
		LOG.info("------------------------------------------------------------");
		LOG.info("------------------------ Test case -------------------------");
		LOG.info("------------------------------------------------------------");
		LOG.info(" ");
		LOG.info("Seed1: " + seed1);
		LOG.info("Seed2: " + seed2);
		LOG.info("Can be set manually in IncrementalDiscoveryJumpstartTest.internalExecute() to replay a (failing) test.");
		LOG.info(" ");		
		LOG.info("------------------------------------------------------------");
		LOG.info("------------------ Updated source objects ------------------");
		LOG.info("------------------------------------------------------------");
		LOG.info(" ");
		logs.forEach(LOG::info);
		LOG.info(" ");
		LOG.info("------------------------------------------------------------");
		LOG.info("----------------- Collected source objects -----------------");
		LOG.info("------------------------------------------------------------");
		LOG.info(" ");
		collectedSources.keySet().forEach(path -> LOG.info(String.format(PRINT_LINE, forLogging(path))));
	}

	private String forLogging(final String path) {
		final int index = path.indexOf(getTestFolder());
		return index == -1 ? path : path.substring(index + getTestFolder().length() + 1);
	}
	
	private void reset() {
		sourceService.resetCaches();
		collectedSources.clear();
		changedSources.clear();
		logs.clear();
	}
	
	private static SourcePojoPrototype changeSourceObject(final SourcePojo source) {
		final SourcePojoPrototype changedSource = new SourcePojoPrototype();
		final Technology technology = source.getTechnology();
		if (TECHNOLOGY_TO_COMMENT.containsKey(technology)) {
			final String comment = TECHNOLOGY_TO_COMMENT.get(technology);
			changedSource.setContent(new BinaryString(source.getContent().toString() + "\n" + comment));
		} else if ( ! technology.equals(Technology.NONE)) {
			fail("Technology is not supported yet: " + technology);
		}
		changedSource.setTechnology(technology);
		changedSource.setType(source.getType());
		changedSource.setUid(source.getUid());
		return changedSource;
	}
	
	/* This method will be used to update a specific type of the module from one type to another type during normal scan based on some conditions. */
	private void updateResourceType(final EntityId projectId, final Technology technology, final Type fromType, final Type toType) {
		try {
			final long recordsUpdated = moduleService.updateModules(q1 -> q1.ofProject(projectId).withType(fromType).withNames(moduleService
					.findModulesLightweight(q2 -> q2.ofProject(projectIdForIncrementalScan).withType(toType))
						.stream().map(ModuleLightweightPojo::getName).collect(Collectors.toList())),
				new ModulePojoPrototype().setTechnology(technology).setType(toType).setOrigin(Origin.CUSTOM));
			LOG.info(String.format("%d Modules types got updated to %s/%s", recordsUpdated, technology.name(), toType.name()));
		} catch (final UncategorizedSQLException e) {
			throw new ConstraintViolationException(Module.class, ExceptionUtils.getRootCauseMessage(e), e);
		}
	}

	private enum Step {
		INCREMENTAL,
		NONINCREMENTAL;
	}
}