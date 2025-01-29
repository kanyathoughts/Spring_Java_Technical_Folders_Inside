/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;
import static innowake.mining.server.JobTestHelper.waitForJobCompletion;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Collections;
import java.util.Optional;
import java.util.Map;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.mining.data.core.SchemaConstants;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.CandidateIdentificationController;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.job.deadcode.IdentifyDeadCodeJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.hashing.CityHash;

/**
 * Tests for dead code calculation using {@link IdentifyDeadCodeJob}
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@WithMockUser
class IdentifyDeadCodeJobTest extends BaseDiscoveryTest {

	final String moduleMatcher = "{\"ids\":[\"%s\"],\"pathPatterns\":[\"%s\"]}";

	@Autowired
	private MockMvc mvc;

	@Autowired
	private AnnotationService annotationService;

	protected static final Long PROJECT_ID = 1L;

	private static final String BASE_FOLDER = "./test-resources/innowake/mining/server/discovery/source/dead-code/";

	private static final String DEAD_CODE_ANNOTATION_NAME = "Dead Code Candidate [System Identified]";

	@Nullable
	private ModulePojo test1aModule;
	@Nullable
	private ModulePojo test1bModule;
	@Nullable
	private ModulePojo test2Module;
	@Nullable
	private ModulePojo test3Module;
	@Nullable
	private ModulePojo cc2Module;	
	
	/* Test setup */
	@BeforeEach
	void createTestProject() {
		sourceService.resetCaches();
		createTestModules();
	}

	/**
	 * Tests the dead code calculation through {@link IdentifyDeadCodeJob}.
	 */
	@Test
	void testDeadCodeLineCountCalculation() {
		projectId = assertNotNull(createProject()).identity();
		final EntityId nonNullProjectId = assertNotNull(projectId);
		
		sourceService.resetCaches();
		uploadResources(assertNotNull(projectId));
		submitJob(jobManager, tracer, new DiscoverCodeJob(nonNullProjectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(nonNullProjectId, false));
		
		/* Initial dead code count is 5 */
		final ModulePojo module = assertDeadCodeCount(nonNullProjectId, 5);
		
		resetLinesOfDeadCode(module.identity());
		
		final String testModuleMatcher = String.format(moduleMatcher, module.getId(), StringUtils.EMPTY);
		submitDeadCodeJob(nonNullProjectId.getNid(), testModuleMatcher);
		
		/* Final dead code count is 5 after calculation through job */
		assertDeadCodeCount(nonNullProjectId, 5);
	}
	
	@Test
	void testDeadCodeAnnotationsCobol() {
		final EntityId moduleId = assertNotNull(test1aModule).identity();
		final List<AnnotationPojo> expectedAnnotations = List.of(
				annotationService.findAny(b -> b.byId(createAnnotation(moduleId, 227, 29))).orElseThrow(),
				annotationService.findAny(b -> b.byId(createAnnotation(moduleId, 265, 42))).orElseThrow());
		
		submitAndAssert(EntityId.of(PROJECT_ID), moduleId, 5, expectedAnnotations);
		
		final List<ModuleRelationshipPojo> relationships = moduleService.findRelationship(q -> q.ofSource(moduleId));
		assertEquals(2, relationships.size());
		final List<ModuleRelationshipPojo> deadRelationships = new ArrayList<>();
		for (final var relationship: relationships) {
			final Map<String, Object> properties = relationship.getProperties().orElse(Collections.emptyMap());
			final var fromDeadCode = properties.get("dead_code");
			if (fromDeadCode instanceof Boolean && ((Boolean) fromDeadCode).booleanValue()) {
				deadRelationships.add(relationship);
			}
		}
		assertEquals(1, deadRelationships.size());
	}

	@Test
	void testDeadCodeAnnotationsCobolAssembled() {
		final EntityId moduleId = assertNotNull(test2Module).identity();
		final List<AnnotationPojo> expectedAnnotations = List.of(
				annotationService.findAny(b -> b.byId(createAnnotation(moduleId, 274, 99))).orElseThrow(),
				annotationService.findAny(b -> b.byId(createAnnotation(moduleId, 382, 29))).orElseThrow(),
				annotationService.findAny(b -> b.byId(createAnnotation(moduleId, 476, 47))).orElseThrow());

		submitAndAssert(EntityId.of(PROJECT_ID), moduleId, 10, expectedAnnotations);
	}

	@Test
	void testDeadCodeAnnotationsCobol1Line() {
		final EntityId moduleId = assertNotNull(test3Module).identity();
		final List<AnnotationPojo> expectedAnnotations = Collections.emptyList();
		submitAndAssert(EntityId.of(PROJECT_ID), moduleId, 3, expectedAnnotations);
	}

	private void submitDeadCodeJob(final Long nonNullProjectId, final String testModuleMatcher) {
		final MvcResult mvcResult;
		try {
			mvcResult = mvc.perform(post("/api" + CandidateIdentificationController.DEAD_CODE_IDENTIFICATION_URL, nonNullProjectId)
					.contentType("application/json")
					.content(testModuleMatcher))
					.andExpect(status().isAccepted())
					.andReturn();
			final String testJobId = mvcResult.getResponse().getContentAsString().replaceAll("\"", "").trim();
			waitForJobCompletion(testJobId, jobManager, 1, TimeUnit.MINUTES);
		} catch (final Exception e) {
			fail("Fail to calculate dead code . Exception occured : " + e.getMessage());
		}
	}

	private ModulePojo assertDeadCodeCount(final EntityId nonNullProjectId, final int expectedDeadCodeCount) {
		final ModulePojo testModule = getModule(nonNullProjectId);
		final Optional<SourceMetricsPojo> sourceMetrics = assertNotNull(testModule.getSourceMetrics());
		assertEquals(expectedDeadCodeCount, sourceMetrics.orElseThrow().getDeadCodeLines());
		return testModule;
	}

	private ModulePojo getModule(final EntityId nonNullProjectId) {
		final Optional<ModulePojo> module = moduleService.findAnyModule(q -> q.ofProject(nonNullProjectId).withPath("src/cobol/WMIN4554/programs/DEAD10.cbl"));
		return module.get();
	}
	
	/* Create all used modules in here */
	private void createTestModules() {
		test1aModule = moduleService.findAnyModule(q -> q.byId(createModule("DEAD1.cbl", Type.PROGRAM))).orElseThrow();
		test1bModule = moduleService.findAnyModule(q -> q.byId(createModule("DEAD1.cpy", Type.COPYBOOK))).orElseThrow();
		makeIncludesReference(assertNotNull(test1aModule).getId(), assertNotNull(test1bModule).getId(), new ModuleLocation(210, 10));
		test2Module = moduleService.findAnyModule(q -> q.byId(createModule("DEAD2.cbl", Type.PROGRAM))).orElseThrow();
		test3Module = moduleService.findAnyModule(q -> q.byId(createModule("DEAD3.cbl", Type.PROGRAM))).orElseThrow();
		cc2Module = moduleService.findAnyModule(q -> q.byId(createModule("CC2.cpy", Type.COPYBOOK))).orElseThrow();
		makeIncludesReference(assertNotNull(test1aModule).getId(), assertNotNull(cc2Module).getId(), new ModuleLocation(301, 6));
	}
	
	private EntityId createModule(final String path, final Type type) {
		final String moduleName = path.split("\\.")[0];
		String content = "";
		try {
			content = new String(Files.readAllBytes(Paths.get(BASE_FOLDER).resolve(path)), StandardCharsets.UTF_8)
					.replaceAll("\\r\\n", "\n")
					.replaceAll("\\r", "\n");
		} catch (final IOException e) {
			Assertions.fail("Could not read file.");
		}
		final String contentHash = CityHash.cityHash128Hex(content);
		final String completePath = BASE_FOLDER + path;
		final SourcePojoPrototype sourcePrototype =
				new SourcePojoPrototype()
					.setProject(EntityId.of(PROJECT_ID))
					.setName(moduleName)
					.setPath(completePath)
					.setTechnology(Technology.COBOL)
					.setType(type)
					.setContent(new BinaryString(content))
					.setContentHash(new BinaryValue(contentHash));
			
		sourceService.create(sourcePrototype);

		final ModulePojoPrototype modulePrototype =
			new ModulePojoPrototype()
				.setName(moduleName)
				.setPath(completePath)
				.setProject(EntityId.of(PROJECT_ID))
				.setTechnology(Technology.COBOL)
				.setType(type)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
				.setInfo(Collections.emptyMap())
				.setSourceMetrics(new SourceMetricsPojoPrototype());
		
		return moduleService.create(modulePrototype);
	}

	protected void makeIncludesReference(final Long fromId, final Long toId, final ModuleLocation location) {
		moduleService.createRelationship(
				new ModuleRelationshipPojoPrototype()
					.setRelationship(RelationshipType.INCLUDES)
					.setSrcModule(EntityId.of(fromId))
					.setSrcLocation(location)
					.setDstModule(EntityId.of(toId)));
	}

	private EntityId createAnnotation(final EntityId moduleId, final int offset, final int length) {
		final AnnotationPojoPrototype annotation =
				new AnnotationPojoPrototype()
					.setName(DEAD_CODE_ANNOTATION_NAME)
					.setType(AnnotationType.DEAD_CODE)
					.setState(WorkingState.CANDIDATE)
					.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
					.setModule(moduleId)
					.setLocation(new ModuleLocation(offset, length));
		
		return annotationService.create(annotation);
	}
	
	/**
	 * Submits the provided {@code job} for execution.
	 *
	 * @param jobManager the {@link JobManager}
	 * @param tracer the {@link Tracer}
	 * @param job the {@link Job} to submit
	 *
	 * @return the jobId
	 */
	public static String submitJob(final JobManager jobManager, final Tracer tracer, final Job<?> job) {
		final Span rootSpan = tracer.newTrace();
		String jobId = "";
		try (final Tracer.SpanInScope scope = tracer.withSpanInScope(rootSpan)) {
			final CountDownLatch latch = new CountDownLatch(1);
			final Throwable[] error = new Throwable[1];
			jobId = jobManager.submit(job, new JobExecutionCallback() {

				@Override
				public void onCompletion() {
					latch.countDown();
				}

				@Override
				public void onFailure(@Nullable final Throwable throwable) {
					error[0] = throwable;
					latch.countDown();
				}
			}).getJobId();

			try {
				final boolean countReachedZero = latch.await(10, TimeUnit.MINUTES);
				if ( ! countReachedZero) {
					throw new IllegalStateException("CountDownLatch timed out in BaseDiscoveryTest.submitJob(), possible deadlock! (" + latch.toString() + ")");
				}
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}

			if (error[0] != null) {
				throw new IllegalStateException(error[0]);
			}
		} finally {
			rootSpan.finish();
		}
		return jobId;
	}
	
	private void resetLinesOfDeadCode(EntityId moduleId) {
		moduleService.putSourceMetrics(new SourceMetricsPojoPrototype()
				.setModule(moduleId)
				.setDeadCodeLines(Integer.valueOf(0)));
	}

	private void assertAnnotations(final List<AnnotationPojo> expectedAnnotations, final List<AnnotationPojo> actualAnnotations) {

		/* making sure the returned annotations are sorted by offset in order to be able to compare them */
		Collections.sort(actualAnnotations, Comparator.comparing(a -> a.getLocation().orElseThrow().getOffset()));

		final List<AnnotationPojo> modList = new ArrayList<>(expectedAnnotations);
		Collections.sort(modList, Comparator.comparing(a -> a.getLocation().orElseThrow().getOffset()));

		for (int i = 0; i < modList.size(); i++) {
			final AnnotationPojo expectedAnnotation = modList.get(i);

			assertTrue(i < actualAnnotations.size());
			final AnnotationPojo actualAnnotation = actualAnnotations.get(i);

			assertEquals(expectedAnnotation.getName(), actualAnnotation.getName());
			assertEquals(expectedAnnotation.getType(), actualAnnotation.getType());
			assertEquals(expectedAnnotation.getState(), actualAnnotation.getState());
			assertEquals(expectedAnnotation.getLocation().orElseThrow().getOffset(),
						 actualAnnotation.getLocation().orElseThrow().getOffset());
		}
	}

	/* Assertions */
	private void submitAndAssert(final EntityId projectId, final EntityId moduleId, final int expectedDeadLineCount, final List<AnnotationPojo> expectedAnnotations) {
		submitJob(jobManager, tracer, new IdentifyDeadCodeJob(projectId, new ModuleMatcher(Collections.singletonList(moduleId), Collections.emptyList())));
		
		final ModulePojo module = moduleService.findAnyModule(q -> q.byId(moduleId)).orElseThrow();
		
		assertEquals(expectedDeadLineCount, module.getSourceMetrics().orElseThrow().getDeadCodeLines());
		assertAnnotations(expectedAnnotations, annotationService.find(q -> q.ofProject(assertNotNull(EntityId.of(PROJECT_ID))).ofModule(moduleId)));
	}

	@Override
	protected String getTestFolder() {
		return "WMIN4554";
	}
}
