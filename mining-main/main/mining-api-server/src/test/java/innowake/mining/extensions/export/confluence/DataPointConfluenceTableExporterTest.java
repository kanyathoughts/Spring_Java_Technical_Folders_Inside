/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.export.confluence;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import com.google.gson.Gson;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.extensions.export.csv.AbstractCSVExporterTest;
import innowake.mining.extensions.export.utils.ExportTestUtils;
import innowake.mining.server.service.MiningJobService;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Test GraphQL query result to CSV conversion
 */
@WithMockUser
public class DataPointConfluenceTableExporterTest extends AbstractCSVExporterTest {

	/* empty project */
	protected static final EntityId PROJECT_ID = EntityId.of(4L);

	@Autowired
	protected DataPointConfluenceTableExporter confluenceTableExporter;

	@Autowired
	protected Tracer tracer;

	@Autowired
	protected JobManager jobManager;

	@Autowired
	protected MiningJobService miningJobService;

	private EntityId rootJob = EntityId.VOID;
	private EntityId stepA = EntityId.VOID;
	private EntityId dummyprg = EntityId.VOID;

	@BeforeAll
	@Override
	public void insertTestData() {
		var module = new ModulePojoPrototype();
		module.setProject(PROJECT_ID);
		module.setName("ROOTJOB");
		module.setTechnology(Technology.COBOL);
		module.setType(Type.COPYBOOK);
		module.setStorage(Storage.FILE);
		module.setPath("ROOTJOB.CBL");
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		rootJob = moduleService.create(module);

		module = new ModulePojoPrototype();
		module.setProject(PROJECT_ID);
		module.setName("ROOTJOB.STEPA");
		module.setTechnology(Technology.COBOL);
		module.setType(Type.EXEC_PGM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		stepA = moduleService.create(module);

		module = new ModulePojoPrototype();
		module.setProject(PROJECT_ID);
		module.setName("DUMMYPRG");
		module.setTechnology(Technology.BASIC);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		dummyprg = moduleService.create( module);

		final ModuleLocation moduleLocation = new ModuleLocation(1, 2);
		final Long createdCategory = annotationService.createCategory(PROJECT_ID, "Test Category", Collections.emptyList());
		annotationService.create(new AnnotationPojoPrototype()
				.setModule(rootJob)
				.setLocation(moduleLocation)
				.setName("Test")
				.setCategoryId(createdCategory)
				.setState(WorkingState.CANDIDATE)
				.setType(AnnotationType.DEAD_CODE)
				.setSourceAttachment(new BinaryString("ABC"))
				.setCreatedByUserId("")
				.setUpdatedByUserId(""));

		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype();
		reference.setRelationship(RelationshipType.CALLS);
		reference.setSrcModule(rootJob);
		reference.setDstModule(stepA);
		reference.setSrcLocation(moduleLocation);
		reference.setDstLocation(moduleLocation);
		moduleService.createRelationship(reference);
	}

	@Test
	void testModules() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("modules"));
		parameters.put("$columns", Arrays.asList("content.name", "content.technology", "content.type"));

		final var linkHashA = moduleService.getModule(dummyprg).getLinkHash();
		final var linkHashB = moduleService.getModule(stepA).getLinkHash();
		final var linkHashC = moduleService.getModule(rootJob).getLinkHash();

		ExportTestUtils.exportAndCompare(getJobResult(PROJECT_ID, parameters), "||Module Name||Technology||Type||",
				String.format("|[DUMMYPRG|http://localhost/#/project-4/module-%s/details/overview]|BASIC|PROGRAM|", linkHashA),
				String.format("|[ROOTJOB.STEPA|http://localhost/#/project-4/module-%s/details/overview]|COBOL|EXEC_PGM|", linkHashB),
				String.format("|[ROOTJOB|http://localhost/#/project-4/module-%s/details/overview]|COBOL|COPYBOOK|", linkHashC));
	}

	@Test
	void testModulesWithTechnologyFilter() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("modules"));
		parameters.put("$columns", Arrays.asList("content.name", "content.technology", "content.type"));
		List<String> filterObject = List.of(new Gson().toJson(Map.of("content_technology", Map.of("eq", "COBOL"))));
		parameters.put("filterObject", filterObject);

		final var linkHashA = moduleService.getModule(stepA).getLinkHash();
		final var linkHashB = moduleService.getModule(rootJob).getLinkHash();

		ExportTestUtils.exportAndCompare(getJobResult(PROJECT_ID, parameters), "||Module Name||Technology||Type||",
				String.format("|[ROOTJOB.STEPA|http://localhost/#/project-4/module-%s/details/overview]|COBOL|EXEC_PGM|", linkHashA),
				String.format("|[ROOTJOB|http://localhost/#/project-4/module-%s/details/overview]|COBOL|COPYBOOK|", linkHashB));
	}

	@Test
	void testModulesWithTechnologyAndTypeFilter() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("modules"));
		parameters.put("$columns", Arrays.asList("content.name", "content.path", "content.technology", "content.type"));
		List<String> filterObject = List.of(new Gson().toJson(Map.of(
				"content_technology", Map.of("eq", "COBOL"),
				"content_type", Map.of("eq", "COPYBOOK"))));
		parameters.put("filterObject", filterObject);

		final var linkHash = moduleService.getModule(rootJob).getLinkHash();

		ExportTestUtils.exportAndCompare(getJobResult(PROJECT_ID, parameters), "||Module Name||Path||Technology||Type||",
				String.format("|[ROOTJOB|http://localhost/#/project-4/module-%s/details/overview]|ROOTJOB.CBL|COBOL|COPYBOOK|", linkHash));
	}

	@Test
	void testModulesWithSizeParameter() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("modules"));
		parameters.put("$columns", Arrays.asList("content.name", "content.technology", "content.type"));
		/* tests that number parameter is parsed correctly */
		parameters.put("size", Arrays.asList("2"));

		/* expected is 3 because there is one additional row for the column headers */
		assertEquals(3, ExportTestUtils.jobResultToString((getJobResult(PROJECT_ID, parameters))).split("\n").length);
	}

	@Test
	void testAnnotations() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("annotations"));
		parameters.put("$columns", Arrays.asList("content.module.name", "content.type", "content.categoryName", "content.state",
				"content.name", "content.sourceAttachment", "content.updatedByUserName"));

		final var linkHash = moduleService.getModule(rootJob).getLinkHash();

		ExportTestUtils.exportAndCompare(getJobResult(PROJECT_ID, parameters),
				"||Module Name||Annotation Type||Category||State||Annotation Description||Source Code||Modified By||",
				String.format("|[ROOTJOB|http://localhost/#/project-4/module-%s/details/overview]|DEAD_CODE|Test Category|CANDIDATE|Test|ABC|SYSTEM|",
						linkHash));
	}

	@Test
	void testIdRowDeletionOnlyWhenModuleNamePresent() throws IOException {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("$query", Collections.singletonList("modules"));
		parameters.put("$columns", Arrays.asList("content.technology", "content.type"));
		ExportTestUtils.exportAndCompare(getJobResult(PROJECT_ID, parameters), "||Technology||Type||", "|BASIC|PROGRAM|", "|COBOL|COPYBOOK|",
				"|COBOL|EXEC_PGM|");
	}

	protected JobResult getJobResult(final EntityId projectId, final Map<String, List<String>> parameters) {
		return miningJobService.getJobResult(submitJob(confluenceTableExporter.createJob(projectId, parameters)))
				.orElseThrow(() -> new IllegalStateException("Failed to execute export Job."));
	}

	protected String submitJob(final Job<?> job) {
		final Span rootSpan = tracer.newTrace();
		try (final Tracer.SpanInScope scope = tracer.withSpanInScope(rootSpan)) {
			final CountDownLatch latch = new CountDownLatch(1);
			final Throwable[] error = new Throwable[1];
			final JobMonitor monitor = jobManager.submit(job, new JobExecutionCallback() {

				@Override
				public void onCompletion() {
					latch.countDown();
				}

				@Override
				public void onFailure(@Nullable final Throwable throwable) {
					error[0] = throwable;
					latch.countDown();
				}
			});

			try {
				final boolean countReachedZero = latch.await(10, TimeUnit.MINUTES);
				if (!countReachedZero) {
					throw new IllegalStateException("CountDownLatch timed out in DataFlowGraphTest.submitJob(), possible deadlock! (" + latch.toString() + ")");
				}
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}

			if (error[0] != null) {
				throw new IllegalStateException(error[0]);
			}
			return monitor.getJobId();
		} finally {
			rootSpan.finish();
		}
	}
}
