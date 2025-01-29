/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashSet;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.tags.DiscoveryTest;

/**
 * Test for persisting relationship types at the end of a discover metrics run.
 */
@WithMockUser
@DiscoveryTest
class PersistMetricsRelationshipTest extends DatabaseRelatedTest {

	@Autowired
	private SourceCachingService sourceService;
	
	@Autowired
	private ModuleService moduleService;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private Tracer tracer;

	private static final Path BASE_FOLDER = Paths.get("./test-resources/innowake/mining/server/discovery");
	private static final Path SOURCE_FOLDER = BASE_FOLDER.resolve("source");
	private static final Path TEST_FOLDER = SOURCE_FOLDER.resolve("iris/WCFD143");

	/** 
	 * Test for WMIN-1496: The type of a relationship should depend on the target module instead of source module.
	 * A Cobol program INCLUDES a Copybook and does not CALL it.
	 */
	@Test
	void referencesHaveCorrectRelationshipTypeTest() {
		sourceService.resetCaches();
		final EntityId projectId = createProject().identity();
		uploadResources(projectId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(projectId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(projectId, false));

		final var references = moduleService.findRelationship(q -> q.ofProject(projectId));
		assertEquals(3, references.size());
		for (var reference : references) {
			final String from = moduleService.getModuleLightweight(EntityId.of(reference.getSrcModule())).getName();
			final String to = moduleService.getModuleLightweight(EntityId.of(reference.getDstModule())).getName();
			if (from.equals("COPYREP") && to.equals("PAYLIB")) {
				assertEquals(RelationshipType.INCLUDES, reference.getRelationship());
			} else if (from.equals("COPYREP2") && to.equals("PAYLIB")) {
				assertEquals(RelationshipType.INCLUDES, reference.getRelationship());
			} else if (from.equals("COPYREP2") && to.equals("PAYLIB2")) {
				assertEquals(RelationshipType.INCLUDES, reference.getRelationship());
			} else {
				fail("Unexpected reference: " + reference);
			}
		}
	}

	private ProjectPojo createProject() {
		final Long ONE = Long.valueOf(1);
		final ProjectPojoPrototype project = new ProjectPojoPrototype();
		project.setName(TEST_FOLDER.toString());
		project.setClient(EntityId.of(ONE));
		project.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY)));
		return assertNotNull(projectService.create(project));
	}

	private void uploadResources(final EntityId projectId) {
		try (final Stream<Path> walk = Files.walk(TEST_FOLDER)) {
			walk.filter(Files::isRegularFile).map(path -> {
				final BinaryString content;
				try {
					content = new BinaryString(Files.readAllBytes(path));
				} catch (final IOException e) {
					throw new IllegalStateException(e);
				}
				final ModuleType moduleType = FileExtension.resolve(path.toString());
				return new SourcePojoPrototype()
						.setProject(projectId)
						.setName(path.getFileName().toString())
						.setPath(Paths.get("temp").resolve(SOURCE_FOLDER.getParent().relativize(path)).toString())
						.setTechnology(moduleType.getTechnology())
						.setType(moduleType.getType())
						.setContent(content);
			}).forEach(sourceObject -> sourceService.create(sourceObject));
		} catch (final Exception e) {
			e.printStackTrace();
			fail(e.getMessage());
		}
	}

	/**
	 * Submits the provided {@code job} for execution.
	 *
	 * @param jobManager the {@link JobManager}
	 * @param tracer the {@link Tracer}
	 * @param job the {@link Job} to submit
	 */
	private static void submitJob(final JobManager jobManager, final Tracer tracer, final Job<?> job) {
		final Span rootSpan = tracer.newTrace();
		try (final Tracer.SpanInScope scope = tracer.withSpanInScope(rootSpan)) {
			final CountDownLatch latch = new CountDownLatch(1);
			final Throwable[] error = new Throwable[1];
			jobManager.submit(job, new JobExecutionCallback() {

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
				latch.await(5, TimeUnit.MINUTES);
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}

			if (error[0] != null) {
				throw new IllegalStateException(error[0]);
			}
		} finally {
			rootSpan.finish();
		}
	}
}
