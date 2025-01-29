/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.mining.server.JobTestHelper.waitForJobCompletion;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.StringStartsWith.startsWith;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.Instant;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import javax.persistence.EntityNotFoundException;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.DummyTestJob;
import innowake.mining.server.JobTestHelper;
import innowake.mining.server.event.MarkedForDeletionEvent;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.job.deletion.BackgroundDeletionJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.model.ProjectNature;

/**
 * Tests verifying the behavior of {@link ProjectService}.
 */
class ProjectServiceTest extends DatabaseRelatedTest {

	@Autowired
	private TaxonomyService taxonomyService;
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	protected ApplicationEventPublisher eventPublisher;

	@Autowired
	private MiningJobService miningJobService;
	
	@Autowired
	private Tracer tracer;
	
	private static final Long ONE = Long.valueOf(1);

	/**
	 * Test to validate that Project's taxonomy, taxonomy type and taxonomy category gets deleted on project delete.
	 */
	@Test
	@WithMockUser /* Required for submitting the deletion job to the job manager */
	void testProjectTaxonomiesDeletion() {
		final EntityId projectId = createTestProject("Test Project").identity();

		Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Test Taxonomy Category").setProject(projectId));
		final List<TaxonomyCategoryPojo> taxonomyCategoryBeforeDelete = taxonomyService.findCategories(q -> q.ofProject(projectId));

		final UUID taxonomyType = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("Test Taxonomy Type").setProject(projectId).setCategoryId(taxonomyCategory));
		final List<TaxonomyTypePojo> taxonomyTypesBeforeDelete = taxonomyService.findTypes(q -> q.ofProject(projectId));

		@SuppressWarnings("unused")
		final EntityId taxonomy1 = taxonomyService.create(new TaxonomyPojoPrototype().setName("Test Taxonomy 1").setProject(projectId).setType(taxonomyType));
		@SuppressWarnings("unused")
		final EntityId taxonomy2 = taxonomyService.create(new TaxonomyPojoPrototype().setName("Test Taxonomy 2").setProject(projectId).setType(taxonomyType));
		@SuppressWarnings("unused")
		final EntityId taxonomy3 = taxonomyService.create(new TaxonomyPojoPrototype().setName("Test Taxonomy 3").setProject(projectId).setType(taxonomyType));
		
		final List<TaxonomyPojo> taxonomyBeforeDelete = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Test Taxonomy Type"));
		assertTrue("Project should contain Taxonomy Category", taxonomyCategoryBeforeDelete.size() > 1);
		assertEquals(5, taxonomyTypesBeforeDelete.size());
		assertEquals(3, taxonomyBeforeDelete.size());
		Map<Long, Long> countModulesPerCategory = taxonomyService.countModulesPerCategory(q -> q.ofProject(projectId));
		assertEquals(0, countModulesPerCategory.get(taxonomyCategory));
		assertEquals(0, taxonomyBeforeDelete.get(0).getTaxonomyReferenceCount());

		Instant now = Instant.now();
		projectService.markForDeletion(projectId, true);
		waitForJobCompletion(
				JobTestHelper.findJobByLastSubmitTime(jobManager, BackgroundDeletionJob.DESCRIPTION, now, 10)
					.orElseThrow(() -> new IllegalStateException("Sumbitted job not found")).getJobId(),
				jobManager, 1, TimeUnit.MINUTES);

		final List<TaxonomyCategoryPojo> taxonomyCategoryAfterDelete = taxonomyService.findCategories(q -> q.ofProject(projectId));
		final List<TaxonomyTypePojo> taxonomyTypesAfterDelete = taxonomyService.findTypes(q -> q.ofProject(projectId));
		final List<TaxonomyPojo> taxonomyAfterDelete = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Test Taxonomy Type"));

		assertEquals(0, taxonomyCategoryAfterDelete.size());
		assertEquals(0, taxonomyTypesAfterDelete.size());
		assertEquals(0, taxonomyAfterDelete.size());
	}

	@Test
	@WithMockUser /* Required for submitting the deletion job to the job manager */
	void asynchronousProjectDeletion() {
		final EntityId projectId = createTestProject("Test Project").identity();

		/* Verify key attributes for project deletion */
		final ProjectPojo foundProject = projectService.find(q -> q.withId(projectId).filterMarkedForDeletion(null)).get(0);
		assertEquals(false, foundProject.isMarkedDeleted());
		assertThat(foundProject.getName(), not(startsWith("_TO_BE_DELETED")));

		/* Delete the project */
		projectService.markForDeletion(projectId, false);
		
		/* Verify attributes after marked for deletion */
		final ProjectPojo softDeletedProject = projectService.find(q -> q.withId(projectId).filterMarkedForDeletion(null)).get(0);
		assertEquals(true, softDeletedProject.isMarkedDeleted());
		assertThat(softDeletedProject.getName(), startsWith("_TO_BE_DELETED"));

		/* The service already makes sure to not return the project */
		assertThrows(EntityNotFoundException.class, () -> projectService.get(projectId));
		
		Instant now = Instant.now();
		eventPublisher.publishEvent(new MarkedForDeletionEvent());
		waitForJobCompletion(
			JobTestHelper.findJobByLastSubmitTime(jobManager, BackgroundDeletionJob.DESCRIPTION, now, 10)
				.orElseThrow(() -> new IllegalStateException("Sumbitted job not found")).getJobId(),
			jobManager, 1, TimeUnit.MINUTES);
			
		/* After the deletion job, the repository should also throw a record not found exception */
		assertEquals(0, projectService.find(q -> q.withId(projectId).filterMarkedForDeletion(null)).size());
	}
	
	@Test
	@WithMockUser
	void testJobInfoForDeletedProject() {
		final EntityId projectToBeDeletedId = createTestProject("Test Project1").identity();
		final EntityId projectIdNew = createTestProject("Test Project2").identity();

		BaseDiscoveryTest.submitJob(jobManager, tracer, new DummyTestJob(projectToBeDeletedId));
		BaseDiscoveryTest.submitJob(jobManager, tracer, new DummyTestJob(projectToBeDeletedId));
		BaseDiscoveryTest.submitJob(jobManager, tracer, new DummyTestJob(projectIdNew));
		final List<UUID> projectToBeDelJobIds1 = miningJobService.findJobIdForProject(projectToBeDeletedId);
		final List<UUID> newProjectJobIds1 = miningJobService.findJobIdForProject(projectIdNew);

		assertEquals(2, projectToBeDelJobIds1.size(), "Miningjobinfo Should not be empty before deleting the project " + projectToBeDeletedId.getNid());
		assertEquals(ONE, newProjectJobIds1.size(), "Miningjobinfo Should not be empty before deleting project " + projectToBeDeletedId.getNid());
		assertEquals(2, jobManager.getJobs(q -> q.byIds(projectToBeDelJobIds1)).size(),
				"JobInfo should not be empty for project " + projectToBeDeletedId.getNid());
		final var now = Instant.now();
		projectService.markForDeletion(projectToBeDeletedId, true);
		waitForJobCompletion(
				JobTestHelper.findJobByLastSubmitTime(jobManager, BackgroundDeletionJob.DESCRIPTION, now, 10)
					.orElseThrow(() -> new IllegalStateException("Sumbitted job not found")).getJobId(),
				jobManager, 1, TimeUnit.MINUTES);

		final List<UUID> projectToBeDelJobIds2 = miningJobService.findJobIdForProject(projectToBeDeletedId);
		final List<UUID> newProjectJobIds2 = miningJobService.findJobIdForProject(projectIdNew);

		assertEquals(0, miningJobService.findJobIdForProject(projectToBeDeletedId).size(),
				"mining Job info should be empty for deleted project " + projectToBeDeletedId);
		assertEquals(0, jobManager.getJobs(q -> q.byIds(projectToBeDelJobIds2)).size(),
				"JobInfo should be empty for deleted project " + projectToBeDeletedId);

		assertEquals(ONE, newProjectJobIds2.size(), "Jobinfo should not be empty for existing project " + projectIdNew);
		assertEquals(ONE, jobManager.getJobs(q -> q.byIds(newProjectJobIds2)).size(),
				"JobInfo should not be empty for existing project " + projectIdNew);
	}

	private ProjectPojo createTestProject(final String projectName) {
		return projectService.create(new ProjectPojoPrototype()
				.setName(projectName)
				.setClient(EntityId.of(ONE))
				.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY))));
	}
}
