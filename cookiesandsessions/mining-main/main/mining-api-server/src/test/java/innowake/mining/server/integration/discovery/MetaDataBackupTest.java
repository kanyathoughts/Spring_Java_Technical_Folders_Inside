/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.integration.discovery;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import innowake.mining.extensions.metadata.MetaDataExportJob;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import java.io.IOException;
import java.util.Collections;

/**
 * Test to verify the functionality of metaData backup in discovery metrics
 * */
@WithMockUser
class MetaDataBackupTest extends BaseDiscoveryTest {

	@Autowired
	private AnnotationService annotationService;

	@Override protected String getTestFolder() {
		return "WMIN13384";
	}

	/**
	 * This test verifies that the metaData is backed up correctly between subsequent discoveries.
	 */
	@Test
	void testMetaDataBackupWithNewlyCreatedId() {
		final var project = createProject();
		performDiscovery(project.identity());
		final var module = moduleService.findModules(q -> q.ofProject(project.identity()));
		assertEquals(1, module.size());
		final var annotation = new AnnotationPojoPrototype();
		annotation.setName("TEST RULE");
		annotation.setType(AnnotationType.RULE);
		annotation.setState(WorkingState.APPROVED);
		final ModuleLocation dummyLocation = new ModuleLocation(0, 0);
		annotation.setLocation(dummyLocation);
		annotation.setModule(module.get(0).identity());
		annotation.setCreatedByUserId("1");
		annotationService.create(annotation);

		/* Perform the discovery again and verify the metadata is correctly backed up */
		submitJob(jobManager, tracer, new DiscoverMetricsJob(project.identity(), false));
		verifyMetaData(project);
	}


	/* This test verifies that if a metaData backup ID is already present, the metrics job uses this backup to restore instead of creating a new one */
	@Test
	void testMetaDataBackupWithOldCreatedId() throws IOException {
		resetData();
		final var project = createProject();
		performDiscovery(project.identity());
		final var module = moduleService.findModules(q -> q.ofProject(project.identity()));
		assertEquals(1, module.size());
		final var annotation = new AnnotationPojoPrototype();
		annotation.setName("TEST RULE");
		annotation.setType(AnnotationType.RULE);
		annotation.setState(WorkingState.APPROVED);
		final ModuleLocation dummyLocation = new ModuleLocation(0, 0);
		annotation.setLocation(dummyLocation);
		annotation.setModule(module.get(0).identity());
		annotation.setCreatedByUserId("1");
		annotationService.create(annotation);

		/* Create the backup job and delete the annotation to verify that the backup that contains annotation is used */
		final var jobId = submitJob(jobManager, tracer, new MetaDataExportJob(project.identity(), Collections.emptyMap()));
		annotationService.delete(q -> q.ofModule(module.get(0).identity()));
		/* Manually add the jobId to the project to verify that the metaData backup ID is correctly stored */
		projectService.update(new ProjectPojoPrototype().setUid(project.getUid()).setMetaDataBackupId(jobId));

		/* Perform the discovery again and verify the metadata is backed up from the job ID present in the database */
		submitJob(jobManager, tracer, new DiscoverMetricsJob(project.identity(), false));
		verifyMetaData(project);
	}

	private void verifyMetaData(ProjectPojo project) {
		final var module = moduleService.findModules(q -> q.ofProject(project.identity()));
		assertEquals(1, module.size());
		final var annotation2 = annotationService.find(q -> q.ofModule(module.get(0).identity()));
		assertEquals(1, annotation2.size());
		assertEquals("TEST RULE", annotation2.get(0).getName());
		assertEquals(AnnotationType.RULE, annotation2.get(0).getType());
		/* verify the backupId is deleted after the discovery */
		final var projectPojo = projectService.find(project.getUid());
		assertTrue(projectPojo.isPresent());
		assertNull(projectPojo.get().getMetaDataBackupId());
	}

	void performDiscovery(final EntityId entityId) {
		uploadResources(entityId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(entityId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(entityId, false));
	}

}
