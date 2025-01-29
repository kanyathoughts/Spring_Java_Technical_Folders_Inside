/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import static innowake.mining.server.integration.discovery.BaseDiscoveryTest.submitJob;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.Collections;
import java.util.UUID;
import java.util.concurrent.Callable;

import innowake.mining.shared.entities.*;
import innowake.mining.shared.model.taxonomy.assignment.AssignmentState;
import org.apache.commons.lang3.time.StopWatch;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;

/**
 * Performance Test for {@link TaxonomyTypeDeletionJob}.
 */
@Disabled("Long running performance tests for manual execution")
@WithMockUser
class TaxonomyTypeDeletionPerformanceTest extends DatabaseResettingTest {
	
	private static final Logger LOG = LoggerFactory.getLogger(TaxonomyTypeDeletionPerformanceTest.class);
	@Autowired
	private ModuleService moduleService;

	@Autowired
	private TaxonomyService taxonomyService;

	@Autowired
	private ProjectService projectService1;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private Tracer tracer;
	
	@Test
	void testTaxonomyTypeDeletionPerformance() {
		final EntityId projectId = createProject();
		
		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("TaxonomyDataDomain").setProject(projectId).setCategoryId(taxonomyCategory));
		final EntityId taxonomyId = taxonomyService.create(new TaxonomyPojoPrototype().setProject(projectId).setType(type).setName("Taxonomy term1"));
		LOG.info("Creating Data for Taxonomy Assignment job");
		for (int i = 0; i < 100; i++) {
			final EntityId moduleI = createModule(projectId, "MODULE I" + i, Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED,
					Origin.CUSTOM);
			
			taxonomyService.createModuleLink(moduleI.getUid(), taxonomyId);
			for (int j = 0; j < 100; j++) {
				final EntityId moduleJ = createModule(projectId, "MODULE I" + i + " J" + j, Technology.COBOL, Type.PROGRAM, Storage.FILE,
						Identification.IDENTIFIED, Origin.CUSTOM);
				createReference(RelationshipType.CALLS, moduleI, moduleJ);
				final ModuleMatcher moduleMatcher = new ModuleMatcher(Arrays.asList(moduleI, moduleJ),
						Collections.<String> emptyList());
		
				final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignmentA = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyId, AssignmentState.ALL);

				final TaxonomyAssignmentsSetRequest taxonomyAssignmentsSetRequest = new TaxonomyAssignmentsSetRequest(moduleMatcher,
						Arrays.asList(assignmentA));

				final String jobId = submitJob(jobManager, tracer, new TaxonomyAssignmentJob(projectId, taxonomyAssignmentsSetRequest));
				assertNotNull("Job Id should not be null", jobId);
			}
			
		}
		LOG.info("Data created for Taxonomy Assignment");
		/* Deleted the taxonomy type and assigned taxonomies involving 10000 modules took 000:00:20.127 */
		time(() -> {
			final String typeName = taxonomyService.getType(type).getName();
			submitJob(jobManager, tracer, new TaxonomyTypeDeletionJob(projectId, typeName));
			/* Just return a bogus value */
			return Boolean.TRUE;
		}, "Deleted the taxonomy type and assigned taxonomies involving 10000 modules");
		
	}
	
	private <V> V time(final Callable<V> runnable, final String message) {
		final StopWatch watch = new StopWatch();
		watch.start();
		final V result;
		try {
			result = runnable.call();
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
		watch.stop();
		LOG.info(() -> String.format("%s took %s", message, watch.toString()));
		return result;
	}
	
	private EntityId createProject() {
		return projectService1.create(new ProjectPojoPrototype()
				.setName("Test Performance Project ")
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet())).identity();
	}
	
	private EntityId createModule(final EntityId projectId, final String moduleName, final Technology technology, final Type type, final Storage storage,
			final Identification identification, final Origin origin) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setCreator(Creator.API);
		module.setProject(projectId);
		module.setName(moduleName);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(storage);
		module.setIdentification(identification);
		module.setOrigin(origin);
		return moduleService.create(module);
	}

	private UUID createReference(final RelationshipType relationship, final EntityId incomingIndex, final EntityId outgoingIndex) {
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype();
		reference.setRelationship(relationship);
		reference.setSrcModule(incomingIndex);
		reference.setDstModule(outgoingIndex);
		return moduleService.createRelationship(reference);
	}
}

