/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import innowake.mining.shared.entities.*;
import innowake.mining.shared.model.taxonomy.assignment.AssignmentState;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;

@WithMockUser
class TaxonomyAssignmentJobTest extends DatabaseRelatedTest {
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private Tracer tracer;
	
	@Autowired
	private TaxonomyService taxonomyService;
		
	@Autowired 
	private ModuleService moduleService;

	private static final EntityId CLIENT_ONE = EntityId.of(1L);
	private static final EntityId CLIENT_TWO = EntityId.of(2L);
	
	/**
	 * Test @link{TaxonomyAssignmentJob} for assign and unassign taxonomy from modules
	 */
	@Test
	void testTaxonomyAssignAndUnAssignmentJobForSingleModule() {
		final EntityId projectId = createProject(CLIENT_ONE).identity();
		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		assertNotNull("Taxonomy category cannot be null", taxonomyCategory);
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(projectId).setCategoryId(taxonomyCategory));

		final EntityId taxonomyId = taxonomyService.create(new TaxonomyPojoPrototype().setProject(projectId).setType(type).setName("Taxonomy A"));
		final EntityId moduleA = createModule(projectId, "moduleA", Technology.COBOL, Type.PROGRAM, Storage.FILE, Origin.CUSTOM);

		final ModuleMatcher moduleMatcher = new ModuleMatcher(Arrays.asList(moduleA), Collections.<String> emptyList());
		TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignment = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyId, AssignmentState.ALL);
		TaxonomyAssignmentsSetRequest taxonomyAssignmentsSetRequest = new TaxonomyAssignmentsSetRequest(moduleMatcher, Arrays.asList(assignment));

		assertEquals(0, modulesWithTaxonomyIds(taxonomyId, projectId).size());
		String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, new TaxonomyAssignmentJob(projectId, taxonomyAssignmentsSetRequest));
		assertNotNull("Job Id should not be null", jobId);
		assertEquals("Modules should have taxonomies after assignment", 1, modulesWithTaxonomyIds(taxonomyId, projectId).size());

		assignment = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyId, AssignmentState.NONE);
		taxonomyAssignmentsSetRequest = new TaxonomyAssignmentsSetRequest(moduleMatcher, Arrays.asList(assignment));
		jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, new TaxonomyAssignmentJob(projectId, taxonomyAssignmentsSetRequest));
		assertNotNull("Job Id should not be null", jobId);
		assertEquals("No Taxonomies should present after unassignment", 0, modulesWithTaxonomyIds(taxonomyId, projectId).size());
	}
	
	/**
	 * Test @link{TaxonomyAssignmentJob} for assign and unassign taxonomy from modules
	 * by using assignment state
	 */
	@Test
	void testTaxonomyAssignAndUnAssignmentJobForMultipleModules() {
		final EntityId projectId = createProject(CLIENT_TWO).identity();
		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		assertNotNull(taxonomyCategory);
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(projectId).setCategoryId(taxonomyCategory));

		final EntityId taxonomyIdA = taxonomyService.create(new TaxonomyPojoPrototype().setProject(projectId).setType(type).setName("Taxonomy A"));
		final EntityId taxonomyIdB = taxonomyService.create(new TaxonomyPojoPrototype().setProject(projectId).setType(type).setName("Taxonomy B"));

		final EntityId moduleA = createModule(projectId, "moduleB", Technology.COBOL, Type.PROGRAM, Storage.FILE, Origin.CUSTOM);
		final EntityId moduleB = createModule(projectId, "moduleB", Technology.COBOL, Type.PROGRAM, Storage.FILE, Origin.CUSTOM);
		final EntityId moduleC = createModule(projectId, "moduleC", Technology.COBOL, Type.PROGRAM, Storage.FILE, Origin.CUSTOM);

		final ModuleMatcher moduleMatcher = new ModuleMatcher(Arrays.asList(moduleA, moduleB, moduleC),
				Collections.<String> emptyList());
		final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignmentA = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyIdA, AssignmentState.ALL);
		final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignmentB = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyIdB, AssignmentState.ALL);

		final TaxonomyAssignmentsSetRequest taxonomyAssignmentsSetRequest = new TaxonomyAssignmentsSetRequest(moduleMatcher,
				Arrays.asList(assignmentA, assignmentB));

		String jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, new TaxonomyAssignmentJob(projectId, taxonomyAssignmentsSetRequest));
		assertNotNull("Job Id should not be null", jobId);
		assertEquals("Modules should have taxonomies after assignment", 3, modulesWithTaxonomyIds(taxonomyIdA, projectId).size());
		assertEquals("Modules should have taxonomies after assignment", 3, modulesWithTaxonomyIds(taxonomyIdB, projectId).size());

		taxonomyAssignmentsSetRequest.getTaxonomies().stream().forEach(e -> e.setState(AssignmentState.NONE));
		jobId = BaseDiscoveryTest.submitJob(jobManager, tracer, new TaxonomyAssignmentJob(projectId, taxonomyAssignmentsSetRequest));
		assertNotNull("Job Id should not be null", jobId);
		assertEquals("No Taxonomies should present after unassignment", 0, modulesWithTaxonomyIds(taxonomyIdA, projectId).size());
		assertEquals("No Taxonomies shoud present after unassignment", 0, modulesWithTaxonomyIds(taxonomyIdB, projectId).size());
	}
	
	private List<EntityId> modulesWithTaxonomyIds(final EntityId taxonomyIdA, final EntityId projectId) {
		return taxonomyService.findTaxonomyModulesIds(q -> q.ofProject(projectId).byId(taxonomyIdA));
	}
	
	private ProjectPojo createProject(final EntityId clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Test Project" + clientId.getNid())
				.setClient(clientId)
				.setNatures(Collections.emptySet()));
	}
	
	private EntityId createModule(final EntityId projectId, final String moduleName, final Technology technology, final Type type, final Storage storage,
			final Origin origin) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName(moduleName);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(storage);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(origin);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
}
