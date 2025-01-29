/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.dao;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import innowake.mining.shared.entities.*;
import innowake.mining.shared.model.taxonomy.assignment.AssignmentState;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.job.TaxonomyAssignmentJob;
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
public class TaxonomyCategoryDaoTest extends DatabaseResettingTest {

	private static final EntityId ONE = EntityId.of(Long.valueOf(1));
	private static final Long TWO = Long.valueOf(2);
	private static final Long THREE = Long.valueOf(3);
	@Autowired
	private JobManager jobManager;

	@Autowired
	private Tracer tracer;
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private TaxonomyService taxonomyService;

	/**
	 * Tests to validate the retrieval of all taxonomy categories of the project
	 *
	 */
	@Test
	public void testFindAllTaxonomyCategories() {
		createTaxonomyCategoryTestdata();
		final List<TaxonomyCategoryPojo> taxonomiesList = taxonomyService.findCategories(q -> q.ofProject(ONE));
		assertEquals(TWO, assertNotNull(taxonomiesList).size());
	}

	/**
	 * Tests the create/update of the taxonomy categories
	 *
	 */
	@Test
	public void testCreateUpdateTaxonomyCategories() {
		List<TaxonomyCategoryPojo> taxonomiesList;
		final TaxonomyCategoryPojoPrototype taxonomyCategory1 = new TaxonomyCategoryPojoPrototype().setName("Technical Taxonomies").setProject(ONE);
		final Long taxonomyCategoryResult = taxonomyService.upsertCategory(taxonomyCategory1);
		assertNotNull(taxonomyCategoryResult);

		final Long taxonomyCategoryUpdateResult = taxonomyService.upsertCategory(taxonomyCategory1);
		assertNotNull(taxonomyCategoryUpdateResult);
		taxonomiesList = taxonomyService.findCategories(q -> q.ofProject(ONE));
		assertEquals(TWO, assertNotNull(taxonomiesList).size());

		final TaxonomyCategoryPojoPrototype taxonomyCategoryUpdatee = new TaxonomyCategoryPojoPrototype()
				.setName("UI Technical Taxonomies")
				.setId(taxonomyCategoryUpdateResult);
		/* Sets the new taxonomy name for the given id */
		final Long taxonomyCategoryWithNameUpdated = taxonomyService.upsertCategory(taxonomyCategoryUpdatee);
		assertNotNull(taxonomyCategoryWithNameUpdated);
		taxonomiesList = taxonomyService.findCategories(q -> q.ofProject(ONE));
		assertEquals(TWO, assertNotNull(taxonomiesList).size());

		/*
		 * New taxonomy with same name & project and without id will still update the existing taxonomy matching the same name & project
		 */
		final TaxonomyCategoryPojoPrototype newTaxonomyCategory = new TaxonomyCategoryPojoPrototype().setName("UI Technical Taxonomies").setProject(ONE);
		final Long newtaxonomyCategoryResult = taxonomyService.upsertCategory(newTaxonomyCategory);
		assertNotNull(newtaxonomyCategoryResult);
		taxonomiesList = taxonomyService.findCategories(q -> q.ofProject(ONE));
		assertEquals(TWO, assertNotNull(taxonomiesList).size());

		/*
		 * New taxonomy with different name and without id will create a new taxonomy
		 */
		final TaxonomyCategoryPojoPrototype defaultTaxonomyCategory = new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies II").setProject(ONE);
		final Long defaultTaxonomyCategoryResult = taxonomyService.upsertCategory(defaultTaxonomyCategory);
		assertNotNull(defaultTaxonomyCategoryResult);
		taxonomiesList = taxonomyService.findCategories(q -> q.ofProject(ONE));
		assertEquals(THREE, assertNotNull(taxonomiesList).size());
	}

	/**
	 * Tests to validate that the taxonomy categories are deleted for the project
	 *
	 */
	@Test
	public void testDeleteTaxonomyCategoriesByProjectId() {
		createTaxonomyCategoryTestdata();
		taxonomyService.deleteCategory(q -> q.ofProject(ONE));
		final List<TaxonomyCategoryPojo> taxonomiesList = taxonomyService.findCategories(q -> q.ofProject(ONE));
		assertEquals(0, assertNotNull(taxonomiesList).size());
	}

	/**
	 * Tests to validate that the taxonomy category is deleted
	 *
	 */
	@Test
	public void testDeleteTaxonomyCategoryById() {
		final Long taxonomyCategory = createTaxonomyCategory("Technical Taxonomies", ONE);
		taxonomyService.deleteCategory(q -> q.byId(taxonomyCategory));
		assertFalse("Taxonomy category is not deleted", taxonomyService.findCategories(q -> q.ofProject(ONE)).stream()
				.anyMatch(t -> t.getId() == taxonomyCategory));
	}

	/**
	 * Tests Aggregation of Taxonomy and module to get TaxonomyCategoryAggragate
	 */
	@Test
	void testTaxonomyCategoryAggregateAll() {
		EntityId projectID = createProject(TWO).identity();

		final UUID typeDataDomain = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(projectID).setCategoryId(createTaxonomyCategory("Technical Taxonomies", projectID)));
		final UUID typeRead = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("Read").setProject(projectID).setCategoryId(createTaxonomyCategory("Business Taxonomies", projectID)));

		final EntityId taxonomyAid = createTaxonomy(projectID, typeDataDomain, "Taxonomy A");
		final EntityId taxonomyBid = createTaxonomy(projectID, typeRead, "Taxonomy B");

		final EntityId moduleAId = createModule(projectID, "moduleB", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final EntityId moduleBId = createModule(projectID, "moduleB", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final EntityId moduleCId = createModule(projectID, "moduleC", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);

		final ModuleMatcher moduleMatcher = new ModuleMatcher(Arrays.asList(moduleAId, moduleBId, moduleCId),
				Collections.<String> emptyList());
		final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignmentA = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyAid, AssignmentState.ALL);
		final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignmentB = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyBid, AssignmentState.ALL);

		final TaxonomyAssignmentsSetRequest taxonomyAssignmentsSetRequest = new TaxonomyAssignmentsSetRequest(moduleMatcher,
				Arrays.asList(assignmentA, assignmentB));
		BaseDiscoveryTest.submitJob(jobManager, tracer, new TaxonomyAssignmentJob(projectID, taxonomyAssignmentsSetRequest));
		final Map<Long, Long> taxonomyAggregate = taxonomyService.countModulesPerCategory(q -> q.ofProject(projectID));
		
		assertEquals("Number of taxonomyAggregate should be equal to categories", 2, assertNotNull(taxonomyAggregate).size());
		assertEquals("All module should be assigned to taxonomies", Long.valueOf(3), taxonomyAggregate.get(taxonomyService.getType(typeDataDomain).getCategory().getId()));
		assertEquals("All module should be assigned to taxonomies", Long.valueOf(3), taxonomyAggregate.get(taxonomyService.getType(typeRead).getCategory().getId()));

	}

	private void createTaxonomyCategoryTestdata() {
		createTaxonomyCategory("Technical Taxonomies", ONE);
		createTaxonomyCategory("Business Taxonomies", ONE);
	}

	private Long createTaxonomyCategory(final String name, final EntityId projectId) {
		return assertNotNull(taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName(name).setProject(projectId)));
	}

	private ProjectPojo createProject(final Long clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Test Project"+ clientId)
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet()));
	}

	private EntityId createModule(final EntityId projectID, final String moduleName, final Technology technology, final Type type, final Storage storage,
			final Identification identification, final Origin origin) {
		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setProject(projectID)
				.setName(moduleName)
				.setTechnology(technology)
				.setType(type)
				.setStorage(storage)
				.setIdentification(identification)
				.setOrigin(origin)
				.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	private EntityId createTaxonomy(final EntityId projectId, final UUID taxonomyType, final String taxonomyName) {
		final TaxonomyPojoPrototype proto = new TaxonomyPojoPrototype()
				.setName(taxonomyName)
				.setProject(projectId)
				.setType(taxonomyType);
		return taxonomyService.create(proto);
	}	
}
