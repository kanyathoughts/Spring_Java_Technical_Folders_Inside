/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.dao;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import innowake.mining.shared.model.taxonomy.assignment.AssignmentState;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.job.TaxonomyAssignmentJob;
import innowake.mining.shared.Definable.ValueNotDefinedException;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojo;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;

/**
 * Tests {@link TaxonomyTypeDaoTest}
 */
@WithMockUser
public class TaxonomyTypeDaoTest extends DatabaseResettingTest {
	private static final Long TWO = Long.valueOf(2);
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private Tracer tracer;

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private TaxonomyService taxonomyService;

	/**
	 * Test to create taxonomy type with default taxonomy category
	 */
	@Test
	void testCreateTaxonomyTypeWithDefaultTaxonomyCategory() {
		final EntityId projectId = createProject().identity();
		final UUID typeId = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("BusinessProcess").setProject(projectId));
		final TaxonomyTypePojo resultTaxonomyType = taxonomyService.getType(typeId);
		assertNotNull(resultTaxonomyType);
		assertNotNull(resultTaxonomyType.getCategory());
	}

	/**
	 * Test the create taxonomy type with the taxonomy category present in the DB
	 *
	 */
	@Test
	void testCreateTaxonomyTypeWithAvailableTaxonomyCategory() {
		final EntityId projectId = createProject().identity();
		final Long categoryId = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID typeId = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("BusinessProcess").setProject(projectId).setCategoryId(categoryId));
		final TaxonomyTypePojo resultTaxonomyType = taxonomyService.getType(typeId);
		assertNotNull(resultTaxonomyType);
		assertNotNull(resultTaxonomyType.getCategory());
	}

	/**
	 * Test the create taxonomy type with the taxonomy category not present in the DB
	 *
	 */
	@Test
	void testCreateTaxonomyTypeWithNewTaxonomyCategory() {
		final EntityId projectId = createProject().identity();
		final Long categoryId = Long.valueOf(-1);
		assertThrows(ConstraintViolationException.class, () -> {
			taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("BusinessProcess").setProject(projectId).setCategoryId(categoryId));
		}, "Should have thrown exception as the provided taxonomy category is not present in the DB");
	}
	
	/**
	 * Tests that taxonomy type gets deleted when it has no taxonomies
	 */
	@Test
	void testDeleteTaxonomyTypeWithNoTaxonomies() {
		final EntityId projectId = createProject().identity();

		final Long categoryId = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID typeId = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("BusinessProcess").setProject(projectId).setCategoryId(categoryId));
		final TaxonomyTypePojo resultTaxonomyType = taxonomyService.getType(typeId);

		final List<TaxonomyPojo> createdTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName(resultTaxonomyType.getName()));
		assertTrue(createdTaxonomies.isEmpty(), "No taxonomies has been created so it should be empty.");
		
		taxonomyService.delete(q -> q.ofProject(projectId).withTypeName(resultTaxonomyType.getName()));
		assertEquals(13, taxonomyService.find(q -> q.ofProject(projectId)).size());
	}
	
	/**
	 * Tests that when a taxonomy type is deleted it deletes the taxonomies as well
	 */
	@Test
	void testDeleteTaxonomyTypeWithMultipleTaxonomies() {
		final EntityId projectId = createProject().identity();
		final Long categoryId = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID typeId = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("BusinessProcess").setProject(projectId).setCategoryId(categoryId));
		final TaxonomyTypePojo taxonomyType = taxonomyService.getType(typeId);
		
		assertEquals(13, taxonomyService.find(q -> q.ofProject(projectId)).size());
		for (int i = 1; i <= 3; i++) {
			TaxonomyPojoPrototype proto = new TaxonomyPojoPrototype()
					.setName("BusinessProcessTaxonomy " + i)
					.setType(typeId)
					.setProject(projectId);
			taxonomyService.create(proto);
		}
		
		final List<TaxonomyPojo> createdTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName(taxonomyType.getName()));
		assertEquals(3, createdTaxonomies.size());
		createdTaxonomies.stream().forEach(t -> assertTrue(t.getName().startsWith("BusinessProcessTaxonomy")));

		taxonomyService.delete(q -> q.ofProject(projectId).withTypeName(taxonomyType.getName()));

		assertEquals(13, taxonomyService.find(q -> q.ofProject(projectId)).size());
		assertEquals(0, taxonomyService.find(q -> q.ofProject(projectId).withTypeName(taxonomyType.getName())).size());
		createdTaxonomies.stream().forEach(createdTaxonomy -> {
			final EntityId id = createdTaxonomy.identity();
			assertThrows(MiningEntityNotFoundException.class, () -> taxonomyService.get(id), "The taxonomy should have been deleted");
		});
	}
	
	/**
	 * Tests functionality of update Taxonomy type
	 */
	@Test
	void testUpdateTaxonomyType() {
		final EntityId projectId = createProject().identity();

		final Long category = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final Long newCategoryId = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Professional Taxonomies").setProject(projectId));
		final UUID typeId = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("BusinessProcess").setProject(projectId).setCategoryId(category));
		final TaxonomyTypePojo taxonomyType = taxonomyService.getType(typeId);
		final TaxonomyCategoryPojo newCategory = taxonomyService.getCategory(newCategoryId);

		assertEquals("BusinessProcess", taxonomyType.getName());
		assertEquals("Business Taxonomies", taxonomyType.getCategory().getName());
		
		var updateProto = new TaxonomyTypePojoPrototype()
			.setId(taxonomyType.getId())
			.setName("UpdatedBusinessProcess")
			.setCategoryId(newCategoryId);
		taxonomyService.updateType(updateProto);
		final List<TaxonomyTypePojo> retrievedTypes = taxonomyService.findTypes(q -> q.ofProject(projectId));
		assertEquals(5, retrievedTypes.size());
		
		final TaxonomyTypePojo updatedType = retrievedTypes.stream()
				.filter(taxonomyType1 -> taxonomyType1.getName().equals("UpdatedBusinessProcess"))
				.findFirst().get();
		assertEquals("UpdatedBusinessProcess", updatedType.getName());
		assertEquals(newCategory.getName(), updatedType.getCategory().getName());
	}
	
	/**
	 * Tests functionality of update Taxonomy type with no Rid provided
	 */
	@Test
	void testUpdateTaxonomyTypeWithNoRid() {
		final EntityId projectId = createProject().identity();

		final Long category = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID typeId = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("BusinessProcess").setProject(projectId).setCategoryId(category));
		final TaxonomyTypePojo taxonomyType = taxonomyService.getType(typeId);
		
		assertEquals("BusinessProcess", taxonomyType.getName());
		assertEquals("Business Taxonomies", taxonomyType.getCategory().getName());
		
		final TaxonomyTypePojoPrototype newTaxonomyType = new TaxonomyTypePojoPrototype()
				.setName(taxonomyType.getName())
				.setProject(taxonomyType.getProject())
				.setCategoryId(taxonomyType.getCategory().getId());
		assertThrows(ValueNotDefinedException.class, () -> taxonomyService.updateType(newTaxonomyType), "Should have thrown exception since Rid is not set.");
	}
	
	/**
	 * Tests Aggregation of Taxonomy and module to get TaxonomyTypeAggregate
	 */
	@Test
	void testTaxonomyTypeAggregateAll() {
		final EntityId projectID = createProject(TWO).identity();

		final UUID typeDataDomain = taxonomyService.createType(
				new TaxonomyTypePojoPrototype()
				.setName("DataDomain")
				.setProject(projectID)
				.setCategoryId(createTaxonomyCategory("Technical Taxonomies", projectID)));
		final UUID typeRead = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("Read").setProject(projectID).setCategoryId(createTaxonomyCategory("Business Taxonomies", projectID)));

		final EntityId taxonomyAid = createTaxonomy(projectID, typeDataDomain, "Taxonomy A");
		final EntityId taxonomyBid = createTaxonomy(projectID, typeRead, "Taxonomy B");

		final EntityId moduleA = createModule(projectID, "moduleB", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final EntityId moduleB = createModule(projectID, "moduleB", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final EntityId moduleC = createModule(projectID, "moduleC", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);

		final ModuleMatcher moduleMatcher = new ModuleMatcher(Arrays.asList(moduleA, moduleB, moduleC),
				Collections.<String> emptyList());
		final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignmentA = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyAid, AssignmentState.ALL);
		final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignmentB = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyBid, AssignmentState.ALL);

		final TaxonomyAssignmentsSetRequest taxonomyAssignmentsSetRequest = new TaxonomyAssignmentsSetRequest(moduleMatcher,
				Arrays.asList(assignmentA, assignmentB));
		BaseDiscoveryTest.submitJob(jobManager, tracer, new TaxonomyAssignmentJob(projectID, taxonomyAssignmentsSetRequest));
		final Map<Long, Long> taxonomyAggregate = taxonomyService.countModulesPerCategory(q -> q.ofProject(projectID));
		assertEquals(2, innowake.lib.core.lang.Assert.assertNotNull(taxonomyAggregate).size(), "Number of taxonomyAggregate should be equal to categories");
		assertEquals(Long.valueOf(3), taxonomyAggregate.get(taxonomyService.getType(typeDataDomain).getCategory().getId()), "All module should be assigned to taxonomies");
		assertEquals(Long.valueOf(3), taxonomyAggregate.get(taxonomyService.getType(typeRead).getCategory().getId()), "All module should be assigned to taxonomies");

	}

	private Long createTaxonomyCategory(final String name, final EntityId projectId) {
		return taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName(name).setProject(projectId));
	}

	private ProjectPojo createProject(final Long clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Test Project"+ clientId)
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet()));
	}

	private EntityId createModule(final EntityId projectId, final String moduleName, final Technology technology, final Type type,
			final Storage storage, final Identification identification, final Origin origin) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName(moduleName);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(storage);
		module.setIdentification(identification);
		module.setOrigin(origin);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	private EntityId createTaxonomy(final EntityId projectId, final UUID taxonomyType, final String taxonomyName) {
		final TaxonomyPojoPrototype proto = new TaxonomyPojoPrototype().setName(taxonomyName).setProject(projectId).setType(taxonomyType);
		return taxonomyService.create(proto);
	}

	private ProjectPojo createProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName("TEST PROJECT 1")
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet()));
	}
}
