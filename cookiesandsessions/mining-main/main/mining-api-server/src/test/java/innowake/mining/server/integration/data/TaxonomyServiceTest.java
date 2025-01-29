/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for {@link TaxonomyService}
 */
class TaxonomyServiceTest extends DatabaseRelatedTest {

	@Autowired
	private TaxonomyService taxonomyService;

	@Autowired
	private ModuleService moduleService;

	/**
	 * Tests {@link TaxonomyService#createModuleLink(UUID, EntityId)} (Long, Long)} and {@link TaxonomyDao#unassign(Long, Long)} for valid and 
	 * invalid taxonomy assignments.
	 */
	@Test
	void testAssignAndUnassignTaxonomies() {
		final EntityId projectId = projectService.create(new ProjectPojoPrototype()
				.setName("Project")
				.setClient(EntityId.of(2L))
				.setNatures(Collections.emptySet())
		).identity();

		final EntityId moduleId = createModule(projectId, "MODULE B", Technology.ECL, Type.TEXT,
				Storage.FILE, Identification.MISSING, Origin.ENVIRONMENT);


		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(projectId)
					.setCategoryId(taxonomyCategory));
		createModule(projectId, "MODULE A", Technology.JAVA, Type.PROGRAM,
				Storage.FILE, Identification.MISSING, Origin.ENVIRONMENT);

		final EntityId taxonomy = createTaxonomy(projectId, type, "Taxonomy A");

		/* Assign Taxonomy that does not exist. It should create an assignment. */
		assertEquals(0, queryCountOfAssignments(moduleId, taxonomy), "Taxonomy Assignment should not exist before assigning.");
		final long created1 = taxonomyService.createModuleLink(moduleId.getUid(), taxonomy);
		assertEquals(1, created1);
		assertEquals(1, queryCountOfAssignments(moduleId, taxonomy), "1 Taxonomy Assignment should exist after assigning.");

		/* Assign Taxonomy that already exists. It should do nothing. */
		final long created2 = taxonomyService.createModuleLink(moduleId.getUid(), taxonomy);
		assertEquals(0, created2);
		assertEquals(1, queryCountOfAssignments(moduleId, taxonomy), "Assigning same Taxonomy multiple times should not create new edges.");

		/* Unassign Taxonomy that already exists. It should delete the assignment. */
		final long deleted1 = taxonomyService.deleteModuleLinks(q -> q.ofModule(moduleId).byId(taxonomy));
		assertEquals(1, deleted1);
		assertEquals(0, queryCountOfAssignments(moduleId, taxonomy), "Taxonomy Assignment should not exist after unassignment.");

		/* Unassign Taxonomy that does not exists. It should do nothing. */
		final long deleted2 = taxonomyService.deleteModuleLinks(q -> q.ofModule(moduleId).byId(taxonomy));
		assertEquals(0, deleted2);
		assertEquals(0, queryCountOfAssignments(moduleId, taxonomy), "Unassigning same Taxonomy multiple times should not have any effect.");
	}

	/**
	 * Tests {@link TaxonomyService#find(BuildingConsumer)}  for finding all Taxonomies based on parameters passed}.
	 */
	@Test
	void findAllTaxonomies() {
		final EntityId projectId = projectService.create(new ProjectPojoPrototype()
				.setName("Test Project")
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet())
		).identity();

		final EntityId moduleId = createModule(projectId, "MODULE A", Technology.JAVA, Type.PROGRAM, Storage.FILE, Identification.MISSING, Origin.ENVIRONMENT);

		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(projectId)
				.setCategoryId(taxonomyCategory));
		final EntityId taxonomyA = createTaxonomy(projectId, type, "Taxonomy A");
		final EntityId taxonomyB = createTaxonomy(projectId, type, "Taxonomy B");

		taxonomyService.createModuleLink(moduleId.getUid(), taxonomyA);

		/* Calling the method findAll */
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("DataDomain"));

		assertEquals(2, taxonomies.size());
		assertEquals(projectId, taxonomies.get(0).getProject());

		final List<TaxonomyPojo> countModulesPerTaxonomy = taxonomyService
				.find(q -> q.withModuleCountsReferencingTaxonomies(Arrays.asList(taxonomyA, taxonomyB)).ofProject(projectId).ofModule(moduleId));
		assertEquals(1, countModulesPerTaxonomy.size());
		assertEquals(0, countModulesPerTaxonomy.stream().mapToLong(t -> t.getTaxonomyReferenceCount()).sum());

		taxonomyService.createModuleLink(moduleId.getUid(), taxonomyB);

		final List<TaxonomyPojo> countModulesPerTaxonomy2 = taxonomyService
				.find(q -> q.withModuleCountsReferencingTaxonomies(Arrays.asList(taxonomyA, taxonomyB)).ofProject(projectId).ofModule(moduleId));
		assertEquals(2, countModulesPerTaxonomy2.size());
		assertEquals(2, countModulesPerTaxonomy2.stream().mapToLong(t -> t.getTaxonomyReferenceCount()).sum());
	}

	private Long queryCountOfAssignments(final EntityId moduleId, final EntityId taxonomy) {
		return taxonomyService.count(q -> q.ofModule(moduleId).byId(taxonomy));
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

	private EntityId createTaxonomy(final EntityId projectId,final UUID type, final String taxonomyName) {
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setName(taxonomyName)
				.setProject(projectId)
				.setType(type);
		return taxonomyService.create(taxonomy);
	}
}
