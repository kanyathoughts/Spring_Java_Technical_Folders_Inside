/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.job.TaxonomyAssignmentJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.AssignmentState;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;

/**
 * Tests for {@link TaxonomyService} findAll method with moduleCountsReferencingTaxonomies filter which tests the possible combinations for Decomposition
 */
@WithMockUser
class TaxonomyDecompositionTest extends DatabaseRelatedTest {
	
	@Autowired
	private TaxonomyService taxonomyService;

	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private JobManager jobManager;
	
	@Autowired
	private Tracer tracer;
	
	private EntityId projectId = EntityId.of(1L);
	
	@BeforeAll
	void ensureSetup() {
		projectId = projectService.create(new ProjectPojoPrototype()
				.setName("Test Project")
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet()))
				.identity();
		
		createModule("MODULE A");
		createModule("MODULE B");
	}
	
	@Test
	@Order(1)
	void findAllTaxonomiesWhenBatchAloneIsSelected() {
	
		final EntityId moduleId = getModule("MODULE A").identity();
		final EntityId moduleIdB = getModule("MODULE B").identity();
		
		final List<TaxonomyPojo> programTypeTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Program Type"));

		final EntityId batch = programTypeTaxonomies.stream().filter(t -> t.getName().equals("Batch"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Batch' found")).identity();
		final EntityId library =
				programTypeTaxonomies.stream().filter(t -> t.getName().equals("Library"))
						.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Library' found")).identity();

		taxonomyService.createModuleLink(moduleId.getUid(), batch);
		taxonomyService.createModuleLink(moduleIdB.getUid(), library);
		
		/* Calling the method findAll when batch alone is selected */
		final List<TaxonomyPojo> taxonomies1 = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Collections.singleton(batch)));
		
		assertEquals(4, taxonomies1.size());
		assertEquals(1, taxonomies1.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		unAssignTaxonomy(moduleId, Collections.singletonList(batch));
		unAssignTaxonomy(moduleIdB, Collections.singletonList(library));
	}

	@Test
	@Order(2)
	void shouldReturnOtherProgramTypeTaxonomiesAssignedRefCountWhenBatchIsSelected() {
	
		final EntityId moduleId = getModule("MODULE A").identity();
		final EntityId moduleIdB = getModule("MODULE B").identity();
		
		final List<TaxonomyPojo> programTypeTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Program Type"));
		
		final EntityId batch = programTypeTaxonomies.stream().filter(t -> t.getName().equals("Batch"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Batch' found")).identity();
		final EntityId library = programTypeTaxonomies.stream().filter(t -> t.getName().equals("Library"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Library' found")).identity();
		final EntityId ui = programTypeTaxonomies.stream().filter(t -> t.getName().equals("UI"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'UI' found")).identity();
		
		taxonomyService.createModuleLinks(Collections.singletonList(moduleId), Arrays.asList(batch, library));
		taxonomyService.createModuleLink(moduleIdB.getUid(), ui);
		
		/* Calling the method findAll when batch alone is selected */
		final List<TaxonomyPojo> taxonomies1 = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Collections.singleton(batch)));
		
		assertEquals(4, taxonomies1.size());
		assertEquals(2, taxonomies1.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		unAssignTaxonomy(moduleId, List.of(batch, library));
		unAssignTaxonomy(moduleIdB, Collections.singletonList(ui));
	}
	
	@Test
	@Order(3)
	void refCountWhenMultipleProgramTypeTaxonomiesSelected() {

		final EntityId moduleId = getModule("MODULE A").identity();
		final EntityId moduleIdB = getModule("MODULE B").identity();

		final List<TaxonomyPojo> programTypeTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Program Type"));
		
		final EntityId batch = programTypeTaxonomies.stream().filter(t -> t.getName().equals("Batch"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Batch' found")).identity();
		final EntityId library =
				programTypeTaxonomies.stream().filter(t -> t.getName().equals("Library"))
						.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Library' found")).identity();
		final EntityId ui = programTypeTaxonomies.stream().filter(t -> t.getName().equals("UI"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'UI' found")).identity();
		final EntityId mq = programTypeTaxonomies.stream().filter(t -> t.getName().equals("MQ"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'MQ' found")).identity();
		
		taxonomyService.createModuleLinks(Collections.singleton(moduleId), Arrays.asList(batch, library, ui));
		taxonomyService.createModuleLink(moduleIdB.getUid(), mq);
		
		/* Calling the method findAll when multiple program type taxonomies which are assigned to a module are selected */
		final List<TaxonomyPojo> taxonomies1 = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Arrays.asList(batch,library,ui)));
		
		assertEquals(4, taxonomies1.size());
		assertEquals(3, taxonomies1.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		/* Calling the method findAll when multiple program type taxonomies are selected but not all of them are assigned to any module*/
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Arrays.asList(batch,library,ui,mq)));
		
		assertEquals(4, taxonomies2.size());

		unAssignTaxonomy(moduleId, List.of(batch, library, ui));
		unAssignTaxonomy(moduleIdB, Collections.singletonList(mq));
	}

	@Test
	@Order(4)
	void refCountWhenSingleStoreTaxonomySelected() {
		
		final EntityId moduleId = getModule("MODULE A").identity();
		final EntityId moduleIdB = getModule("MODULE B").identity();
		
		final List<TaxonomyPojo> dbAccessTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("DB Access"));
		
		final EntityId updateTax = dbAccessTaxonomies.stream().filter(t -> t.getName().equals("Store"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Store' found")).identity();
		final EntityId storeTax = dbAccessTaxonomies.stream().filter(t -> t.getName().equals("Update"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Update' found")).identity();
		
		taxonomyService.createModuleLink(moduleId.getUid(), storeTax);
		taxonomyService.createModuleLink(moduleIdB.getUid(), updateTax);

		/* Calling the method findAll when Store Taxonomy is selected but no Program type taxonomies are assigned to the module where Store is assigned */
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Collections.singleton(storeTax)));

		assertEquals(4, taxonomies.size());
		assertEquals(0, taxonomies.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		final List<TaxonomyPojo> programTypeTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Program Type"));
		final EntityId batch = programTypeTaxonomies.stream().filter(t -> t.getName().equals("Batch"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Batch' found")).identity();
		
		taxonomyService.createModuleLink(moduleId.getUid(), batch);
		
		/* Calling the method findAll when Store Taxonomy is selected and one of the Program type taxonomies are assigned
		 * to the module where Store is assigned */
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Collections.singletonList(storeTax)));

		assertEquals(4, taxonomies2.size());
		assertEquals(1, taxonomies2.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());

		unAssignTaxonomy(moduleId, List.of(batch, storeTax));
		unAssignTaxonomy(moduleIdB, Collections.singletonList(updateTax));
	}
	
	@Test
	@Order(5)
	void refCountWhenMultipleDbAccessTaxonomiesSelected() {
		
		final EntityId moduleId = getModule("MODULE A").identity();
		final EntityId moduleIdB = getModule("MODULE B").identity();
		
		final List<TaxonomyPojo> dbAccessTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("DB Access"));
		
		final EntityId updateTax = dbAccessTaxonomies.stream().filter(t -> t.getName().equals("Store"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Store' found")).identity();
		final EntityId storeTax = dbAccessTaxonomies.stream().filter(t -> t.getName().equals("Update"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Update' found")).identity();
		
		taxonomyService.createModuleLinks(Arrays.asList(moduleId, moduleIdB), Arrays.asList(updateTax,storeTax));

		/* Calling the method findAll when Store and Update Taxonomy are selected but no Program type taxonomies are 
		 * assigned to the module where Store and Update taxonomies are assigned */
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Arrays.asList(storeTax, updateTax)));

		assertEquals(4, taxonomies.size());
		assertEquals(0, taxonomies.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		final List<TaxonomyPojo> programTypeTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Program Type"));
		
		final EntityId batch = programTypeTaxonomies.stream().filter(t -> t.getName().equals("Batch"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Batch' found")).identity();
		final EntityId library = programTypeTaxonomies.stream().filter(t -> t.getName().equals("Library"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Library' found")).identity();
		taxonomyService.createModuleLink(moduleId.getUid(), batch);
		taxonomyService.createModuleLink(moduleIdB.getUid(), library);
		
		/* Calling the method findAll when Store and Update Taxonomy are selected when Program type taxonomies are 
		 * assigned to the module where Store and Update taxonomies are assigned */
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Arrays.asList(storeTax, updateTax)));

		assertEquals(4, taxonomies2.size());
		assertEquals(2, taxonomies2.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		unAssignTaxonomy(moduleId, List.of(batch, storeTax, updateTax));
		unAssignTaxonomy(moduleIdB, List.of(storeTax, library, updateTax));
	}
	
	@Test
	@Order(6)
	void refCountTaxonomiesWhenSingleFileAccessReadTaxonomySelected() {

		final EntityId moduleId = getModule("MODULE A").identity();
		final EntityId moduleIdB = getModule("MODULE B").identity();
		
		final List<TaxonomyPojo> fileAccessTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("File Access"));
		
		final EntityId fileAccessReadTax = fileAccessTaxonomies.stream().filter(t -> t.getName().equals("Read"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Read' found")).identity();
		final EntityId fileAccessWriteTax = fileAccessTaxonomies.stream().filter(t -> t.getName().equals("Write"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Write' found")).identity();
		
		taxonomyService.createModuleLink(moduleId.getUid(), fileAccessReadTax);
		taxonomyService.createModuleLink(moduleIdB.getUid(), fileAccessWriteTax);

		/* Calling the method findAll when Read Taxonomy is selected but no Program type taxonomies are assigned to the module where Read is assigned */
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Collections.singleton(fileAccessReadTax)));

		assertEquals(4, taxonomies.size());
		assertEquals(0, taxonomies.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		final List<TaxonomyPojo> programTypeTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Program Type"));
		final EntityId batch = programTypeTaxonomies.stream().filter(t -> t.getName().equals("Batch")).findFirst()
				.orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Batch' found"))
				.identity();
		
		taxonomyService.createModuleLink(moduleId.getUid(), batch);
		
		/* Calling the method findAll when Read Taxonomy is selected and one of the Program type taxonomies are assigned
		 * to the module where Read is assigned */
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Collections.singletonList(fileAccessReadTax)));

		assertEquals(4, taxonomies2.size());
		assertEquals(1, taxonomies2.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		unAssignTaxonomy(moduleId, List.of(batch, fileAccessReadTax));
		unAssignTaxonomy(moduleIdB, Collections.singletonList(fileAccessWriteTax));
	}
	
	@Test
	@Order(7)
	void refCountTaxonomiesWhenMultipleFileAccessTaxonomiesSelected() {
		
		final EntityId moduleId = getModule("MODULE A").identity();
		final EntityId moduleIdB = getModule("MODULE B").identity();
		
		final List<TaxonomyPojo> fileAccessTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("File Access"));
		
		final EntityId fileAccessReadTax = fileAccessTaxonomies.stream().filter(t -> t.getName().equals("Read"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Read' found")).identity();
		final EntityId fileAccessWriteTax = fileAccessTaxonomies.stream().filter(t -> t.getName().equals("Write"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Write' found")).identity();
		
		taxonomyService.createModuleLinks(Arrays.asList(moduleId, moduleIdB), Arrays.asList(fileAccessReadTax, fileAccessWriteTax));

		/* Calling the method findAll when Read and Write Taxonomies are selected but no Program Type taxonomies are assigned 
		 * to the module where Read and Write are assigned */
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Arrays.asList(fileAccessReadTax, fileAccessWriteTax)));

		assertEquals(4, taxonomies.size());
		assertEquals(0, taxonomies.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		final List<TaxonomyPojo> programTypeTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Program Type"));
		
		final EntityId ui = programTypeTaxonomies.stream().filter(t -> t.getName().equals("UI"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'UI' found")).identity();
		final EntityId mq = programTypeTaxonomies.stream().filter(t -> t.getName().equals("MQ"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'MQ' found")).identity();
		
		taxonomyService.createModuleLink(moduleId.getUid(), ui);
		taxonomyService.createModuleLink(moduleIdB.getUid(), mq);
		
		/* Calling the method findAll when Read and Write Taxonomy is selected and one of the Program type taxonomies are assigned
		 * to the module where Read and Write are assigned */
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Arrays.asList(fileAccessReadTax, fileAccessWriteTax)));

		assertEquals(4, taxonomies2.size());
		assertEquals(2, taxonomies2.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());

		unAssignTaxonomy(moduleId, List.of(ui, fileAccessReadTax, fileAccessWriteTax));
		unAssignTaxonomy(moduleIdB, List.of(mq, fileAccessWriteTax, fileAccessReadTax));
	}
	
	@Test
	@Order(8)
	void refCountWhenSingleBusinessTaxonomySelected() {
		final EntityId moduleId = getModule("MODULE A").identity();
		final EntityId moduleIdB = getModule("MODULE B").identity();
		
		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(projectId)
				.setCategoryId(taxonomyCategory));
		final EntityId taxonomyA = createTaxonomy(projectId, type, "Taxonomy A");
		final EntityId taxonomyB = createTaxonomy(projectId, type, "Taxonomy B");

		taxonomyService.createModuleLink(moduleId.getUid(), taxonomyA);
		taxonomyService.createModuleLink(moduleIdB.getUid(), taxonomyB);

		/* Calling the method findAll when taxonomyA is selected but no Program type taxonomies are assigned to the module where taxonomyA is assigned */
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Collections.singleton(taxonomyA)));

		assertEquals(4, taxonomies.size());
		assertEquals(0, taxonomies.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		final List<TaxonomyPojo> programTypeTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Program Type"));
		final EntityId batch = programTypeTaxonomies.stream().filter(t -> t.getName().equals("Batch"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Batch' found")).identity();
		
		taxonomyService.createModuleLink(moduleId.getUid(), batch);
		
		/* Calling the method findAll when taxonomyA is selected and one of the Program type taxonomies are assigned
		 * to the module where taxonomyA is assigned */
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Collections.singleton(taxonomyA)));

		assertEquals(4, taxonomies2.size());
		assertEquals(1, taxonomies2.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		unAssignTaxonomy(moduleId, List.of(taxonomyA, batch));
		unAssignTaxonomy(moduleIdB, Collections.singletonList(taxonomyB));
	}
	
	@Test
	@Order(9)
	void refCountWhenMultipleBusinessTaxonomySelected() {
		final EntityId moduleId = getModule("MODULE A").identity();
		final EntityId moduleIdB = getModule("MODULE B").identity();
		
		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(projectId));
		final UUID type =
				taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain1").setProject(projectId).setCategoryId(taxonomyCategory));
		final EntityId taxonomy1 = createTaxonomy(projectId, type, "Taxonomy 1");
		final EntityId taxonomy2 = createTaxonomy(projectId, type, "Taxonomy 2");
		final EntityId taxonomy3 = createTaxonomy(projectId, type, "Taxonomy 3");

		taxonomyService.createModuleLinks(Collections.singletonList(moduleId), Arrays.asList(taxonomy1, taxonomy2, taxonomy3));
		taxonomyService.createModuleLink(moduleIdB.getUid(), taxonomy2);

		/* Calling the method findAll when taxonomy1 taxonomy2 taxonomy3 are selected but no Program type taxonomies are assigned to the module where 
		 * taxonomies are assigned */
		final List<TaxonomyPojo> taxonomies = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Arrays.asList(taxonomy1, taxonomy2, taxonomy3)));

		assertEquals(4, taxonomies.size());
		assertEquals(0, taxonomies.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());
		
		final List<TaxonomyPojo> programTypeTaxonomies = taxonomyService.find(q -> q.ofProject(projectId).withTypeName("Program Type"));
		final EntityId batch = programTypeTaxonomies.stream().filter(t -> t.getName().equals("Batch"))
				.findFirst().orElseThrow(() -> new NoSuchElementException("No taxonomy with name 'Batch' found")).identity();
		
		taxonomyService.createModuleLink(moduleId.getUid(), batch);
		
		/* Calling the method findAll when taxonomy1 taxonomy2 taxonomy3 are selected and one of the Program type taxonomies are assigned
		 * to the module where taxonomies are assigned */
		final List<TaxonomyPojo> taxonomies2 = taxonomyService.find(q -> q.ofProject(projectId)
				.withTypeName("Program Type").withModuleCountsReferencingTaxonomies(Arrays.asList(taxonomy1, taxonomy2, taxonomy3)));

		assertEquals(4, taxonomies2.size());
		assertEquals(1, taxonomies2.stream().mapToLong(TaxonomyPojo::getTaxonomyReferenceCount).sum());

		unAssignTaxonomy(moduleId, List.of(taxonomy1, taxonomy2, taxonomy3, batch));
		unAssignTaxonomy(moduleIdB, Collections.singletonList(taxonomy2));
	}
	
	private void createModule(final String moduleName) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName(moduleName);
		module.setTechnology(Technology.JAVA);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification( Identification.MISSING);
		module.setOrigin(Origin.ENVIRONMENT);
		module.setCreator(Creator.DISCOVERY);
		moduleService.create(module);
	}

	private EntityId createTaxonomy(final EntityId projectId,final UUID type, final String taxonomyName) {
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setName(taxonomyName)
				.setProject(projectId)
				.setType(type);
		return taxonomyService.create(taxonomy);
	}
	
	private ModulePojo getModule(final String name) {
		return moduleService.findAnyModule(b -> b.ofProject(projectId).withName(name).withType(Type.PROGRAM).withTechnology(Technology.JAVA))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with name: " + name));
	}
	
	private void unAssignTaxonomy(final EntityId moduleId, final List<EntityId> taxonomyIds) {
		final ModuleMatcher moduleMatcher = new ModuleMatcher(Collections.singletonList(moduleId), Collections.<String> emptyList());
		
		for (final EntityId taxonomyId : taxonomyIds) {
			final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignment = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomyId,
					AssignmentState.NONE);
			final TaxonomyAssignmentsSetRequest taxonomyAssignmentsSetRequest = new TaxonomyAssignmentsSetRequest(moduleMatcher, List.of(assignment));
			BaseDiscoveryTest.submitJob(jobManager, tracer, new TaxonomyAssignmentJob(projectId, taxonomyAssignmentsSetRequest));
		}
	}
}
