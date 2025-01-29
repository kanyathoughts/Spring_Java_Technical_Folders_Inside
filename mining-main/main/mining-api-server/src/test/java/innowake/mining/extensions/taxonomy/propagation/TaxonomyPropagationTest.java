/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.taxonomy.propagation;

import static innowake.mining.server.integration.discovery.BaseDiscoveryTest.submitJob;
import static innowake.mining.shared.model.ReferenceAttributes.DB_ACCESS_TYPES;
import static java.util.Arrays.asList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsIterableContainingInOrder.contains;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.extensions.propagation.taxonomy.TaxonomyPropagation;
import innowake.mining.extensions.propagation.taxonomy.TaxonomyPropagationJob;
import innowake.mining.extensions.propagation.taxonomy.TaxonomyPropagationModuleIdentificationJob;
import innowake.mining.extensions.propagation.taxonomy.TaxonomyPropagationRequest;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.service.TaxonomyModelService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.PropagationData;

/**
 * Tests for {@link TaxonomyPropagation}.
 */
@TestInstance(Lifecycle.PER_CLASS)
@WithMockUser
class TaxonomyPropagationTest extends DatabaseRelatedTest {
	
	private static final List<RelationshipType> CALLS = List.of(RelationshipType.CALLS);
	private static final List<RelationshipType> INCLUDES = List.of(RelationshipType.INCLUDES);
	private static final List<RelationshipType> REFERENCES = List.of(RelationshipType.REFERENCES);
	private static final List<RelationshipType> ACCESSES = List.of(RelationshipType.ACCESSES);

	@Autowired
	protected ModuleService moduleService;

	@Autowired
	protected TaxonomyService taxonomyService;

	@Autowired
	protected TaxonomyModelService taxonomyModelService;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private Tracer tracer;

	private TaxonomyTypePojo type;

	private EntityId projectId;

	@BeforeAll
	public void insertTestData() {
		projectId = createProject().identity();
		final var categoryProto = new TaxonomyCategoryPojoPrototype()
			.setName("Business Taxonomies")
			.setProject(projectId);
		final Long categoryId = taxonomyService.upsertCategory(categoryProto);
		var taxonomyTypeProto = new TaxonomyTypePojoPrototype()
			.setName("DataDomain")
			.setProject(projectId)
			.setCategoryId(categoryId);

		final UUID id = taxonomyService.createType(taxonomyTypeProto);
		type = taxonomyService.getType(id);
	}

	@Test
	void testTaxonomyPropagationUniqueRelationship() {
		/*
		 * Test data for unique relationship.
		 * 
		 * Entry Point --> 		Module A (TaxonomyPojoPrototype A)
		 * 						|
		 * 						|
		 * Propagation Level1	+---> Module A1 (TaxonomyPojoPrototype A propagated)
		 *   						|
		 * 							|
		 * Propagation Level2		+---> Module A2 (TaxonomyPojoPrototype A propagated)
		 */
		final var moduleA = createModule(projectId, "MODULE A", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleA1 = createModule(projectId, "MODULE A1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleA2 = createModule(projectId, "MODULE A2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		
		final String taxonomy = "TaxonomyPojoPrototype A";
		final EntityId taxonomyA = createTaxonomy(type, taxonomy);

		createReference(RelationshipType.CALLS, moduleA, moduleA1);
		createReference(RelationshipType.CALLS, moduleA1, moduleA2);
		taxonomyService.createModuleLink(moduleA.getUid(), taxonomyA);
		
		final List<String> modulesIds = Collections.singletonList(String.valueOf(moduleA.getUid()));
		final List<String> taxonomyIds = Collections.singletonList(String.valueOf(taxonomyA.getUid()));
		
		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(modulesIds, taxonomyIds, CALLS, CALLS,
								Collections.emptyList())));
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, getPropagationData(jobId)));

		final List<TaxonomyPojo> taxonomyListForModuleA = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleA));
		assertEquals(1, taxonomyListForModuleA.size());
		assertEquals(taxonomy, taxonomyListForModuleA.get(0).getName());

		final List<TaxonomyPojo> taxonomyListForModuleA1 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleA1));
		assertEquals(1, taxonomyListForModuleA1.size());
		assertEquals(taxonomy, taxonomyListForModuleA1.get(0).getName());

		final List<TaxonomyPojo> taxonomyListForModuleA2 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleA2));
		assertEquals(1, taxonomyListForModuleA2.size());
		assertEquals(taxonomy, taxonomyListForModuleA2.get(0).getName());
	}

	@Test
	void testTaxonomyPropagationMultipleRelationship() {
		/*
		 * Test data for multiple assignments.
		 *
		 * Entry Point --> 		Module B1 (TaxonomyPojoPrototype B & C(C propagated))		Module B2 (TaxonomyPojoPrototype C & B(B propagated))
		 * 						|													|
		 * 						|													|
		 * Propagation Level1	+---> Module B3 (TaxonomyPojoPrototype B and C propagated)<------+
		 * 									|
		 * 									|
		 * Propagation Level2				+---> Module B4 (TaxonomyPojoPrototype B and C propagated)
		 */
		final var moduleB1 = createModule(projectId, "Module B1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleB2 = createModule(projectId, "Module B2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleB3 = createModule(projectId, "Module B3", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleB4 = createModule(projectId, "Module B4", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);

		final String taxB = "TaxonomyPojoPrototype B";
		final String taxC = "TaxonomyPojoPrototype C";
		final EntityId taxonomyB = createTaxonomy(type, taxB);
		final EntityId taxonomyC = createTaxonomy(type, taxC);

		createReference(RelationshipType.CALLS, moduleB1, moduleB3);
		createReference(RelationshipType.CALLS, moduleB2, moduleB3);
		createReference(RelationshipType.CALLS, moduleB3, moduleB4);
		taxonomyService.createModuleLink(moduleB1.getUid(), taxonomyB);
		taxonomyService.createModuleLink(moduleB2.getUid(), taxonomyC);

		final List<String> moduleIds = asList(String.valueOf(moduleB1.getUid()), String.valueOf(moduleB2.getUid()));
		final List<String> taxonomyIds = asList(String.valueOf(taxonomyB.getUid()), String.valueOf(taxonomyC.getUid()));
		
		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds, CALLS, CALLS,
								Collections.emptyList())));
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, getPropagationData(jobId)));
		
		final List<String> taxonomyListForModuleB1 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleB1))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleB1, Matchers.containsInAnyOrder(taxB, taxC));

		final List<String> taxonomyListForModuleB2 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleB2))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleB2, Matchers.containsInAnyOrder(taxC, taxB));

		final List<String> taxonomyListForModuleB3 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleB3))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleB3, Matchers.containsInAnyOrder(taxC, taxB));

		final List<String> taxonomyListForModuleB4 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleB4))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleB4, Matchers.containsInAnyOrder(taxC, taxB));
	}

	@Test
	void testTaxonomyPropagationRecursion() {
		/*
		 * Test data for recursion.
		 * 
		 * Entry Point -->				Module C1 (TaxonomyPojo X)						Module C2 (TaxonomyPojo Y)
		 * 								|											|
		 * 								|											|
		 * Propagation Level1	+>>>----+--> Module C3 (TaxonomyPojo X)					+---> Module C4 (TaxonomyPojo Y)
		 * 						|				|											|
		 * 						|				|											|
		 * Propagation Level2	+---------------+>>>-------------------------------------<<<+
		 */
		final var moduleC1 = createModule(projectId, "Module C1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleC2 = createModule(projectId, "Module C2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleC3 = createModule(projectId, "Module C3", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleC4 = createModule(projectId, "Module C4", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);

		final String taxX = "Taxonomy X";
		final String taxY = "Taxonomy Y";
		final String taxZ = "Taxonomy z";
		final EntityId taxonomyX = createTaxonomy(type, taxX);
		final EntityId taxonomyY = createTaxonomy(type, taxY);
		final EntityId taxonomyZ = createTaxonomy(type, taxZ);

		createReference(RelationshipType.CALLS, moduleC1, moduleC3);
		createReference(RelationshipType.CALLS, moduleC2, moduleC4);
		createReference(RelationshipType.CALLS, moduleC3, moduleC4);
		createReference(RelationshipType.CALLS, moduleC4, moduleC3);
		taxonomyService.createModuleLink(moduleC1.getUid(), taxonomyX);
		taxonomyService.createModuleLinks(moduleC2, List.of(taxonomyY, taxonomyZ));

		final List<String> moduleIds = asList(String.valueOf(moduleC1.getUid()), String.valueOf(moduleC2.getUid()));
		final List<String> taxonomyIds = asList(String.valueOf(taxonomyX.getUid()), String.valueOf(taxonomyY.getUid()), String.valueOf(taxonomyZ.getUid()));

		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds, CALLS, CALLS,
								Collections.emptyList())));
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, getPropagationData(jobId)));
		
		final List<TaxonomyPojo> taxonomyListForModuleC1 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleC1).sortName(SortDirection.ASCENDING));
		assertEquals(3, taxonomyListForModuleC1.size());
		assertThat(taxonomyListForModuleC1.stream()
				.map(TaxonomyPojo::getName)
				.collect(Collectors.toList()), contains(taxX, taxY, taxZ));

		final List<TaxonomyPojo> taxonomyListForModuleC2 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleC2));
		assertEquals(3, taxonomyListForModuleC2.size());
		assertThat(taxonomyListForModuleC1.stream()
				.map(TaxonomyPojo::getName)
				.collect(Collectors.toList()), contains(taxX, taxY, taxZ));

		final List<String> taxonomyListForModuleC3 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleC3))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleC3, Matchers.containsInAnyOrder(taxX, taxY, taxZ));

		final List<String> taxonomyListForModuleC4 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleC4))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleC4, Matchers.containsInAnyOrder(taxX, taxY, taxZ));
	}

	@Test
	void testTaxonomyPropagationEntryPointPropagation() {
		/*
		 * Test data for entry point propagation.
		 * 
		 * Entry Point --> 		+----->	Module 1 (TaxonomyPojo 1)													Module 2 (TaxonomyPojo 2)
		 * 						|		|																		|
		 * 						|		|																		|
		 * Propagation Level1	|		+---> Module 3 (TaxonomyPojo 1 propagated)									+---> Module 4 (TaxonomyPojo 1 & 2 propagated)
		 * 						|																					|
		 * 						|																					|
		 * Propagation Level2	+-----------------------------------------------------------------------------------+
		 * 
		 * Note that Module 4 receives TaxonomyPojo 1 and 2 simultaneously in Level 1 from Modules 1 and 2.
		 * This is because the CALLS relationship is specified in the INCOMING and OUTGOING direction in the call to the TaxonomyPojo Propagation.
		 */
		
		final var module1 = createModule(projectId, "Module 1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var module2 = createModule(projectId, "Module 2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var module3 = createModule(projectId, "Module 3", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var module4 = createModule(projectId, "Module 4", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);

		final String tax1 = "Taxonomy 1";
		final String tax2 = "Taxonomy 2";
		final EntityId taxonomy1 = createTaxonomy(type, tax1);
		final EntityId taxonomy2 = createTaxonomy(type, tax2);

		createReference(RelationshipType.CALLS, module1, module3);
		createReference(RelationshipType.CALLS, module2, module4);
		createReference(RelationshipType.CALLS, module4, module1);
		taxonomyService.createModuleLink(module1.getUid(), taxonomy1);
		taxonomyService.createModuleLink(module2.getUid(), taxonomy2);

		final List<String> moduleIds = asList(String.valueOf(module1.getUid()), String.valueOf(module2.getUid()));
		final List<String> taxonomyIds = asList(String.valueOf(taxonomy1.getUid()), String.valueOf(taxonomy2.getUid()));

		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds, CALLS, CALLS,
				Collections.emptyList())));
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, getPropagationData(jobId)));

		final List<String> taxonomyListForModule1 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(module1))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModule1, Matchers.containsInAnyOrder(tax1, tax2));

		final List<String> taxonomyListForModule2 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(module2))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModule2, Matchers.containsInAnyOrder(tax2, tax1));

		final List<String> taxonomyListForModule4 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(module4))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModule4, Matchers.containsInAnyOrder(tax1, tax2));

		final List<String> taxonomyListForModule3 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(module3))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModule3, Matchers.containsInAnyOrder(tax1, tax2));
	}

	@Test
	void testSelectiveTaxonomyPropagation() {
		/*
		 * Test data for selective Taxonomy Propagation.
		 * 
		 * Entry Point -->							ModuleV1 (TV1)							Module G1 (TG1)
		 * 												|	|									|
		 * 												|	|									|
		 * Propagation Level1		Module R1 (TR1)<----+ 	+--> Module V2 (TV1 propagated)		+-------------> Module G2 (TG1 propagated)
		 *   						|									|											|
		 * 							|									|											|
		 * Propagation Level2		+---> Module R2 (No Propagation)	+--> Module VG (TV1 and TG1 propagated) <---+
		 */

		final var moduleV1 = createModule(projectId, "Module V1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleV2 = createModule(projectId, "Module V2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleR1 = createModule(projectId, "Module R1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleR2 = createModule(projectId, "Module R2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleG1 = createModule(projectId, "Module G1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleG2 = createModule(projectId, "Module G2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleVG = createModule(projectId, "Module VG", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);

		createReference(RelationshipType.CALLS, moduleV1, moduleV2);
		createReference(RelationshipType.REFERENCES, moduleV1, moduleR1);
		createReference(RelationshipType.CALLS, moduleR1, moduleR2);
		createReference(RelationshipType.CALLS, moduleG1, moduleG2);
		createReference(RelationshipType.CALLS, moduleV2, moduleVG);
		createReference(RelationshipType.CALLS, moduleG2, moduleVG);

		final String taxTV1 = "TV1";
		final String taxTG1 = "TG1";
		final String taxTR1 = "TR1";
		final EntityId taxonomyV1 = createTaxonomy(type, taxTV1);
		final EntityId taxonomyG1 = createTaxonomy(type, taxTG1);
		final EntityId taxonomyR1 = createTaxonomy(type, taxTR1);

		taxonomyService.createModuleLink(moduleV1.getUid(), taxonomyV1);
		taxonomyService.createModuleLink(moduleG1.getUid(), taxonomyG1);
		taxonomyService.createModuleLink(moduleR1.getUid(), taxonomyR1);

		final List<String> moduleIds = asList(String.valueOf(moduleV1.getUid()), String.valueOf(moduleG1.getUid()));
		final List<String> taxonomyIds = asList(String.valueOf(taxonomyV1.getUid()), String.valueOf(taxonomyG1.getUid()));

		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds, CALLS, CALLS,
				Collections.emptyList())));
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, getPropagationData(jobId)));
		
		final List<String> taxonomyListForModuleV1 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleV1))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleV1, Matchers.containsInAnyOrder(taxTV1, taxTG1));

		final List<String> taxonomyListForModuleG1 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleG1))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleG1, Matchers.containsInAnyOrder(taxTG1, taxTV1));

		final List<TaxonomyPojo> taxonomyListForModuleR1 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleR1));
		assertEquals(1, taxonomyListForModuleR1.size());
		assertEquals(taxTR1, taxonomyListForModuleR1.get(0).getName());

		assertEquals(0, taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleR2)).size());

		final List<String> taxonomyListForModuleV2 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleV2))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleV2, Matchers.containsInAnyOrder(taxTV1, taxTG1));

		final List<String> taxonomyListForModuleG2 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleG2))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleG2, Matchers.containsInAnyOrder(taxTG1, taxTV1));

		final List<String> taxonomyListForModuleVG = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleVG))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleVG, Matchers.containsInAnyOrder(taxTG1, taxTV1));
	}

	@Test
	void testEdgeDirectionPropagation() {
		/*
		 * Test data for edge direction propagation.
		 * 
		 * Entry Point --> 		MMRS7112 (TaxonomyPojo A)
		 * 						|
		 * 						|
		 * Propagation Level1	+---> MMRS710A (TaxonomyPojo A propagated) <----+-------+-----------+-----------+-----------+ 
		 * 																	|		|			|			|			|
		 *   																|		|			|			|			|
		 * 																	|		|			|			|			|
		 * Propagation Level2											MMRS71Z3	MMRS71D1	MMRS71Z2	MMRS7101	MMRS7102 (TaxonomyPojo A Propagated)
		 */
		
		final var MMRS7112 = createModule(projectId, "MMRS7112", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var MMRS710A = createModule(projectId, "MMRS710A", Technology.COBOL, Type.COPYBOOK, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var MMRS71Z3 = createModule(projectId, "MMRS71Z3", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var MMRS71D1 = createModule(projectId, "MMRS71D1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var MMRS71Z2 = createModule(projectId, "MMRS71Z2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var MMRS7101 = createModule(projectId, "MMRS7101", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var MMRS7102 = createModule(projectId, "MMRS7102", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		
		final String taxMmrs = "Taxonomy MMRS";
		final EntityId taxonomyA = createTaxonomy(type, taxMmrs);
		
		createReference(RelationshipType.CALLS, MMRS7112, MMRS710A);
		createReference(RelationshipType.CALLS, MMRS71Z3, MMRS710A);
		createReference(RelationshipType.CALLS, MMRS71D1, MMRS710A);
		createReference(RelationshipType.CALLS, MMRS71Z2, MMRS710A);
		createReference(RelationshipType.CALLS, MMRS7101, MMRS710A);
		createReference(RelationshipType.CALLS, MMRS7102, MMRS710A);
		taxonomyService.createModuleLink(MMRS7112.getUid(), taxonomyA);
		
		final List<String> moduleIds = Collections.singletonList(String.valueOf(MMRS7112.getUid()));
		final List<String> taxonomyIds = Collections.singletonList(String.valueOf(taxonomyA.getUid()));

		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds, CALLS, CALLS,
								Collections.emptyList())));
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, getPropagationData(jobId)));
		
		final List<TaxonomyPojo> taxonomyListForMMRS7112 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS7112));
		assertEquals(1, taxonomyListForMMRS7112.size());
		assertEquals(taxMmrs, taxonomyListForMMRS7112.get(0).getName());
		
		final List<TaxonomyPojo> taxonomyListForMMRS710A = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS710A));
		assertEquals(1, taxonomyListForMMRS710A.size());
		assertEquals(taxMmrs, taxonomyListForMMRS710A.get(0).getName());
		
		final List<TaxonomyPojo> taxonomyListFoMMRS71Z3 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS71Z3));
		assertEquals(1, taxonomyListFoMMRS71Z3.size());
		assertEquals(taxMmrs, taxonomyListFoMMRS71Z3.get(0).getName());
		
		final List<TaxonomyPojo> taxonomyListForMMRS71D1 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS71D1));
		assertEquals(1, taxonomyListForMMRS71D1.size());
		assertEquals(taxMmrs, taxonomyListForMMRS71D1.get(0).getName());
		
		final List<TaxonomyPojo> taxonomyListForMMRS71Z2 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS71Z2));
		assertEquals(1, taxonomyListForMMRS71Z2.size());
		assertEquals(taxMmrs, taxonomyListForMMRS71Z2.get(0).getName());
		
		final List<TaxonomyPojo> taxonomyListForMMRS7101 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS7101));
		assertEquals(1, taxonomyListForMMRS7101.size());
		assertEquals(taxMmrs, taxonomyListForMMRS7101.get(0).getName());
		
		final List<TaxonomyPojo> taxonomyListForMMRS7102 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS7102));
		assertEquals(1, taxonomyListForMMRS7102.size());
		assertEquals(taxMmrs, taxonomyListForMMRS7102.get(0).getName());
	}
	
	@Test
	void testTaxonomyPropagationModulesAlreadyAssignedTaxonomy() {
		/*
		 * Test data for taxonomy propagation to modules already assigned taxonomy.
		 * 
		 * Entry Point -->										MMRS000 (Taxonomy Batch)
		 * 																 |	|
		 * 																 |	|
		 * Propagation Level1	 MMRS001 (Taxonomy Batch propagated)<----+	+----> Module MMRS00A (Taxonomy Library)
		 */
		final var MMRS000 = createModule(projectId, "MMRS000", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var MMRS00A = createModule(projectId, "MMRS00A", Technology.COBOL, Type.COPYBOOK, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var MMRS001 = createModule(projectId, "MMRS001", Technology.JCL, Type.UNKNOWN, Storage.SYSTEM, Identification.IDENTIFIED, Origin.CUSTOM);
		
		final String taxBatch = "Batch";
		final String taxLibrary = "Library";
		
		final EntityId taxonomyBatch = createTaxonomy(type, taxBatch);
		final EntityId taxonomyLibrary = createTaxonomy(type, taxLibrary);
		
		createReference(RelationshipType.CALLS, MMRS001, MMRS000);
		createReference(RelationshipType.INCLUDES, MMRS000, MMRS00A);
		
		taxonomyService.createModuleLink(MMRS000.getUid(), taxonomyBatch);
		taxonomyService.createModuleLink(MMRS00A.getUid(), taxonomyLibrary);
		
		final List<String> moduleIds = Collections.singletonList(String.valueOf(MMRS000.getUid()));
		final List<String> taxonomyIds = Collections.singletonList(String.valueOf(taxonomyBatch.getUid()));

		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds, CALLS,
								INCLUDES, Collections.emptyList())));
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, getPropagationData(jobId)));
		
		final List<TaxonomyPojo> taxonomyListForMMRS000 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS000));
		assertEquals(1, taxonomyListForMMRS000.size());
		assertEquals(taxBatch, taxonomyListForMMRS000.get(0).getName());
		
		final List<TaxonomyPojo> taxonomyListForMMRS001 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS001));
		assertEquals(1, taxonomyListForMMRS001.size());
		assertEquals(taxBatch, taxonomyListForMMRS001.get(0).getName());
		
		/* Module is already assigned taxonomy , propagation should happen for taxonomy which are not already propagated */
		final List<TaxonomyPojo> taxonomyListForMMRS00A = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS00A));
		assertEquals(2, taxonomyListForMMRS00A.size());
		Collections.sort(taxonomyListForMMRS00A, (t1, t2) -> t1.getName().compareTo(t2.getName()));
		assertEquals(taxBatch, taxonomyListForMMRS00A.get(0).getName());
		assertEquals(taxLibrary, taxonomyListForMMRS00A.get(1).getName());
	}
	
	@Test
	void testTaxonomyPropagationSelectedTaxonomyOnly() {
		/*
		 * Test data for taxonomy propagation to modules already assigned taxonomy.
		 * 
		 * Entry Point -->									MMRS00B (Taxonomy Database, UI)
		 * 															|
		 * 															|
		 * Propagation Level1	 									+----> Module MMRS00J (Taxonomy Database Propagated)
		 */
		final var MMRS00B = createModule(projectId, "MMRS00B", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var MMRS00J = createModule(projectId, "MMRS00J", Technology.JCL, Type.UNKNOWN, Storage.SYSTEM, Identification.IDENTIFIED, Origin.CUSTOM);
		
		final String taxDb = "Database";
		final String taxUi = "UI";
		
		final EntityId taxonomyDb = createTaxonomy(type, taxDb);
		final EntityId taxonomyUi = createTaxonomy(type, taxUi);
		
		createReference(RelationshipType.CALLS, MMRS00J, MMRS00B);
		
		taxonomyService.createModuleLinks(MMRS00B, List.of(taxonomyDb, taxonomyUi));
		
		final List<String> moduleIds = Collections.singletonList(String.valueOf(MMRS00B.getUid()));
		final List<String> taxonomyIds = Collections.singletonList(String.valueOf(taxonomyDb.getUid()));

		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds, CALLS, CALLS,
								Collections.emptyList())));
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, getPropagationData(jobId)));
		
		final List<String> taxonomyListForMMRS00B = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS00B))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForMMRS00B, Matchers.containsInAnyOrder(taxDb, taxUi));

		/* TaxonomyPojo propagation should happen only for selected taxonomy*/
		final List<String> taxonomyListForMMRS00J = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS00J))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForMMRS00J, Matchers.containsInAnyOrder(taxDb));
	}
	
	@Test
	void testTaxonomyPropagationForVirtualModules() {
		/*
		 * Test data for taxonomy propagation to modules already assigned taxonomy.
		 * 
		 * Entry Point -->									MMRS00P (Taxonomy Database, UI)
		 * 															|
		 * 															|
		 * Propagation Level1	 									+----> Module MMRS00V (Taxonomy Database Propagated) 
		 */
		/* Create a Module MMRS00P and a Virtual Module MMRS00V */
		final var MMRS00P = createModuleWithPath(projectId, "MMRS00P", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED,
				Origin.CUSTOM, "src/cobol/programs/MMRS00P.cbl", Representation.PHYSICAL);
		final var MMRS00V = createModuleWithPath(projectId, "MMRS00V", Technology.JCL, Type.EXEC_PGM, Storage.SYSTEM, Identification.IDENTIFIED,
				Origin.CUSTOM, "src/cobol/programs/MMRS00P.cbl", Representation.VIRTUAL);
		
		final String taxDb = "DB";
		final String taxFile = "File";
		
		final EntityId taxonomyDb = createTaxonomy(type, taxDb);
		final EntityId taxonomyUi = createTaxonomy(type, taxFile);

		createReference(RelationshipType.INCLUDES, MMRS00P, MMRS00V);
		createReference(RelationshipType.REFERENCES, MMRS00P, MMRS00V);
		
		taxonomyService.createModuleLink(MMRS00P.getUid(), taxonomyDb);
		taxonomyService.createModuleLink(MMRS00P.getUid(), taxonomyUi);
		
		final List<String> moduleIds = Collections.singletonList(String.valueOf(MMRS00P.getUid()));
		final List<String> taxonomyIds = Collections.singletonList(String.valueOf(taxonomyDb.getUid()));

		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds,
				Collections.emptyList(), REFERENCES, Collections.emptyList())));
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, getPropagationData(jobId)));
		
		final List<String> taxonomyListForMMRS00P = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS00P))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForMMRS00P, Matchers.containsInAnyOrder(taxDb, taxFile));
		
		/* Taxonomy propagation should also happen for Virtual module for selected taxonomy */
		final List<String> taxonomyListForMMRS00V = taxonomyService.find(q -> q.ofProject(projectId).ofModule(MMRS00V))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForMMRS00V, Matchers.containsInAnyOrder(taxDb));
	}
	
	@Test
	void testTaxonomyPropagationWithTaxonomyMonitor() {
		final var moduleA = createModule(projectId, "MODULE M1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleA1 = createModule(projectId, "MODULE M2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleA2 = createModule(projectId, "MODULE M3", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		createModule(projectId, "MODULE M4", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		createModule(projectId, "MODULE M5", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		createModule(projectId, "MODULE M6", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		createModule(projectId, "MODULE M7", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		createModule(projectId, "MODULE M8", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		createModule(projectId, "MODULE M9", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final String taxonomy = "TaxonomyMonitor";
		final EntityId taxonomyA = createTaxonomy(type, taxonomy);

		createReference(RelationshipType.CALLS, moduleA, moduleA1);
		createReference(RelationshipType.CALLS, moduleA1, moduleA2);
		taxonomyService.createModuleLink(moduleA.getUid(), taxonomyA);
		taxonomyService.createModuleLink(moduleA2.getUid(), taxonomyA);
		final int unitToWork = (int) moduleService.countModules(b -> b.ofProject(projectId)) - 2;

		final List<String> moduleIds = asList(String.valueOf(moduleA.getUid()), String.valueOf(moduleA2.getUid()));
		final List<String> taxonomyIds = Collections.singletonList(String.valueOf(taxonomyA.getUid()));

		final String taxonomyPrePropagationJobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds,
				taxonomyIds, CALLS, CALLS, Collections.emptyList())));
		final JobInformation taxonomyPrePropagationJobInfo = getJobInformation(taxonomyPrePropagationJobId);
		assertNotNull(taxonomyPrePropagationJobInfo);
		assertEquals(unitToWork, taxonomyPrePropagationJobInfo.getTotalWorkUnits());
		assertEquals(unitToWork, taxonomyPrePropagationJobInfo.getProcessedWorkUnits());
		
		final List<PropagationData> propagationData = getPropagationData(taxonomyPrePropagationJobId);
		final String taxonomyPropagationJobId = submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, propagationData));

		final JobInformation taxonomyPropagationJobInfo = getJobInformation(taxonomyPropagationJobId);
		assertNotNull(taxonomyPropagationJobInfo);
		assertEquals(propagationData.size(), taxonomyPropagationJobInfo.getTotalWorkUnits());
		assertEquals(propagationData.size(), taxonomyPropagationJobInfo.getProcessedWorkUnits());
	}
	
	@Test
	void testTaxonomyPropagationWithSelectedModules() {
		/*
		 * Test data for multiple assignments.
		 * 
		 * Entry Point --> 		Module B1 (Taxonomy 1)							Module B2 (Taxonomy 2)
		 * 						|													|
		 * 						|													|
		 * Propagation Level1	+---> Module B3 (Taxonomy 1 and 2 propagated)<------+
		 * 									|
		 * 									|
		 * Propagation Level2				+---> Module B4 (Taxonomy 1 and 2 propagated) 
		 */
		final var moduleB1 = createModule(projectId, "Module T1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleB2 = createModule(projectId, "Module T2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleB3 = createModule(projectId, "Module T3", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleB4 = createModule(projectId, "Module T4", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);

		final String tax1 = "Taxonomy tax1";
		final String tax2 = "Taxonomy tax2";
		final EntityId taxonomy1 = createTaxonomy(type, tax1);
		final EntityId taxonomy2 = createTaxonomy(type, tax2);

		createReference(RelationshipType.CALLS, moduleB1, moduleB3);
		createReference(RelationshipType.CALLS, moduleB2, moduleB3);
		createReference(RelationshipType.CALLS, moduleB3, moduleB4);
		taxonomyService.createModuleLink(moduleB1.getUid(), taxonomy1);
		taxonomyService.createModuleLink(moduleB2.getUid(), taxonomy2);

		final List<String> moduleIds = asList(String.valueOf(moduleB1.getUid()), String.valueOf(moduleB2.getUid()));
		final List<String> taxonomyIds = asList(String.valueOf(taxonomy1.getUid()), String.valueOf(taxonomy2.getUid()));

		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds, CALLS, CALLS,
				Collections.emptyList())));
		
		/* selecting only moduleB3 and moduleB4 for taxonomy assignment */
		final List<PropagationData> propagationData = getPropagationData(jobId).stream()
				.filter(data -> data.getModuleId().equals(moduleB3.getNid()) || data.getModuleId().equals(moduleB4.getNid()))
				.collect(Collectors.toList());
		
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, propagationData));
		
		final List<TaxonomyPojo> taxonomyListForModuleB1 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleB1));
		assertEquals(1, taxonomyListForModuleB1.size());
		assertEquals(tax1, taxonomyListForModuleB1.get(0).getName());

		final List<TaxonomyPojo> taxonomyListForModuleB2 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleB2));
		assertEquals(1, taxonomyListForModuleB2.size());
		assertEquals(tax2, taxonomyListForModuleB2.get(0).getName());

		final List<String> taxonomyListForModuleB3 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleB3))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleB3, Matchers.containsInAnyOrder(tax1, tax2));

		final List<String> taxonomyListForModuleB4 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleB4))
				.stream().map(TaxonomyPojo::getName).collect(Collectors.toList());
		assertThat(taxonomyListForModuleB4, Matchers.containsInAnyOrder(tax1, tax2));
	}
	
	@Test
	void testTaxonomyPropagationDoesNotReturnAlreadyAssignedTaxonomies() {
		/*
		 * Test taxonomy propagation does not Return already assigned taxonomy data.
		 * 
		 * Entry Point --> 		Module M1 (Taxonomy M1)
		 * 						|
		 * 						|
		 * Propagation Level1	+---> Module M2 (Taxonomy M1)
		 */
		final var moduleM1 = createModule(projectId, "Module M1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleM2 = createModule(projectId, "Module M2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);

		final EntityId taxonomy1 = createTaxonomy(type, "Taxonomy M1");
		taxonomyService.createModuleLink(moduleM1.getUid(), taxonomy1);
		createReference(RelationshipType.CALLS, moduleM2, moduleM1);
		
		final List<String> moduleIds = Collections.singletonList(String.valueOf(moduleM1.getUid()));
		final List<String> taxonomyIds = Collections.singletonList(String.valueOf(taxonomy1.getUid()));

		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds, CALLS, CALLS,
				Collections.emptyList())));

		final List<PropagationData> propagations = getPropagationData(jobId);
		final List<Long> propagationModuleIds = propagations
				.stream().map(PropagationData::getModuleId).collect(Collectors.toList());
		assertThat(propagationModuleIds, Matchers.containsInAnyOrder(moduleM2.getNid()));

		final List<EntityId> propagationTaxonomyIds = propagations.stream()
				.map(PropagationData::getTaxonomies)
				.flatMap(Set::stream)
				.collect(Collectors.toList());
		assertThat(propagationTaxonomyIds, Matchers.containsInAnyOrder(taxonomy1));
	}
	
	@Test
	void testTaxonomyPropagationWithReadWriteAccessTypes() {
		final EntityId moduleA = createModule(projectId, "MODULE AC", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final EntityId moduleA1 = createModule(projectId, "MODULE AC1", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		final EntityId moduleA2 = createModule(projectId, "MODULE AC2", Technology.COBOL, Type.PROGRAM, Storage.FILE, Identification.IDENTIFIED, Origin.CUSTOM);
		
		final String taxonomy = "Taxonomy AC";
		final EntityId taxonomyA = createTaxonomy(type, taxonomy);
		
		final String taxonomy1 = "Taxonomy AC1";
		final EntityId taxonomyB = createTaxonomy(type, taxonomy1);

		createReference(RelationshipType.ACCESSES, moduleA, moduleA1,
				Collections.singletonMap(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), DatabaseAccessType.READ.name()));
		
		createReference(RelationshipType.ACCESSES, moduleA, moduleA2,
				Collections.singletonMap(DB_ACCESS_TYPES.getReferenceAttributeExcelName(), DatabaseAccessType.STORE.name()));
		
		createReference(RelationshipType.CALLS, moduleA2, moduleA);
		taxonomyService.createModuleLink(moduleA2.getUid(), taxonomyB);
		taxonomyService.createModuleLink(moduleA.getUid(), taxonomyA);

		final List<String> moduleIds = Collections.singletonList(String.valueOf(moduleA.getUid()));
		final List<String> taxonomyIds = Collections.singletonList(String.valueOf(taxonomyA.getUid()));

		final String jobId = submitJobFortaxonomyPropagation(taxonomyPropagationModuleIdentificationJob(createParameters(moduleIds, taxonomyIds, ACCESSES,
				ACCESSES, Collections.singletonList(DatabaseAccessType.READ))));
		submitJobFortaxonomyPropagation(taxonomyPropagationJob(projectId, getPropagationData(jobId)));

		final List<TaxonomyPojo> taxonomyListForModuleA = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleA));
		assertEquals(1, taxonomyListForModuleA.size());
		assertEquals(taxonomy, taxonomyListForModuleA.get(0).getName());

		final List<TaxonomyPojo> taxonomyListForModuleA1 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleA1));
		assertEquals(1, taxonomyListForModuleA1.size());
		assertEquals(taxonomy, taxonomyListForModuleA1.get(0).getName());
		
		final List<TaxonomyPojo> taxonomyListForModuleA2 = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleA2));
		assertEquals(2, taxonomyListForModuleA2.size());
	}
	
	private EntityId createModule(final EntityId projectId, final String moduleName, final Technology technology, final Type type, final Storage storage,
			final Identification identification, final Origin origin) {
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
	
	private EntityId createModuleWithPath(final EntityId projectId, final String moduleName, final Technology technology, final Type type,
			final Storage storage, final Identification identification, final Origin origin, final String path, final Representation representation) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setName(moduleName);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(storage);
		module.setIdentification(identification);
		module.setOrigin(origin);
		module.setRepresentation(representation);
		module.setCreator(Creator.DISCOVERY);
		if (Representation.PHYSICAL == representation) {
			module.setPath(path);
		} else {
			module.setParentPath(path);
		}
		return moduleService.create(module);
	}

	private EntityId createTaxonomy(final TaxonomyTypePojo taxonomyType, final String taxonomyName) {
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype();
		taxonomy.setName(taxonomyName);
		taxonomy.setProject(projectId);
		taxonomy.setType(taxonomyType.getId());
		return taxonomyService.create(taxonomy);
	}

	private UUID createReference(final RelationshipType relationship, final EntityId incomingIndex, final EntityId outgoingIndex) {
		return createReference(relationship, incomingIndex, outgoingIndex, null);
	}

	private UUID createReference(final RelationshipType relationship, final EntityId incomingIndex, final EntityId outgoingIndex,
			@Nullable Map<String, Object> properties) {
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype()
				.setRelationship(relationship)
				.setSrcModule(incomingIndex)
				.setDstModule(outgoingIndex);
		
		if (properties != null) {
			reference.properties.set(properties);
		}
		
		return moduleService.createRelationship(reference);
	}

	private ProjectPojo createProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Test Project")
				.setClient(EntityId.of(1L))
				.setNatures(new HashSet<>(Arrays.asList(ProjectNature.DISCOVERY))));
	}

	private Map<String, List<String>> createParameters(final List<String> moduleIds, final List<String> taxonomyIds, final List<RelationshipType> inReferences,
			final List<RelationshipType> outReferences, final List<DatabaseAccessType> readsWritesAccesses) {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put("moduleIds", moduleIds);
		parameters.put("taxonomyIds", taxonomyIds);
		parameters.put("incomingReferences", inReferences.stream().map(RelationshipType::name).collect(Collectors.toList()));
		parameters.put("outgoingReferences", outReferences.stream().map(RelationshipType::name).collect(Collectors.toList()));
		parameters.put("readsWritesAccesses", readsWritesAccesses.stream().map(DatabaseAccessType::name).collect(Collectors.toList()));
		return parameters;
	}
	
	private String submitJobFortaxonomyPropagation(final Job<?> job) {
		return submitJob(jobManager, tracer, job);
	}
	
	@Nullable
	private JobInformation getJobInformation(final String jobId) {
		final List<JobInformation> jobInfos = jobManager.getJobs(q -> q.byId(UUID.fromString(jobId)));
		return jobInfos.isEmpty() ? null : jobInfos.get(0);
	}

	private TaxonomyPropagationModuleIdentificationJob taxonomyPropagationModuleIdentificationJob(final Map<String, List<String>> parameters) {
		final List<EntityId> taxonomyIds = Optional.ofNullable(parameters.get("taxonomyIds"))
													.map(list -> list.stream().map(EntityId::of).collect(Collectors.toList()))
													.orElse(Collections.emptyList());
		final List<RelationshipType> incomings = Optional.ofNullable(parameters.get("incomingReferences")).orElse(Collections.emptyList()).stream()
													.map(RelationshipType::from)
													.collect(Collectors.toList());
		final List<RelationshipType> outgoings = Optional.ofNullable(parameters.get("outgoingReferences")).orElse(Collections.emptyList()).stream()
													.map(RelationshipType::from)
													.collect(Collectors.toList());
		final List<DatabaseAccessType> accesses = Optional.ofNullable(parameters.get("readsWritesAccesses")).orElse(Collections.emptyList()).stream()
													.map(DatabaseAccessType::valueOf)
													.collect(Collectors.toList());
		final List<EntityId> moduleIds = parameters.get("moduleIds").stream().map(EntityId::of).collect(Collectors.toList());
		final TaxonomyPropagationRequest taxonomyPropagationRequest = new TaxonomyPropagationRequest(moduleIds, taxonomyIds, incomings, outgoings, accesses);
		return new TaxonomyPropagationModuleIdentificationJob(projectId, taxonomyPropagationRequest);
	}
	
	private TaxonomyPropagationJob taxonomyPropagationJob(final EntityId projectId, final List<PropagationData> propagationData) {
		return new TaxonomyPropagationJob(projectId, propagationData);
	}
	
	@SuppressWarnings("unchecked")
	private List<PropagationData> getPropagationData(final String jobId) {
		final Result<Serializable> result = (Result<Serializable>) jobManager.getJobResult(jobId);
		assertNotNull("Job result must not be null", result);
		return (List<PropagationData>) result.value;
	}
}
