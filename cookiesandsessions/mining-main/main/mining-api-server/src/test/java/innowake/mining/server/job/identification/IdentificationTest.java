/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.javatuples.Triplet;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.core.annotation.AnnotationCandidates;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.job.BulkAnnotationsUpdateJob;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationMetaDataReasonEnum;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.AnnotationUpdateType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Identification job tests.
 */
@WithMockUser
class IdentificationTest extends AbstractIdentificationTest {
	
	private static final String TEST_CATEGORY = "Test Category";

	@Autowired
	private AnnotationService annotationService;

	@Autowired
	private JobManager jobManager;

	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private MiningDataCoreService core;

	private static final String VALIDATION_RULE_NAME = "Data Validation Rule Candidate [System identified]";
	private static final String BUSINESS_RULE_NAME = "Business Rule Candidate [System identified]";


	/**
	 * Creates the test data as following:
	 * <ul>
	 * <li>Creates a new project</li>
	 * <li>Creates new {@link Module Modules} in the new project</li>
	 * </ul>
	 */
	@BeforeAll
	void init() {
		createCobolProgram(PROJECT_ID_1, "BrBranch", "BrBranch.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "EXECSQL", "EXECSQL.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7114", "MMRS7114.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "NESTEDDB", "NESTEDDB.cpy", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "DBSTATEMENT2", "DBSTATEMENT2.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7115", "MMRS7115.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7116", "MMRS7116.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7117", "MMRS7117.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "OVERLAP", "OVERLAP.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7118", "MMRS7118.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7119", "MMRS7119.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "OVERLAP2", "OVERLAP2.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7120", "MMRS7120.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "DIFTYPE", "DIFTYPE.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "WMIN336A", "WMIN336A.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS71Z1", "MMRS71Z1.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "AE621ISO", "AE621ISO.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "COBOL_UNISYS_REPLACING", "cobol_unisys_REPLACING.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "COBOL_UNISYS_COPYBOOK1", "cobol_unisys_Copybook1.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7111", "MMRS7111.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7112", "MMRS7112.cbl", RESOURCE_PATH);
	}

	/**
	 * Tests the business rule candidate identification without exclude annotations for excluding BR candidates.
	 */
	@Test
	void identifyBusinessRuleCandidates() {
		final String path = RESOURCE_PATH + "BrBranch.cbl";
		final var module = moduleService.findAnyModule(q -> q.ofProject(PROJECT_ID_1).withPath(path))
											.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for path in project 1: " + path));
		moduleService.deleteModule(module.identity(), true);
		createCobolProgram(PROJECT_ID_1, "BrBranch", "BrBranch.cbl", RESOURCE_PATH);
		
		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue("There must be no annotations present before the candidate identification job ran", annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<Triplet<ModuleLocation, String, AnnotationType>> brCandidateLocations = new ArrayList<>();
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(521, 68), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(685, 61), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(2292, 113), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(2477, 206), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(2773, 167), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(2954, 158), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(802, 86), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(960, 274), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(1344, 250), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(1691, 174), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(1956, 159), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(3126, 148), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(3352, 201), VALIDATION_RULE_NAME, AnnotationType.RULE));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertEquals(brCandidateLocations.size(), annotationsAfter.size(), "Number of created BR candidate annotations must match");
		
		compareResults(annotationsAfter, brCandidateLocations);
	}

	private void compareResults(final List<AnnotationPojo> annotationsAfter,
			final List<Triplet<ModuleLocation,String,AnnotationType>> brCandidateLocations) {
		final List<Triplet<ModuleLocation,String,AnnotationType>> actualTriplets = annotationsAfter.stream()
				.map(a -> new Triplet<>(a.getLocation().orElse(null), a.getName(), a.getType()))
				.collect(Collectors.toList());

		MatcherAssert.assertThat(actualTriplets, Matchers.containsInAnyOrder(brCandidateLocations.toArray()));
	}

	/**
	 * Tests the business rule candidate identification with exclude annotations for excluding BR candidates.
	 */
	@Test
	void identifyBusinessRuleCandidatesImprovedWithExcludes() {
		final ModulePojo module = findByPath("BrBranch.cbl");
		assertNotNull("Module BrBranch must not be null", module);
		moduleService.deleteModule(module.identity(), true);
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "BrBranch", "BrBranch.cbl", RESOURCE_PATH);

		final Set<ModuleLocation> excludeLocations = new HashSet<>();
		/* Line 15 - 18 (including) */
		excludeLocations.add(new ModuleLocation(390, 102));
		/* Line 21 - 24 (including) */
		excludeLocations.add(new ModuleLocation(554, 101));
		/* Line 38 - 43 (including) */
		excludeLocations.add(new ModuleLocation(1045, 178));

		createExcludeAnnotations(moduleId, excludeLocations);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofModule(moduleId));
		assertEquals(excludeLocations.size(), annotationsBefore.size(), "Only EXCLUDE annotations must be present");

		submitIdentifyCandidatesJob(PROJECT_ID_1, moduleId);

		final List<Triplet<ModuleLocation, String, AnnotationType>> brCandidateLocations = new ArrayList<>();
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(390, 102), "Exclude at: 390, length: 102", AnnotationType.EXCLUDE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(1045, 178), "Exclude at: 1045, length: 178", AnnotationType.EXCLUDE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(554, 101), "Exclude at: 554, length: 101", AnnotationType.EXCLUDE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(685, 61), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(2292, 113), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(2477, 206), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(2773, 167), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(2954, 158), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(802, 86), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(1344, 250), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(1691, 174), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(1956, 159), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(3126, 148), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(new Triplet<>(new ModuleLocation(3352, 201), VALIDATION_RULE_NAME, AnnotationType.RULE));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofModule(moduleId));
		compareResults(annotationsAfter, brCandidateLocations);
	}

	@Test
	void testDatabaseAnnotationIdentificationTest() {
		final ModulePojo module = findByPath("EXECSQL.cbl");
		assertNotNull("Module EXECSQL must not be null", module);
		createAnnotation(module.identity(), "Database Annotation 1", 7, 14, WorkingState.CANDIDATE, AnnotationType.DATABASE, "IDENTIFICATION", null);
		createAnnotation(module.identity(), "Database Annotation 2", 677, 7, WorkingState.CANDIDATE, AnnotationType.DATABASE, "GOBACK.", null);
		
		assertEquals(2, annotationService.find(q -> q.ofModule(module.identity())).size());
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "EXECSQL.cbl");
		assertEquals(4, annotationService.find(q -> q.ofModule(module.identity())).size(), "2 more annotations should be identified");
	}

	@Test
	void testIdentifyDatabaseAnnotationsInCopybooks() {
		final ModulePojo module1 = findByPath("MMRS7114.cbl");
		assertNotNull("Module MMRS7114 must not be null", module1);
		final ModulePojo module2 = findByPath("NESTEDDB.cpy");
		assertNotNull("Module NESTEDDB must not be null", module2);
		final ModulePojo module3 = findByPath("DBSTATEMENT2.cpy");
		assertNotNull("Module DBSTATEMENT2 must not be null", module3);		
		
		createReference(RelationshipType.INCLUDES, module1.identity(), module2.identity());
		createReference(RelationshipType.INCLUDES, module2.identity(), module3.identity());
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, module1.identity());

		assertEquals(0L, annotationService.find(q -> q.ofModule(module1.identity())).size(), "Annotation count for MMRS7114 should be 0");
		assertEquals(1L, annotationService.find(q -> q.ofModule(module2.identity())).size(), "Annotation count for NESTEDDB should be 1");
		assertEquals(1L, annotationService.find(q -> q.ofModule(module3.identity())).size(), "Annotation count for DBSTATEMENT2 should be 1");
	}

	@Test
	void testIdentifyOnlyOneDatabaseAnnotationPerLocationAndTypeAllowed() {
		annotationService.delete(q -> q.ofProject(PROJECT_ID_1));

		final ModulePojo module1 = findByPath("MMRS7114.cbl");
		assertNotNull("Module MMRS7114 must not be null", module1);
		final ModulePojo module2 = findByPath("NESTEDDB.cpy");
		assertNotNull("Module NESTEDDB must not be null", module2);
		final ModulePojo module3 = findByPath("DBSTATEMENT2.cpy");
		assertNotNull("Module DBSTATEMENT2 must not be null", module3);	
		
		createReference(RelationshipType.INCLUDES, module1.identity(), module2.identity());
		createReference(RelationshipType.INCLUDES, module2.identity(), module3.identity());
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7114.cbl");
		assertEquals(2, annotationService.find(q -> q.ofProject(PROJECT_ID_1)).size(), "Only 2 annotations should be identified");
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7114.cbl");
		final List<AnnotationPojo> annotationsInDb = annotationService.find(q -> q.ofProject(PROJECT_ID_1));
		
		assertEquals(2, annotationsInDb.size(), "Number of annotations in DB should not change after multiple identification");
		assertEquals(0L, getCandidateAnnotationCountForModule("MMRS7114"), "Annotation count for \"MMRS7114\" should be 0");
		assertEquals(1L, getCandidateAnnotationCountForModule("NESTEDDB"), "Annotation count for \"NESTEDDB\" should be 1");
		assertEquals(1L, getCandidateAnnotationCountForModule("DBSTATEMENT2"), "Annotation count for \"DBSTATEMENT2\" should be 1");
	}

	@Test
	void testIdentifyOverlappingAnnotationsAreAllowed() {
		annotationService.delete(q -> q.ofProject(PROJECT_ID_1));

		final ModulePojo module1 = findByPath("MMRS7115.cbl");
		assertNotNull("Module MMRS7115 must not be null", module1);
		final ModulePojo module2 = findByPath("MMRS7116.cbl");
		assertNotNull("Module MMRS7116 must not be null", module2);
		final ModulePojo module3 = findByPath("MMRS7117.cbl");
		assertNotNull("Module MMRS7117 must not be null", module3);
		final ModulePojo module4 = findByPath("OVERLAP.cpy");
		assertNotNull("Module OVERLAP must not be null", module4);
		
		createReference(RelationshipType.INCLUDES, module1.identity(), module4.identity());
		createReference(RelationshipType.INCLUDES, module2.identity(), module4.identity());
		createReference(RelationshipType.INCLUDES, module3.identity(), module4.identity());
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, module1.identity());
		assertEquals(1, annotationService.find(q -> q.ofModule(module4.identity())).size(), "Only 1 annotation should be identified");
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7116.cbl");
		assertEquals(1, annotationService.find(q -> q.ofModule(module4.identity())).size(), "Only 1 annotations should be identified");
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, module3.identity());
		final List<AnnotationPojo> annotationsInDb = annotationService.find(q -> module4.identity());
		
		assertEquals(1, annotationsInDb.size(), "Only 1 annotation should be identified");
		assertEquals(0L, getCandidateAnnotationCountForModule("MMRS7115"), "Annotation count for \"MMRS7115\" should be 0");
		assertEquals(0L, getCandidateAnnotationCountForModule("MMRS7116"), "Annotation count for \"MMRS7116\" should be 0");
		assertEquals(0L, getCandidateAnnotationCountForModule("MMRS7117"), "Annotation count for \"MMRS7117\" should be 0");
		assertEquals(1L, getCandidateAnnotationCountForModule("OVERLAP"), "Annotation count for \"OVERLAP\" should be 1");
	}

	@Test
	void testIdentifyOverlappingAnnotationsAreAllowedWithLongerOffsets() {
		final ModulePojo module1 = findByPath("MMRS7118.cbl");
		assertNotNull("Module MMRS7118 must not be null", module1);
		final ModulePojo module2 = findByPath("MMRS7119.cbl");
		assertNotNull("Module MMRS7119 must not be null", module2);
		final ModulePojo module3 = findByPath("OVERLAP2.cpy");
		assertNotNull("Module OVERLAP2 must not be null", module3);
		createReference(RelationshipType.INCLUDES, module1.identity(), module3.identity());
		createReference(RelationshipType.INCLUDES, module2.identity(), module3.identity());
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7118.cbl");
		assertEquals(1, annotationService.find(q -> q.ofModule(module3.identity())).size(), "Only 1 annotation should be identified");
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, module2.identity());

		assertEquals(0, annotationService.find(q -> q.ofModule(module1.identity())).size(), "Annotation count for \"MMRS7118\" should be 0");
		assertEquals(0, annotationService.find(q -> q.ofModule(module2.identity())).size(), "Annotation count for \"MMRS7119\" should be 0");
		assertEquals(1, annotationService.find(q -> q.ofModule(module3.identity())).size(), "Annotation count for \"OVERLAP2\" should be 1");
	}

	@Test
	void testIdentifyAnnotationsWithSameLocationButDifferentTypes() {
		final ModulePojo module1 = findByPath("MMRS7120.cbl");
		assertNotNull("Module MMRS7120 must not be null", module1);
		final ModulePojo module2 = findByPath("DIFTYPE.cpy");
		assertNotNull("Module DIFTYPE must not be null", module2);

		final String moduleAPath = RESOURCE_PATH + "MMRS7120.cbl";
		final String copybookPath = RESOURCE_PATH + "DIFTYPE.cpy";
		createAnnotation(module2.identity(), "Rule annotation 1", 11, 57, WorkingState.CANDIDATE, AnnotationType.RULE, "test", null);
		createReference(RelationshipType.INCLUDES, module1.identity(), module2.identity());
		
		assertEquals(1, annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(copybookPath)).size());
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7120.cbl");
		assertEquals(2, annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(copybookPath)).size(), "2 annotations should exist");
		assertEquals(0, annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(moduleAPath)).size(), "No annotation should have been identified");
	}

	@Test
	void testIdentifyRuleAnnotationIdentificationTest() {
		final ModulePojo module = findByPath("WMIN336A.cbl");
		assertNotNull("Module WMIN336A must not be null", module);
		submitIdentifyCandidatesJob(PROJECT_ID_1, module.identity());
		final var annotationPojos = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(RESOURCE_PATH + "WMIN336A.cbl"));
		final List<List<?>> actualAnnotations = annotationPojos.stream()
				.map(a -> Arrays.asList(a.getName(),
						a.getModuleName(),
						a.getCategoryName().orElse(null),
						a.getLocation().map(ModuleLocation::getOffset).orElse(null)
				))
				.collect(Collectors.toList());

		MatcherAssert.assertThat(actualAnnotations, Matchers.containsInAnyOrder(
				List.of("Business Rule Candidate [System identified]", "WMIN336A", "Business Rule", 10359),
				List.of("Field Computation Rule Candidate [System identified]", "WMIN336A", "Field Computation Rule", 7873),
				List.of("Field Computation Rule Candidate [System identified]", "WMIN336A", "Field Computation Rule", 9114),
				List.of("Technical Rule Candidate [System identified]", "WMIN336A", "Technical Rule", 7659),
				List.of("Technical Rule Candidate [System identified]", "WMIN336A", "Technical Rule", 8900),
				List.of("Data Validation Rule Candidate [System identified]", "WMIN336A", "Validation Rule", 5902),
				List.of("Data Validation Rule Candidate [System identified]", "WMIN336A", "Validation Rule", 8019),
				List.of("Data Validation Rule Candidate [System identified]", "WMIN336A", "Validation Rule", 9187),
				List.of("Data Validation Rule Candidate [System identified]", "WMIN336A", "Validation Rule", 12840),
				List.of("Data Validation Rule Candidate [System identified]", "WMIN336A", "Validation Rule", 13789)
		));
	}

	@Test
	void testIdentificationWithBusinessVariable() {
		final ModulePojo module = findByPath("MMRS71Z1.cbl");
		assertNotNull("Module MMRS71Z1 must not be null", module);
		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(module.identity())).size());
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS71Z1.cbl");
		
		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(module.identity()));
		final List<DataDictionaryPojo> businessVariables = dataDictionaries.stream().filter(q -> q.getIsBusiness().orElseThrow()).collect(Collectors.toList());
		assertEquals(27, dataDictionaries.size());
		assertEquals(3, businessVariables.size());
		assertTrue("Should contain MY-HEX-DIGIT, MY-HEX-ZONE, D",
				businessVariables.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList())
						.containsAll(Arrays.asList("MY-HEX-DIGIT", "MY-HEX-ZONE","D")));
	}


	@Test
	void testIdentifyDataDictionaryInCopyBookWhereReferenceNameInCodeHasDifferentCase() {
		final var module1 = moduleService.findAnyModule(q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "COBOL_UNISYS_REPLACING.cbl"))
											.orElseThrow(() -> new MiningEntityNotFoundException("Module COBOL_UNISYS_REPLACING must exist"));
		final var module2 = moduleService.findAnyModule(q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "COBOL_UNISYS_COPYBOOK1.cpy"))
											.orElseThrow(() -> new MiningEntityNotFoundException("Module COBOL_UNISYS_COPYBOOK1 must exist"));
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
					.setRelationship(RelationshipType.INCLUDES)
					.setSrcModule(module1.identity())
					.setDstModule(module2.identity()));
		

		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "cobol_unisys_REPLACING.cbl");
		List<DataDictionaryPojo> ddes = dataDictionaryService.find(q -> q.ofModule(module1.identity()));
		assertEquals(0, ddes.size(), "Main cobol program COBOL_UNISYS_REPLACING, did not have any Data Dictionary identified.");

		ddes = dataDictionaryService.find(q -> q.ofModule(module2.identity()));
		assertEquals(1, ddes.size(), "Copybook cobol program COBOL_UNISYS_COPYBOOK1, Only 1 Data Dictionary should be identified.");
		assertEquals("TESTFIELD", ddes.get(0).getName(), "The Data Dictionary having name 'TESTFIELD' should be identified.");
	}
	
	@Test
	void testAstNodeDDELinking() {
		final var module = moduleService.findAnyModule(q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "AE621ISO.cbl"))
										.orElseThrow(() -> new MiningEntityNotFoundException("Module AE621ISO.cbl must exist"));
		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(module.identity())).size());

		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "AE621ISO.cbl");
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(module.identity()));

		MatcherAssert.assertThat(dataDictionaries.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList()),
				Matchers.containsInAnyOrder("WS-CLOB-COLUMN", "WS-CLOB-COLUMN-LENGTH", "WS-CLOB-COLUMN-DATA"));

		MatcherAssert.assertThat(dataDictionaries.stream()
						.map(DataDictionaryPojo::getGroupPath)
						.map(o -> o.orElse(null))
						.collect(Collectors.toList()),
				Matchers.containsInAnyOrder("WS-CLOB-COLUMN", "WS-CLOB-COLUMN/WS-CLOB-COLUMN-LENGTH", "WS-CLOB-COLUMN/WS-CLOB-COLUMN-DATA"));

		final Map<String, Integer> nameToOffset = dataDictionaries.stream()
				.collect(Collectors.toMap(d -> d.getName(),
						d -> d.getLocation().map(ModuleLocation::getOffset).orElse(null)));

		assertEquals(821, nameToOffset.get("WS-CLOB-COLUMN"));
		assertEquals(817, nameToOffset.get("WS-CLOB-COLUMN-LENGTH"));
		assertEquals(817, nameToOffset.get("WS-CLOB-COLUMN-DATA"));
	}

	@Test
	void testSwitchingBusinessRelatedFromNoToYes() {
		final var module = moduleService.findAnyModule(q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "MMRS7112.cbl"));
		assertTrue("Module data created for MMRS7112 cobol program in the 'init' method should not be null when fetched from database", module.isPresent());

		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofModule(module.get().identity()));
		assertEquals(0, annotations.size());

		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7112.cbl");
		
		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofModule(module.get().identity()));
		final List<AnnotationPojo> nonBuisnessRuleAnnotations = annotationsAfter.stream()
				.filter(annotation -> ! AnnotationCategory.RuleAnnotationCategory.BUSINESS_RULE.getName().equals(annotation.getCategoryName().orElse(null)))
				.collect(Collectors.toList());
		assertEquals(7, nonBuisnessRuleAnnotations.size());
		
		submitJob(jobManager, new BulkAnnotationsUpdateJob(annotationService, PROJECT_ID_1, nonBuisnessRuleAnnotations,
				AnnotationUpdateType.BUSINESS_RELATED_FROM_NO_TO_YES.getType()));
		
		final List<AnnotationPojo> retrievedAnnotation = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(module.get().identity()));
		assertEquals(annotationsAfter.size(), retrievedAnnotation.size());
		
		final List<AnnotationPojo> updatedBusinessRuleAndMetadataAnnotations = retrievedAnnotation.stream()
				.filter(annotation -> AnnotationCategory.RuleAnnotationCategory.BUSINESS_RULE.getName().equals(annotation.getCategoryName().orElse(null))
						&& AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED.name().equals(annotation.getReasons().stream().findFirst().orElseThrow()))
				.collect(Collectors.toList());
		assertEquals(7, updatedBusinessRuleAndMetadataAnnotations.size());
	}

	@Test
	void testSwitchingBusinessRelatedFromYesToNo() {
		final var module = moduleService.findAnyModule(q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "MMRS7111.cbl"));
		assertNotNull("Module data created for MMRS7111 cobol program in the 'init' method should not be null when fetched from database", module);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(module.get().identity()));
				
		assertTrue("annotations before candidate identification must be empty", annotationsBefore.isEmpty());
		
		submitIdentifyCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7111.cbl");
		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofModule(module.get().identity())
																					.withCategory(AnnotationCategory.RuleAnnotationCategory.BUSINESS_RULE.getName()));
		assertEquals(7, annotationsAfter.size());
		
		final AnnotationCategory annotationCategory = createAnnotationCategory(TEST_CATEGORY, PROJECT_ID_1);
		assertEquals(TEST_CATEGORY, annotationCategory.getName());
		
		final List<AnnotationPojo> annotationsWithCategory = createAnnotationWithCustomCategory(annotationsAfter, annotationCategory);
		
		submitJob(jobManager, new BulkAnnotationsUpdateJob(annotationService, PROJECT_ID_1,
				annotationsWithCategory, AnnotationUpdateType.BUSINESS_RELATED_FROM_YES_TO_NO.getType()));
		final List<AnnotationPojo> retrievedAnnotation = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(module.get().identity()));
		final List<AnnotationPojo> nonBusinessRules = retrievedAnnotation.stream()
				.filter(annotation -> ! annotation.getReasons().contains(AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED.name())
						&& annotationCategory.getName().equals(annotation.getCategoryName().orElse(null)))
				.collect(Collectors.toList());
		assertEquals(nonBusinessRules.size(), annotationsAfter.size());
	}

	private void submitIdentifyCandidatesJob(final EntityId projectId, final String path) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Collections.emptyList(), Arrays.asList(path))));
	}

	private void submitIdentifyCandidatesJob(final EntityId projectId, final EntityId moduleId) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
	}

	private void createExcludeAnnotations(final EntityId moduleId, final Set<ModuleLocation> excludeLocations) {
		final var annotationCandidates = excludeLocations.stream()
				.map(loc -> createAnnotation(moduleId, loc.getOffset(), loc.getLength()))
				.collect(Collectors.toList());

		final AnnotationCandidates candidates = new AnnotationCandidates();
		final Long cnt = candidates.store(moduleId, annotationCandidates, core);
		assertEquals(excludeLocations.size(), cnt.intValue(), "Number of created exclude annotations must match");
	}

	private AnnotationPojoTemplate createAnnotation(final EntityId moduleId, final int offset, final int length) {
		final var annotation = new AnnotationPojoTemplate();
		annotation
			.setModule(moduleId)
			.setLocation(new ModuleLocation(offset, length))
			.setState(WorkingState.CANDIDATE)
			.setType(AnnotationType.EXCLUDE)
			.setName("Exclude at: " + offset + ", length: " + length);
		return annotation;
	}

	private AnnotationPojo createAnnotation(final EntityId moduleId, final String name, final int offset, final int length,
			final WorkingState state, final AnnotationType type, String content, @Nullable final AnnotationCategory annotationCategory) {
		final var annotation = new AnnotationPojoPrototype()
		.setModule(moduleId)
		.setLocation(new ModuleLocation(offset, length))
		.setName(name)
		.setState(state)
		.setType(type)
		.setSourceAttachment(new BinaryString(content))
		.setCreatedByUserId("system_user")
		.setUpdatedByUserId("admin");

		if (annotationCategory != null) {
			annotation.setCategoryId(annotationCategory.getId());
		}
		return annotationService.get(annotationService.create(annotation));
	}
	
	private AnnotationCategory createAnnotationCategory(final String name, final EntityId projectId) {
		return annotationService.getCategory(projectId, annotationService.createCategory(projectId, name, Collections.emptyList()));
	}

	private List<AnnotationPojo> createAnnotationWithCustomCategory(final List<AnnotationPojo> annotationsAfter, final AnnotationCategory annotationCategory) {
		final List<AnnotationPojo> annotationsWithCategory = new ArrayList<>();
		for (final AnnotationPojo annotation : annotationsAfter) {
			final AnnotationPojoPrototype annotation1 = new AnnotationPojoPrototype();
			annotation1.withId(annotation.identity());
			annotation1.setCategoryId(annotationCategory.getId());
			annotationsWithCategory.add(AnnotationPojoDummy.build(annotation1));
		}
		return annotationsWithCategory;
	}
	
	private long getCandidateAnnotationCountForModule(final String moduleName) {
		return annotationService.count(q -> q.withState(WorkingState.CANDIDATE).withModuleName(moduleName, true));
	}
	
	private ModulePojo findByPath(final String fileName) {
		return moduleService.findAnyModule(b -> b.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + fileName))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for path: " + RESOURCE_PATH + fileName));
	}
}
