/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.util;

import static innowake.lib.core.lang.Assert.assertFalse;
import static innowake.lib.core.lang.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.branchstatement.BranchStatement;
import innowake.mining.server.job.identification.AbstractIdentificationTest;
import innowake.mining.server.job.identification.IdentifyCandidatesJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.testing.AstNodePojoDummy;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Branch Statement Utility job tests.
 */
@WithMockUser
class BranchStatementUtilityTest extends AbstractIdentificationTest {

	@Autowired
	private JobManager jobManager;

	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private AnnotationService annotationService;

	@Autowired
	private BranchStatementUtility branchStatementUtility;

	@BeforeAll
	void init() {
		createModule(PROJECT_ID_1, "BRE2_IFELSE", "BRE2_IFELSE.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createModule(PROJECT_ID_1, "BRE2_NESTEDIFELSE", "BRE2_NESTEDIFELSE.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createModule(PROJECT_ID_1, "CBL_EVALUATE", "CBL_EVALUATE.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createModule(PROJECT_ID_1, "MMRS7111", "MMRS7111.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createModule(PROJECT_ID_1, "ONENEST", "ONENEST.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
	}

	@Test
	void getReferencedDDEntriesTest() {
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "MMRS7111.cbl"));
		assertFalse(modules.isEmpty());
		final EntityId moduleId = modules.get(0).identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));

		final List<DataDictionaryPojo> referencedDDes = branchStatementUtility.getReferencedDDEntries(moduleId, new ModuleLocation(14959, 467));

		assertEquals(10, referencedDDes.size());
		final String[] dataElementNames = {
				"MYSYSIN-COMMAND", "MYSYSIN-COMMAND-ALL", "MYSYSIN-COMMAND-F", "MYSYSIN-COMMAND-FB", "MYSYSIN-COMMAND-V", "MYSYSIN-COMMAND-VB",
				"MYSYSIN-COMMAND-VSAMK", "MYSYSIN-COMMAND-VSAMR", "MYSYSIN-COMMAND-VSAME", "MYSYSIN-STATUS"};
		for (final String dataElementName : dataElementNames) {
			final Optional<DataDictionaryPojo> expectedDataElement = dataDictionaries.stream().filter(dde -> dataElementName.equals(dde.getName()))
					.findFirst();
			final Optional<DataDictionaryPojo> actualDataElement = referencedDDes.stream().filter(dde -> dataElementName.equals(dde.getName()))
					.findFirst();
			assertDataDictionaries(expectedDataElement.get(), actualDataElement.get());
		}
	}

	@Test
	void conditionInIfStatementTest() {
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "BRE2_IFELSE.cbl"));
		final EntityId moduleId = modules.get(0).identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final Optional<DataDictionaryPojo> dataElement = dataDictionaries.stream().filter(dde -> "TESTFIELD".equals(dde.getName())).findFirst();

		final List<AstNodePojo> expectedCondition = new ArrayList<>();
		expectedCondition.add(createAst("TESTFIELD = '2'", "CobolComparisonExpression"));

		final Map<String, AstNodePojo> expectedBranches = new HashMap<>();
		expectedBranches.put("TRUE",
				createAst("READ TEST-FILE AT END MOVE TESTFIELD TO WS-EOF NOT AT END DISPLAY TESTFIELD END-READ END-IF", "CobolThenBlock"));
		expectedBranches.put("FALSE",
				createAst("IF TESTFIELD EQ '4'", "CobolIfStmt"));

		final List<Tuple2<BranchStatement, String>> branchList = branchStatementUtility.getConditions(moduleId, new ModuleLocation(774, 146));
		assertEquals(1, branchList.size());
		assertEquals("TRUE", branchList.get(0).b);

		final Map<String, AstNodePojo> actualBranches = branchList.get(0).a.getBranches();
		assertBranch(expectedBranches, actualBranches);

		final List<String> conditionAsString = branchList.get(0).a.getConditionAsString();
		assertEquals("TESTFIELD = '2'", conditionAsString.get(0));

		final List<AstNodePojo> actualConditions = branchList.get(0).a.getConditions();
		assertCondition(expectedCondition, actualConditions);

		final List<DataDictionaryPojo> conditionalVariables = branchList.get(0).a.getConditionVariables();
		assertEquals(1, conditionalVariables.size());
		assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));
	}

	@Test
	void conditionInElseStatementTest() {
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "BRE2_IFELSE.cbl"));
		final EntityId moduleId = modules.get(0).identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final Optional<DataDictionaryPojo> dataElement = dataDictionaries.stream().filter(dde -> "TESTFIELD".equals(dde.getName())).findFirst();

		final List<AstNodePojo> expectedCondition = new ArrayList<>();
		expectedCondition.add(createAst("TESTFIELD = '4'", "CobolComparisonExpression"));

		final Map<String, AstNodePojo> expectedBranches = new HashMap<>();
		expectedBranches.put("TRUE", createAst("DISPLAY '4'", "CobolThenBlock"));
		expectedBranches.put("FALSE", createAst("ELSE EXEC SQL DECLARE C1 CURSOR FOR SELECT TESTFIELD FROM IW_SQL_TEST END-EXEC END-IF", "CobolElseBlock"));

		final List<Tuple2<BranchStatement, String>> branchList = branchStatementUtility.getConditions(moduleId, new ModuleLocation(1081, 141));
		assertEquals(1, branchList.size());
		assertEquals("FALSE", branchList.get(0).b);

		final Map<String, AstNodePojo> actualBranches = branchList.get(0).a.getBranches();
		assertBranch(expectedBranches, actualBranches);

		final List<String> conditionAsString = branchList.get(0).a.getConditionAsString();
		assertEquals("TESTFIELD = '4'", conditionAsString.get(0));

		final List<AstNodePojo> actualConditions = branchList.get(0).a.getConditions();
		assertCondition(expectedCondition, actualConditions);

		final List<DataDictionaryPojo> conditionalVariables = branchList.get(0).a.getConditionVariables();
		assertEquals(1, conditionalVariables.size());
		assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));
	}

	@Test
	void conditionInNestedIfStatementTest() {
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "BRE2_NESTEDIFELSE.cbl"));
		final EntityId moduleId = modules.get(0).identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));

		final List<AstNodePojo> expectedCondition1 = new ArrayList<>();
		expectedCondition1.add(createAst("D = '4'", "CobolComparisonExpression"));

		final List<AstNodePojo> expectedCondition2 = new ArrayList<>();
		expectedCondition2.add(createAst("E = '3'", "CobolComparisonExpression"));

		final Map<String, AstNodePojo> expectedBranches1 = new HashMap<>();
		expectedBranches1.put("TRUE", createAst("DISPLAY '3' ELSE DISPLAY '2' END-IF", "CobolThenBlock"));
		expectedBranches1.put("FALSE", createAst("DISPLAY '1'", "CobolDisplayStmt"));

		final Map<String, AstNodePojo> expectedBranches2 = new HashMap<>();
		expectedBranches2.put("TRUE", createAst("DISPLAY '3'", "CobolThenBlock"));
		expectedBranches2.put("FALSE", createAst("ELSE DISPLAY '2' END-IF", "CobolElseBlock"));

		final List<Tuple2<BranchStatement, String>> branchList = branchStatementUtility.getConditions(moduleId, new ModuleLocation(581, 28));
		assertEquals(2, branchList.size());
		assertEquals("TRUE", branchList.get(0).b);
		assertEquals("TRUE", branchList.get(1).b);

		Map<String, AstNodePojo> actualBranches = branchList.get(0).a.getBranches();
		assertBranch(expectedBranches1, actualBranches);
		actualBranches = branchList.get(1).a.getBranches();
		assertBranch(expectedBranches2, actualBranches);

		List<String> conditionAsString = branchList.get(0).a.getConditionAsString();
		assertEquals("D = '4'", conditionAsString.get(0));

		conditionAsString = branchList.get(1).a.getConditionAsString();
		assertEquals("E = '3'", conditionAsString.get(0));

		List<AstNodePojo> actualConditions = branchList.get(0).a.getConditions();
		assertCondition(expectedCondition1, actualConditions);
		actualConditions = branchList.get(1).a.getConditions();
		assertCondition(expectedCondition2, actualConditions);

		List<DataDictionaryPojo> conditionalVariables = branchList.get(0).a.getConditionVariables();
		assertEquals(1, conditionalVariables.size());
		Optional<DataDictionaryPojo> dataElement = dataDictionaries.stream().filter(dde -> "D".equals(dde.getName())).findFirst();
		assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));

		conditionalVariables = branchList.get(1).a.getConditionVariables();
		assertEquals(1, conditionalVariables.size());
		dataElement = dataDictionaries.stream().filter(dde -> "E".equals(dde.getName())).findFirst();
		assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));
	}

	@Test
	void conditionInNestedElseStatementTest() {
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "BRE2_NESTEDIFELSE.cbl"));
		final EntityId moduleId = modules.get(0).identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));

		final List<AstNodePojo> expectedCondition1 = new ArrayList<>();
		expectedCondition1.add(createAst("D = '4'", "CobolComparisonExpression"));

		final List<AstNodePojo> expectedCondition2 = new ArrayList<>();
		expectedCondition2.add(createAst("E = '3'", "CobolComparisonExpression"));

		final Map<String, AstNodePojo> expectedBranches1 = new HashMap<>();
		expectedBranches1.put("TRUE", createAst("DISPLAY '3' ELSE DISPLAY '2' END-IF", "CobolThenBlock"));
		expectedBranches1.put("FALSE", createAst("DISPLAY '1'", "CobolDisplayStmt"));

		final Map<String, AstNodePojo> expectedBranches2 = new HashMap<>();
		expectedBranches2.put("TRUE", createAst("DISPLAY '3'", "CobolThenBlock"));
		expectedBranches2.put("FALSE", createAst("ELSE DISPLAY '2' END-IF", "CobolElseBlock"));

		final List<Tuple2<BranchStatement, String>> branchList = branchStatementUtility.getConditions(moduleId, new ModuleLocation(629, 28));
		assertEquals(2, branchList.size());
		assertEquals("TRUE", branchList.get(0).b);
		assertEquals("FALSE", branchList.get(1).b);

		Map<String, AstNodePojo> actualBranches = branchList.get(0).a.getBranches();
		assertBranch(expectedBranches1, actualBranches);
		actualBranches = branchList.get(1).a.getBranches();
		assertBranch(expectedBranches2, actualBranches);

		List<String> conditionAsString = branchList.get(0).a.getConditionAsString();
		assertEquals("D = '4'", conditionAsString.get(0));

		conditionAsString = branchList.get(1).a.getConditionAsString();
		assertEquals("E = '3'", conditionAsString.get(0));

		List<AstNodePojo> actualConditions = branchList.get(0).a.getConditions();
		assertCondition(expectedCondition1, actualConditions);
		actualConditions = branchList.get(1).a.getConditions();
		assertCondition(expectedCondition2, actualConditions);

		List<DataDictionaryPojo> conditionalVariables = branchList.get(0).a.getConditionVariables();
		assertEquals(1, conditionalVariables.size());
		Optional<DataDictionaryPojo> dataElement = dataDictionaries.stream().filter(dde -> "D".equals(dde.getName())).findFirst();
		assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));

		conditionalVariables = branchList.get(1).a.getConditionVariables();
		assertEquals(1, conditionalVariables.size());
		dataElement = dataDictionaries.stream().filter(dde -> "E".equals(dde.getName())).findFirst();
		assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));

	}

	@Test
	void conditionIncludeWhiteSpace() {
		final Optional<EntityId> moduleEid = moduleService.findAnyModuleId(q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "BRE2_NESTEDIFELSE.cbl"));
		assertTrue(moduleEid.isPresent(), "Module must exist: " + RESOURCE_PATH + "BRE2_NESTEDIFELSE.cbl");
		final var moduleId = moduleEid.get();
		
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));

		final List<AstNodePojo> expectedCondition = new ArrayList<>();
		expectedCondition.add(createAst("D = '4'", "CobolComparisonExpression"));

		final Map<String, AstNodePojo> expectedBranches = new HashMap<>();
		expectedBranches.put("TRUE", createAst("DISPLAY '3' ELSE DISPLAY '2' END-IF", "CobolThenBlock"));
		expectedBranches.put("FALSE", createAst("DISPLAY '1'", "CobolDisplayStmt"));


		final List<Tuple2<BranchStatement, String>> branchList = branchStatementUtility.getConditions(moduleId, new ModuleLocation(556, 123));
		assertEquals(1, branchList.size());
		assertEquals("TRUE", branchList.get(0).b);

		final Map<String, AstNodePojo> actualBranches = branchList.get(0).a.getBranches();
		assertBranch(expectedBranches, actualBranches);

		final List<String> conditionAsString = branchList.get(0).a.getConditionAsString();
		assertEquals("D = '4'", conditionAsString.get(0));

		final List<AstNodePojo> actualConditions = branchList.get(0).a.getConditions();
		assertCondition(expectedCondition, actualConditions);

		final List<DataDictionaryPojo> conditionalVariables = branchList.get(0).a.getConditionVariables();
		assertEquals(1, conditionalVariables.size());
		final Optional<DataDictionaryPojo> dataElement = dataDictionaries.stream().filter(dde -> "D".equals(dde.getName())).findFirst();
		assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));
	}

	@Test
	void conditionDeepInsideNestedIfElseStatementTestAndNoAstIsInDatabaseForMarkedCode() {
		final Optional<EntityId> moduleEid = moduleService.findAnyModuleId(q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "BRE2_NESTEDIFELSE.cbl"));
		assertTrue(moduleEid.isPresent(), "Module must exist: " + RESOURCE_PATH + "BRE2_NESTEDIFELSE.cbl");
		final var moduleId = moduleEid.get();

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));

		final List<AstNodePojo> expectedCondition1 = new ArrayList<>();
		expectedCondition1.add(createAst("D = '4'", "CobolComparisonExpression"));

		final List<AstNodePojo> expectedCondition2 = new ArrayList<>();
		expectedCondition2.add(createAst("E = '3'", "CobolComparisonExpression"));

		final Map<String, AstNodePojo> expectedBranches1 = new HashMap<>();
		expectedBranches1.put("TRUE", createAst("DISPLAY '3' ELSE DISPLAY '2' END-IF", "CobolThenBlock"));
		expectedBranches1.put("FALSE", createAst("DISPLAY '1'", "CobolDisplayStmt"));

		final Map<String, AstNodePojo> expectedBranches2 = new HashMap<>();
		expectedBranches2.put("TRUE", createAst("DISPLAY '3'", "CobolThenBlock"));
		expectedBranches2.put("FALSE", createAst("ELSE DISPLAY '2' END-IF", "CobolElseBlock"));

		final List<ModuleLocation> moduleLocations = new ArrayList<>();
		moduleLocations.add(new ModuleLocation(601, 10));
		moduleLocations.add(new ModuleLocation(602, 9));
		moduleLocations.add(new ModuleLocation(603, 3));

		for (final ModuleLocation moduleLocation: moduleLocations) {
			final List<Tuple2<BranchStatement, String>> branchList = branchStatementUtility.getConditions(moduleId, moduleLocation);
			assertEquals(2, branchList.size());
			assertEquals("TRUE", branchList.get(0).b);
			assertEquals("TRUE", branchList.get(1).b);

			Map<String, AstNodePojo> actualBranches = branchList.get(0).a.getBranches();
			assertBranch(expectedBranches1, actualBranches);
			actualBranches = branchList.get(1).a.getBranches();
			assertBranch(expectedBranches2, actualBranches);

			List<String> conditionAsString = branchList.get(0).a.getConditionAsString();
			assertEquals("D = '4'", conditionAsString.get(0));

			conditionAsString = branchList.get(1).a.getConditionAsString();
			assertEquals("E = '3'", conditionAsString.get(0));

			List<AstNodePojo> actualConditions = branchList.get(0).a.getConditions();
			assertCondition(expectedCondition1, actualConditions);
			actualConditions = branchList.get(1).a.getConditions();
			assertCondition(expectedCondition2, actualConditions);

			List<DataDictionaryPojo> conditionalVariables = branchList.get(0).a.getConditionVariables();
			assertEquals(1, conditionalVariables.size());
			Optional<DataDictionaryPojo> dataElement = dataDictionaries.stream().filter(dde -> "D".equals(dde.getName())).findFirst();
			assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));

			conditionalVariables = branchList.get(1).a.getConditionVariables();
			assertEquals(1, conditionalVariables.size());
			dataElement = dataDictionaries.stream().filter(dde -> "E".equals(dde.getName())).findFirst();
			assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));
		}
	}

	@Test
	void conditionInEvaluateTest() {
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "CBL_EVALUATE.cbl"));
		final EntityId moduleId = modules.get(0).identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));

		final List<AstNodePojo> expectedCondition1 = new ArrayList<>();
		expectedCondition1.add(createAst("EVALUATE B", "CobolEvaluateCondition"));

		final List<AstNodePojo> expectedCondition2 = new ArrayList<>();
		expectedCondition2.add(createAst("EVALUATE D", "CobolEvaluateCondition"));

		final Map<String, AstNodePojo> expectedBranches1 = new HashMap<>();
		expectedBranches1.put("3", createAst("WHEN 3", "CobolWhenStmt"));
		expectedBranches1.put("OTHER", createAst("WHEN OTHER", "CobolWhenStmt"));

		final Map<String, AstNodePojo> expectedBranches2 = new HashMap<>();
		expectedBranches2.put("5", createAst("WHEN 5", "CobolWhenStmt"));
		expectedBranches2.put("OTHER", createAst("WHEN OTHER", "CobolWhenStmt"));

		final List<Tuple2<BranchStatement, String>> branchList = branchStatementUtility.getConditions(moduleId, new ModuleLocation(685, 37));
		assertEquals(2, branchList.size());
		assertEquals("OTHER", branchList.get(0).b);
		assertEquals("OTHER", branchList.get(1).b);

		Map<String, AstNodePojo> actualBranches = branchList.get(0).a.getBranches();
		assertBranch(expectedBranches1, actualBranches);
		actualBranches = branchList.get(1).a.getBranches();
		assertBranch(expectedBranches2, actualBranches);

		List<String> conditionAsString = branchList.get(0).a.getConditionAsString();
		assertEquals("EVALUATE B", conditionAsString.get(0));

		conditionAsString = branchList.get(1).a.getConditionAsString();
		assertEquals("EVALUATE D", conditionAsString.get(0));

		List<AstNodePojo> actualConditions = branchList.get(0).a.getConditions();
		assertCondition(expectedCondition1, actualConditions);
		actualConditions = branchList.get(1).a.getConditions();
		assertCondition(expectedCondition2, actualConditions);

		List<DataDictionaryPojo> conditionalVariables = branchList.get(0).a.getConditionVariables();
		assertEquals(1, conditionalVariables.size());
		Optional<DataDictionaryPojo> dataElement = dataDictionaries.stream().filter(dde -> "B".equals(dde.getName())).findFirst();
		assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));

		conditionalVariables = branchList.get(1).a.getConditionVariables();
		assertEquals(1, conditionalVariables.size());
		dataElement = dataDictionaries.stream().filter(dde -> "D".equals(dde.getName())).findFirst();
		assertDataDictionaries(dataElement.get(), conditionalVariables.get(0));

	}

	@Test
	void conditionWithLevel88Dde() {
		final var moduleId = moduleService.findAnyModuleLightweight(q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "ONENEST.cbl"))
										.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path")).identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final Optional<DataDictionaryPojo> expectedDataElement = dataDictionaries.stream()
				.filter(dde -> "LK414-NEW-GROUP".equals(dde.getName())).findFirst();
		assertTrue(expectedDataElement.isPresent(), "Expected Data element not found");

		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofModule(moduleId));
		List<DataDictionaryPojo> referencedDDEs = new ArrayList<>();
		for (final AnnotationPojo annotation : annotations) {
			referencedDDEs  = branchStatementUtility.getReferencedDDEntries(moduleId,
					annotation.getLocation().orElseThrow(() -> new IllegalStateException("Annotation location must be present")));
		}
		final Optional<DataDictionaryPojo> actualDataElement = referencedDDEs.stream()
				.filter(dde -> "LK414-NEW-GROUP".equals(dde.getName())).findFirst();
		assertTrue(actualDataElement.isPresent(), "Actual Data element not found");

		assertDataDictionaries(expectedDataElement.get(), actualDataElement.get());

	}

	@Test
	void testGetConditionsWhenControlFlowCalculationFailed() {
		/* Created an invalid module which throws COBOL parser errors */
		final var moduleId = createModule(PROJECT_ID_1, "Invalid_Module", "Invalid_Module.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		final ModuleLocation moduleLocation = new ModuleLocation(685, 37);

		/* As module is invalid so cfg will fail */
		try {
			branchStatementUtility.getConditions(moduleId, moduleLocation);
			Assertions.fail("Expected exception was not thrown");
		} catch (final IllegalStateException exception) {
			/* Verify the exception message */
			assertEquals("Failed to calculate Control flow graph for module ID - " + moduleId +
					" and Module Location: (offset - 685, length - 37).", exception.getMessage());
		}
	}

	@Test
	void testGetConditionsReturnEmpty() {
		final var moduleId = createModule(PROJECT_ID_1, "DATADICTEXPORT", "DATADICTEXPORT.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofModule(moduleId));
		for (final AnnotationPojo annotation: annotations) {
			final Optional<ModuleLocation> moduleLocation = annotation.getLocation();
			assertTrue(moduleLocation.isPresent());
			final List<Tuple2<BranchStatement, String>> branchList = branchStatementUtility.getConditions(moduleId, moduleLocation.get());
			assertEquals(0, branchList.size(), "No condition should return as annotation are independent Branches");
		}
	}

	@Test
	void testGetReferencedDdeDoesNotReturnExtraDdeUsedToRedefineVariableInCopyBook() {
		final var copyBookId = createCobolCopybook(PROJECT_ID_1, "REDEFINED", "REDEFINED.cpy", RESOURCE_PATH);
		final var moduleId = createModule(PROJECT_ID_1, "REDEFINES_MAIN", "REDEFINES_MAIN.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
											.setRelationship(RelationshipType.INCLUDES)
											.setSrcModule(moduleId)
											.setDstModule(copyBookId));
		
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> referencedDDEs = branchStatementUtility.getReferencedDDEntries(moduleId, new ModuleLocation(598, 685));
		assertEquals(4, referencedDDEs.size());

		/* check correct referenced ddes are identified or not */
		final List<String> referencedDataElementNames = List.of ("EMPLOYEE-RECORD", "EMPLOYEE-ID", "EMPLOYEE-NAME", "MAN-DEPT");
		final List<String> actualRedefinedDDEs =
				referencedDDEs.stream().map(DataDictionaryPojo::getName)
						.collect(Collectors.toList());
		assertTrue(actualRedefinedDDEs.stream().anyMatch(referencedDataElementNames::contains),
		"The Data Dictionary Reference present in ModuleLocation should be identified.");

		/* check dde with field reference from copybook should not be identified */
		final List<String> redefinedDDEs = List.of ("EMPLOYEE-SALARY", "MANAGER-SALARY", "MANAGER-DEPT", "EMPLOYEE-DEPT");
		assertFalse(actualRedefinedDDEs.stream().anyMatch(redefinedDDEs::contains),
				"The Data Dictionary Reference ASTNODE from the Copybook, but not used in ModuleLocation, should not be identified.");
	}

	@Test
	void testDuplicateDDECopyUsingGroup() {
		final var copyBookId = createCobolCopybook(PROJECT_ID_1, "EMP0001", "EMP0001.cpy", RESOURCE_PATH);
		final var newCopyBookId = createCobolCopybook(PROJECT_ID_1, "DPT0001", "DPT0001.cpy", RESOURCE_PATH);
		final var moduleId = createModule(PROJECT_ID_1, "PGM0001", "PGM0001.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
												.setRelationship(RelationshipType.INCLUDES)
												.setSrcModule(moduleId)
												.setDstModule(copyBookId));
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
												.setRelationship(RelationshipType.INCLUDES)
												.setSrcModule(moduleId)
												.setDstModule(newCopyBookId));
		
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> referencedDDEs = branchStatementUtility.getReferencedDDEntries(moduleId, new ModuleLocation(312, 691));
		assertEquals(7, referencedDDEs.size());
		assertTrue(referencedDDEs.stream().map(DataDictionaryPojo::getName)
				.collect(Collectors.toList()).containsAll(Arrays.asList("EMPLOYEE-ID", "EMPLOYEE-NAME", "EMPLOYEE-SALARY", "EMPLOYEE-DEPT",
						"ADDRESS-DTL", "CORRESPONDENCE", "PHONE-NUMBER")),
				"Should contain EMPLOYEE-ID, EMPLOYEE-NAME, EMPLOYEE-SALARY, EMPLOYEE-DEPT, ADDRESS-DTL, CORRESPONDENCE, PHONE-NUMBER");
	}

	@Test
	void testAllConditionIdentification() {
		final var moduleId = createModule(PROJECT_ID_1, "COCRDLIC", "COCRDLIC.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> referencedDDEs = branchStatementUtility.getReferencedDDEntries(moduleId, new ModuleLocation(1054, 28));
		assertFalse(referencedDDEs.isEmpty());
		
		final List<Tuple2<BranchStatement, String>> branchList = branchStatementUtility.getConditions(moduleId, new ModuleLocation(1054, 28));
		assertEquals(3, branchList.size());
		assertEquals("TRUE", branchList.get(0).b);
		assertEquals(3, branchList.get(0).a.getConditionVariables().size());
		assertEquals("TRUE", branchList.get(1).b);
		assertEquals(1, branchList.get(1).a.getConditionVariables().size());
		assertEquals("TRUE", branchList.get(2).b);
		assertEquals(1, branchList.get(2).a.getConditionVariables().size());
	}

	@Test
	void testEvaluateConditionWithPerform() {
		final var moduleId = createModule(PROJECT_ID_1, "COCRD001", "COCRD001.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> referencedDDEs = branchStatementUtility.getReferencedDDEntries(moduleId, new ModuleLocation(919, 35));
		assertFalse(referencedDDEs.isEmpty());

		final List<Tuple2<BranchStatement, String>> branchList = branchStatementUtility.getConditions(moduleId, new ModuleLocation(919, 35));
		assertEquals(1, branchList.size());
		assertEquals("OTHER", branchList.get(0).b);
		assertEquals(0, branchList.get(0).a.getConditionVariables().size());
	}

	private void assertBranch(final Map<String, AstNodePojo> expectedBranches, final Map<String, AstNodePojo> actualBranches) {
		assertEquals(expectedBranches.size(), actualBranches.size(), "Number of branches is different");

		for (final Map.Entry<String, AstNodePojo> entry : expectedBranches.entrySet()) {
			final String branchKey = entry.getKey();
			final AstNodePojo expectedNode = entry.getValue();
			final AstNodePojo actualNode = actualBranches.get(branchKey);

			assertNotNull("Branch key not found: " + branchKey, actualNode);

			assertEquals(expectedNode.getLabel(), actualNode.getLabel(), "Label mismatch for branch: " + branchKey);
			assertEquals(expectedNode.getType(), actualNode.getType(), "Type mismatch for branch: " + branchKey);
		}
	}

	private void assertCondition(final List<AstNodePojo> expectedCondition, final List<AstNodePojo> actualConditions) {
		assertEquals(expectedCondition.size(), actualConditions.size(), "Size of actual and expected conditions are not same");

		for (int i = 0; i < expectedCondition.size(); i++) {
			final AstNodePojo expectedNode = expectedCondition.get(i);
			final AstNodePojo actualNode = actualConditions.get(i);

			assertEquals(expectedNode.getLabel(), actualNode.getLabel(), "Label mismatch at index " + i);
			assertEquals(expectedNode.getType(), actualNode.getType(), "Type mismatch at index " + i);
		}
	}

	private void assertDataDictionaries(final DataDictionaryPojo expected, final DataDictionaryPojo actual) {
		assertEquals(expected.getName(), actual.getName());
		assertEquals(expected.getFormat(), actual.getFormat());
	}

	protected void submitIdentifyDataDictionaryCandidatesJob(final EntityId projectId, final EntityId moduleId) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
	}

	private AstNodePojo createAst(final String label, final String type) {
		return new AstNodePojoDummy().prepare(node -> node
				.setType(type)
				.setLabel(label))
			.build();
	}

}
