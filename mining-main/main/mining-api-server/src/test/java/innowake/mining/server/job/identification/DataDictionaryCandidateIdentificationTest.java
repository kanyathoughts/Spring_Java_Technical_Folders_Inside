/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.model.Type.BMS_MAPSET;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.collection.IsEmptyCollection.empty;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.map.HashedMap;
import org.ff4j.FF4j;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Data Dictionary Candidate Identification job tests.
 */
@WithMockUser
class DataDictionaryCandidateIdentificationTest extends AbstractIdentificationTest {

	private static final String BMS_MAP_FIELD_FORMAT = "3270";

	@Autowired
	private JobManager jobManager;

	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private AnnotationService annotationService;

	@Autowired
	private FF4j ff4j;

	/**
	 * Creates the test data as following:
	 * <ul>
	 * <li>Creates a new project</li>
	 * <li>Creates new {@link Module Modules} in the new project</li>
	 * </ul>
	 */
	@BeforeAll
	void init() {
		createModule(PROJECT_ID_1, "WMIN3141", "WMIN3141.map", RESOURCE_PATH, Technology.CICS, BMS_MAPSET);
		createCobolProgram(PROJECT_ID_1, "IOSCOPE", "IOSCOPE.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7111", "MMRS7111.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "WMIN451A", "WMIN451A.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "WMIN451B", "WMIN451B.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "WMIN451C", "WMIN451C.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "WMIN451D", "WMIN451D.cpy", RESOURCE_PATH);
		createModule(PROJECT_ID_1, "UISCOPE", "UISCOPE.map", RESOURCE_PATH, Technology.CICS, Type.BMS_MAPSET);
		createCobolProgram(PROJECT_ID_1, "DBSCOPE", "DBSCOPE.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "DBB", "DBB.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "DBC", "DBC.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "DBD", "DBD.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "DBE", "DBE.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "DBF", "DBF.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "wmin477", "wmin477.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "SQLCPY", "SQLCPY.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "PARAM", "PARAM.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "PARAA", "PARAA.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MULTISCOPE", "MULTISCOPE.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "INCCPY", "INCCPY.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "CPY", "CPY.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MULTI_TABLES_MODULEA", "MULTI_TABLES_MODULEA.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MULTI_TABLES_MODULEB", "MULTI_TABLES_MODULEB.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "MULTI_TABLES_MODULEC", "MULTI_TABLES_MODULEC.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MULTI_TABLES_MODULEA", "MULTI_TABLES_MODULEA2.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MULTI_TABLES_MODULEB", "MULTI_TABLES_MODULEB2.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "MULTI_TABLES_MODULEC", "MULTI_TABLES_MODULEC2.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MULTI_SCOPE_MODULEA", "MULTI_SCOPE_MODULEA.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MULTI_SCOPE_MODULEB", "MULTI_SCOPE_MODULEB.cbl", RESOURCE_PATH);
		createCobolCopybook(PROJECT_ID_1, "MULTI_SCOPE_MODULEC", "MULTI_SCOPE_MODULEC.cpy", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7102", "MMRS7102.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7101", "MMRS7101.cbl", RESOURCE_PATH);
		createModule(PROJECT_ID_1, "MMRS71B", "MMRS71B.map", RESOURCE_PATH, Technology.CICS, Type.BMS_MAPSET);
		createModule(PROJECT_ID_1, "MMRS71Z1", "MMRS71Z1.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createModule(PROJECT_ID_1, "MMABCD", "MMABCD.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		createCobolProgram(PROJECT_ID_1, "WMIN106", "WMIN106.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7100", "MMRS7100.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "TESTPGM", "TESTPGM.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "TESTInitialValue", "TESTInitialValue.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS71B1", "MMRS71B1.cbl", RESOURCE_PATH);
		createCobolProgram(PROJECT_ID_1, "MMRS7112", "MMRS7112.cbl", RESOURCE_PATH);
	}

	@Test
	void identifyDataDictionaryCandidatesInBMSMapSet() {
		final EntityId moduleId = getModule("WMIN3141.map").identity();
		final List<DataDictionaryPojo> dataDictionariesBeforeIdentification = getDataDictionaryEntries(moduleId);
		assertThat(dataDictionariesBeforeIdentification, empty());

		/* Run the Candidate Identification on the BMS Mapset Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		/* Verify that the BMS Mapset Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = getDataDictionaryEntries(moduleId);
		assertThat(dataDictionaries, hasSize(3));
		final List<String> dataElements = dataDictionaries.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList());
		assertTrue("Identified data dictinary should contain PRDDS", dataElements.contains("PRDDS"));
		assertTrue("Identified data dictinary should contain ISSCO2", dataElements.contains("ISSCO2"));
		assertTrue("Identified data dictinary should contain APPTST2", dataElements.contains("APPTST2"));
	}

	@Test
	void testIdentifyDataDictionaryIoScope() {
		final EntityId moduleId = getModule("IOSCOPE.cbl").identity();

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "IOSCOPE.cbl");
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 1);
		expectedScopes.put(DataDictionaryVariableScope.FILE, 3);
		assertScopes(expectedScopes, dictionaries);
		final Map<String, DataDictionaryPojo> actualInitialValues = new HashMap<>();
		dictionaries.forEach( dde -> actualInitialValues.put(dde.getName(), dde));
		assertEquals("'A'", actualInitialValues.get("MY-PROGRAM-NAME").getInitialValue().orElseThrow());
	}

	@Test
	void testIdentifyDataDictionaryIoScopeFuntionWithReadNoIntoFields() {
		final EntityId moduleId = getModule("MMRS7111.cbl").identity();

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(83, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.FILE, 41);
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 42);
		assertScopes(expectedScopes, dictionaries);
		final Map<String, String> actualInitialValues = dictionaries.stream()
				.filter(dde -> dde.getInitialValue().isPresent())
				.collect(Collectors.toMap(DataDictionaryPojo::getName, d -> d.getInitialValue().get()));

		final Map<String, String> expectedInitialValues = Map.ofEntries(
				Map.entry("MYFXOUT-COUNTER", "ZERO"), Map.entry("WS-NUM5", "222"), Map.entry("WS-NUM4", "0"), Map.entry("MYFBOUT-COUNTER", "ZERO"),
				Map.entry("WS-NUM1", "123,45"), Map.entry("WS-NUM2", "123,45"), Map.entry("MYVXOUT-COUNTER", "ZERO"), Map.entry("WS-NUM3", "\"+123,45\""),
				Map.entry("MYSYSIN-COMMAND", "'F         '"), Map.entry("WS-CHARA", "'ABCDEF'"), Map.entry("MYSYSIN-COMMAND-F", "'F         '"),
				Map.entry("MYVBOUT-COUNTER", "ZERO"), Map.entry("WS-CHARX", "'A121$'"), Map.entry("MYSYSIN-COMMAND-FB", "'FB        '"),
				Map.entry("MYSYSIN-COMMAND-V", "'V         '"), Map.entry("MYVSAMK-COUNTER", "ZERO"), Map.entry("MYSYSIN-COMMAND-VB", "'VB        '"),
				Map.entry("MYSYSIN-COMMAND-VSAMK", "'VSAMK     '"), Map.entry("MYVSAMR-COUNTER", "ZERO"), Map.entry("MYSYSIN-COMMAND-VSAME", "'VSAME     '"),
				Map.entry("MYSYSIN-COMMAND-VSAMR", "'VSAMR     '"), Map.entry("MYVSAMR-RRN", "ZERO"), Map.entry("MYSYSIN-COMMAND-ALL", "'ALL       '"),
				Map.entry("MY-PROGRAM-NAME", "'MMRS7111:'"), Map.entry("MYVSAME-COUNTER", "ZERO"), Map.entry("WS-NUM6", "11"),
				Map.entry("MYDATIN-COUNTER", "ZERO"));

		assertEquals(actualInitialValues, expectedInitialValues);
	}

	@Test
	void testIdentifyDataDictionaryIoScopeFuntionInCopy() {
		final EntityId moduleId = getModule("WMIN451A.cbl").identity();
		final EntityId copyBookId = getModule("WMIN451B.cpy").identity();
		createReference(RelationshipType.INCLUDES, moduleId, copyBookId);

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(copyBookId);
		assertEquals(3, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.FILE, 3);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryIoScopeFuntionInNestedCopy() {
		final EntityId moduleId = getModule("WMIN451C.cbl").identity();
		final EntityId copyBookId1 = getModule("WMIN451D.cpy").identity();
		final EntityId copyBookId2 = getModule("WMIN451B.cpy").identity();
		createReference(RelationshipType.INCLUDES, moduleId, copyBookId1);
		createReference(RelationshipType.INCLUDES, copyBookId1, copyBookId2);

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(copyBookId2);
		assertEquals(3, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.FILE, 3);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryUiScope() {
		final EntityId moduleId = getModule("UISCOPE.map").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.CICS_UI, 3);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryDatabaseScope() {
		final EntityId moduleId = getModule("DBSCOPE.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "DBSCOPE.cbl");

		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(5, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.SQL_DATABASE, 5);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryDatabaseScopeDeleteQuery() {
		final EntityId moduleId = getModule("DBB.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(5, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.SQL_DATABASE, 3);
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 2);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryDatabaseScopeUpdateQuery() {
		final EntityId moduleId = getModule("DBC.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(6, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.SQL_DATABASE, 6);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryDatabaseScopeInsertQuery() {
		final EntityId moduleId = getModule("DBD.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(7, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.SQL_DATABASE, 3);
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 4);
		assertScopes(expectedScopes, dictionaries);
	}


	private List<DataDictionaryPojo> getDataDictionaryEntries(final EntityId moduleId) {
		return dataDictionaryService.find(q -> q.ofModule(moduleId));
	}

	@Test
	void testIdentifyDataDictionaryDatabaseScopeCursor() {
		final EntityId moduleId = getModule("DBE.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(7, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.SQL_DATABASE, 5);
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 2);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryDatabaseScopeMultipleCursor() {
		final EntityId moduleId = getModule("DBF.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(12, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.SQL_DATABASE, 10);
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 2);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryDatabaseScopeExecSqlInclude() {
		final EntityId moduleId = getModule("wmin477.cbl").identity();
		final EntityId copyBookId = getModule("SQLCPY.cpy").identity();
		createReference(RelationshipType.INCLUDES, moduleId, copyBookId);

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(copyBookId);
		assertEquals(2, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.SQL_DATABASE, 2);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryParameterScope() {
		final EntityId moduleId = getModule("PARAM.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(4, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.PARAMETER, 3);
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 1);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryParameterScopeNoFieldsInLinkage() {
		final EntityId moduleId = getModule("PARAA.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(1, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 1);
		assertScopes(expectedScopes, dictionaries);
	}

	@Test
	void testIdentifyDataDictionaryWithEditedDataDictionary() {
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(
				q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "PARAA.cbl")).orElseThrow();
		final EntityId moduleId = module.identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertEquals(1, dictionaries.size());
		final DataDictionaryPojo dataDictionary = dictionaries.get(0);

		final DataDictionaryPojoPrototype prototype = new DataDictionaryPojoPrototype();
		prototype.setUid(dataDictionary.getUid())
				.setDescription("newDesc")
				.setUpdatedByUserId("User2");
		dataDictionaryService.update(prototype);

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> dictionariesAfterIdentify = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertEquals(1, dictionariesAfterIdentify.size());
		final DataDictionaryPojo editedDataDictionary = dictionariesAfterIdentify.get(0);
		assertEquals("newDesc", editedDataDictionary.getDescription());
	}

	@Test
	void testIdentifyDataDictionaryParameterAndDatabaseScope() {
		final EntityId moduleId = getModule("MULTISCOPE.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MULTISCOPE.cbl");
		List<DataDictionaryPojo> dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(5, dictionaries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.PARAMETER, 5);
		expectedScopes.put(DataDictionaryVariableScope.SQL_DATABASE, 5);
		assertScopes(expectedScopes, dictionaries);

		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module again to verify that there are no duplicate scopes*/
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MULTISCOPE.cbl");
		dictionaries = getDataDictionaryEntries(moduleId);
		assertEquals(5, dictionaries.size());
		assertScopes(expectedScopes, dictionaries);
	}

	/**
	 * Identifies data dictionary entries for a module with a copy where the candidate is in the copy.
	 * Tests that the data dictionary entry is created for the correct module with the correct offset.
	 */
	@Test
	void testOffsetAndModuleLocationIdentifyDataDictionary() {
		final EntityId moduleId = getModule("INCCPY.cbl").identity();
		final EntityId copyBookId = getModule("CPY.cpy").identity();

		createReference(RelationshipType.INCLUDES, moduleId, copyBookId);

		assertTrue("There should be no data dictionaries before identification", getDataDictionaryEntries(moduleId).isEmpty());
		assertTrue("There should be no data dictionaries before identification", getDataDictionaryEntries(copyBookId).isEmpty());

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(copyBookId);
		assertEquals(0, getDataDictionaryEntries(moduleId).size());
		assertEquals(1, entries.size());

		final ModuleLocation entry = entries.get(0).getLocation().orElseThrow();
		assertEquals(61, entry.getOffset());
		assertEquals(9, entry.getLength());
	}

	@Test
	void testIdentifyDataDictionaryAddsDatabaseInformationToAlreadyExistingDatabaseEntries() {
		final EntityId moduleIdA = getModule("MULTI_TABLES_MODULEA2.cbl").identity();
		final EntityId moduleIdB = getModule("MULTI_TABLES_MODULEB2.cbl").identity();
		final EntityId copyBookId = getModule("MULTI_TABLES_MODULEC2.cpy").identity();
		final EntityId table1 = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "TABLE1", "UISCOPE.map", Technology.SQL, Type.TABLE);
		final EntityId table2 = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "TABLE2", "UISCOPE.map", Technology.SQL, Type.TABLE);

		createReference(RelationshipType.INCLUDES, moduleIdA, copyBookId);
		createReference(RelationshipType.INCLUDES, moduleIdB, copyBookId);

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(moduleIdA)
				.setDstModule(table1)
				.setSrcLocation(new ModuleLocation(249, 163))
				.setProperties(Map.of("DB_ACCESS_TYPE", "DELETE")));

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(moduleIdB)
				.setDstModule(table2)
				.setSrcLocation(new ModuleLocation(249, 163))
				.setProperties(Map.of("DB_ACCESS_TYPE", "DELETE")));

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MULTI_TABLES_MODULEA2.cbl");
		assertEquals(0, getDataDictionaryEntries(moduleIdA).size());

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MULTI_TABLES_MODULEB2.cbl");
		assertEquals(0, getDataDictionaryEntries(moduleIdB).size());

		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(copyBookId);
		assertEquals(1, entries.size());
		final Map<String, String> scopeAttributes = entries.get(0).getScopes().get(DataDictionaryVariableScope.SQL_DATABASE);
		assertTrue("Table scope attribute should be present", scopeAttributes.containsKey("tables"));
		assertEquals("TABLE1,TABLE2", scopeAttributes.get("tables"));
		assertEquals("TABLE1;TABLE2", entries.get(0).getTargetOutput().orElseThrow());
	}

	@Test
	void testIdentifyDataDictionaryAddsDatabaseInformationToSourceInputAndTargetOutput() {
		final EntityId moduleIdA = getModule("MULTI_TABLES_MODULEA.cbl").identity();
		final EntityId moduleIdB = getModule("MULTI_TABLES_MODULEB.cbl").identity();
		final EntityId copyBookId = getModule("MULTI_TABLES_MODULEC.cpy").identity();
		final EntityId table3 = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "TABLE3", "UISCOPE.map", Technology.SQL, Type.TABLE);
		final EntityId table4 = createFileSection(PROJECT_ID_1, RESOURCE_PATH, "TABLE4", "UISCOPE.map", Technology.SQL, Type.TABLE);

		createReference(RelationshipType.INCLUDES, moduleIdA, copyBookId);
		createReference(RelationshipType.INCLUDES, moduleIdB, copyBookId);

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(moduleIdA)
				.setDstModule(table3)
				.setSrcLocation(new ModuleLocation(249, 163))
				.setProperties(Map.of("DB_ACCESS_TYPE", "DELETE")));

		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(moduleIdB)
				.setDstModule(table4)
				.setSrcLocation(new ModuleLocation(249, 163))
				.setProperties(Map.of("DB_ACCESS_TYPE", "READ")));

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MULTI_TABLES_MODULEA.cbl");
		assertEquals(0, getDataDictionaryEntries(moduleIdA).size());

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MULTI_TABLES_MODULEB.cbl");
		assertEquals(0, getDataDictionaryEntries(moduleIdB).size());

		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(copyBookId);
		assertEquals(1, entries.size());
		final Map<String, String> scopeAttributes = entries.get(0).getScopes().get(DataDictionaryVariableScope.SQL_DATABASE);
		assertTrue("Table scope attribute should be present", scopeAttributes.containsKey("tables"));
		assertEquals("TABLE1,TABLE2", scopeAttributes.get("tables"));
		assertEquals("TABLE3", entries.get(0).getTargetOutput().orElseThrow());
		assertEquals("TABLE4", entries.get(0).getSourceInput().orElseThrow());
	}

	@Test
	void testIdentifyDataDictionaryMultipleScopesForEntryInCopybook() {
		final EntityId moduleIdA = getModule("MULTI_SCOPE_MODULEA.cbl").identity();
		final EntityId moduleIdB = getModule("MULTI_SCOPE_MODULEB.cbl").identity();
		final EntityId copyBookId = getModule("MULTI_SCOPE_MODULEC.cpy").identity();

		createReference(RelationshipType.INCLUDES, moduleIdA, copyBookId);
		createReference(RelationshipType.INCLUDES, moduleIdB, copyBookId);

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleIdA);
		assertEquals(0, getDataDictionaryEntries(moduleIdA).size());

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MULTI_SCOPE_MODULEB.cbl");
		assertEquals(0, getDataDictionaryEntries(moduleIdB).size());

		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(copyBookId);
		assertEquals(1, entries.size());
		assertTrue("Should identify DB scope", entries.get(0).getScopes().containsKey(DataDictionaryVariableScope.SQL_DATABASE));
		final Map<String, String> scopeAttributes = entries.get(0).getScopes().get(DataDictionaryVariableScope.SQL_DATABASE);
		final Set<DataDictionaryVariableScope> scopes = entries.get(0).getScopes().keySet();
		assertTrue("Table scope attribute should be present", scopeAttributes.containsKey("tables"));
		assertEquals("TABLE1", scopeAttributes.get("tables"));
		assertEquals(2, scopes.size());
		assertTrue("Should contains Parameter scope", scopes.contains(DataDictionaryVariableScope.PARAMETER));
		assertTrue("Should contains DataBase scope", scopes.contains(DataDictionaryVariableScope.SQL_DATABASE));
	}

	@Test
	void testIdentifyDataDictionaryCorrectModule() {
		final EntityId moduleId = getModule("wmin477.cbl").identity();
		final EntityId copyBookId = getModule("SQLCPY.cpy").identity();

		createReference(RelationshipType.INCLUDES, moduleId, copyBookId);

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(copyBookId);
		assertEquals(2, entries.size());
		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.SQL_DATABASE, 2);
		assertScopes(expectedScopes, entries);
	}

	@Test
	void testIdentifyDataDictionaryShowsCorrectFieldFormat() {
		final EntityId moduleId = getModule("MMRS7102.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7102.cbl");
		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(moduleId);

		final Map<String, Integer> actualFormats = new HashedMap<>();
		final Map<String, Integer> expectedFormats = new HashedMap<>();
		expectedFormats.put("L88", 5);
		expectedFormats.put("PICX", 41);
		expectedFormats.put("PIC9", 20);
		expectedFormats.put("Group", 9);
		entries.forEach(dde -> actualFormats.merge(dde.getFormat().orElse(null), 1, Integer::sum));
		assertEquals(expectedFormats, actualFormats);
	}

	@Test
	void testIdentifyDataDictionaryShowsCorrectUsage() {
		final EntityId moduleId = getModule("MMRS7101.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(moduleId);
		final List<String> usages = entries.stream().map(d -> d.getUsage().orElseThrow()).collect(Collectors.toList());
		assertNotNull(usages);

		final Map<String, Integer> actualUsages = new HashedMap<>();
		final Map<String, Integer> expectedUsages = new HashedMap<>();
		expectedUsages.put("DISPLAY", 11);
		expectedUsages.put("COMP", 2);
		expectedUsages.put("COMP1", 1);
		expectedUsages.put("COMP3", 1);
		entries.forEach(dde -> actualUsages.merge(dde.getUsage().orElse(null), 1, Integer::sum));
		assertEquals(expectedUsages, actualUsages);
	}

	@Test
	void testIdentifiedDataDictionaryLengthSameAsManuallyCreatedDataDictionary() {
		final EntityId moduleId = getModule("MMRS7100.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7100.cbl");
		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(moduleId);
		entries.forEach(
				(identifiedDataDictionary) -> {
					dataDictionaryService.delete(q -> q.byId(identifiedDataDictionary.identity()));
					final DataDictionaryPojo manuallyCreatedDataDictionary = createDataDictionaryPojo(identifiedDataDictionary);
					assertEquals(identifiedDataDictionary.getFormat(), manuallyCreatedDataDictionary.getFormat());
					assertEquals(identifiedDataDictionary.getLength(), manuallyCreatedDataDictionary.getLength());
				});
	}

	@Test
	void testIdentifyDataDictionaryCandidatesInBMSMapSet() {
		final EntityId moduleId = getModule("MMRS71B.map").identity();
		final List<DataDictionaryPojo> dataDictionariesBeforeIdentification = getDataDictionaryEntries(moduleId);
		assertThat(dataDictionariesBeforeIdentification, empty());

		/* Run the Data Dictionary Candidate Identification on the BMS Mapset Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		/* Verify that the BMS Mapset Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = getDataDictionaryEntries(moduleId);
		assertThat(dataDictionaries, hasSize(4));
		final List<String> dataElements = dataDictionaries.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList());
		assertTrue("dataElements list should contain USERTXT", dataElements.contains("USERTXT"));
		assertTrue("dataElements list should contain PWTXT", dataElements.contains("PWTXT"));
		assertTrue("dataElements list should contain PWDATA", dataElements.contains("PWDATA"));
		dataDictionaries.forEach(dataDictionary -> assertTrue("isCandidate value must be true", dataDictionary.getIsCandidate()));
		final List<Long> lengths = dataDictionaries.stream().map(d -> d.getLength().orElseThrow()).collect(Collectors.toList());
		final Set<Map<DataDictionaryVariableScope, Map<String, String>>> attributes =
				dataDictionaries.stream().map(DataDictionaryPojo::getScopes).collect(Collectors.toSet());
		final List<Long> expectedLengths = Arrays.asList(19L, 50L, 19L, 20L);
		assertEquals(expectedLengths.size(), lengths.size());
		assertEquals(1, attributes.size());
		assertTrue("lengths list should have the same elements as expectedLengths", lengths.containsAll(expectedLengths));
		assertEquals(BMS_MAP_FIELD_FORMAT, dataDictionaries.get(0).getFormat().orElseThrow());
		assertEquals(Set.of(Map.of(DataDictionaryVariableScope.CICS_UI, Map.of("accessType", "OUTPUT", "mapname", "MMRS71B", "mapset", "MMRS71B"))), attributes);
	}

	@Test
	void identifyDataDictionaryCandidatesInCobolProgram() {
		final EntityId moduleId = getModule("MMRS71Z1.cbl").identity();
		final List<DataDictionaryPojo> dataDictionariesBeforeIdentification = getDataDictionaryEntries(moduleId);
		assertThat(dataDictionariesBeforeIdentification, empty());

		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = getDataDictionaryEntries(moduleId);
		assertThat(dataDictionaries, hasSize(27));
		final List<String> dataElements = dataDictionaries.stream().map(DataDictionaryPojo::getName).collect(Collectors.toList());
		assertTrue("dataElements list should contain MY-HEX-ORIGIN-LEN", dataElements.contains("MY-HEX-ORIGIN-LEN"));
		dataDictionaries.forEach(dataDictionary -> assertTrue("isCandidate value must be true", dataDictionary.getIsCandidate()));
		final List<String> format = dataDictionaries.stream().map(d -> d.getFormat().orElseThrow()).collect(Collectors.toList());
		assertTrue("Format list should contain GROUP", format.contains("Group"));
		assertTrue("Format list should contain PICX", format.contains("PICX"));
		assertTrue("Length must be null for GROUP format field", dataDictionaries.get(format.indexOf("Group")).getLength().isEmpty());
		final Optional<DataDictionaryPojo> element = dataDictionaries.stream().filter(dde -> "MY-HEX-ORIGIN".equals(dde.getName())).findFirst();
		assertTrue("MY-HEX-ORIGIN should exist", element.isPresent());
		assertEquals(Long.valueOf(33000), element.get().getLength().orElse(null));

		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.PARAMETER, 8);
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 19);
		assertScopes(expectedScopes, dataDictionaries);
	}

	@Test
	void identifyDataDictionariesAreIdentifiedInCorrectLocation() {
		final EntityId moduleId = getModule("MMABCD.cbl").identity();
		moduleService.deleteModule(moduleId, true);
		final EntityId newModuleId = createModule(PROJECT_ID_1, "MMABCD", "MMABCD.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);
		final List<DataDictionaryPojo> dataDictionariesBeforeIdentification = getDataDictionaryEntries(newModuleId);
		assertThat(dataDictionariesBeforeIdentification, empty());
		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMABCD.cbl");

		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = getDataDictionaryEntries(newModuleId);
		assertEquals(8, dataDictionaries.size());
		assertEquals(1, dataDictionaries.stream().filter(dde -> "MY-HEX-SHOW-CHARS".equals(dde.getName())).count());
		assertEquals(1, dataDictionaries.stream().filter(dde -> "DP004-MSG-IDX".equals(dde.getName())).count());
		assertEquals(0, dataDictionaries.stream().filter(dde -> "".equals(dde.getName())).count());

		final DataDictionaryPojo testDde = dataDictionaries.stream().filter(dde -> "DP004-MSG-IDX".equals(dde.getName())).findAny().orElseThrow();
		assertEquals(new ModuleLocation(1286, 13), testDde.getLocation().orElseThrow());
	}

	@Test
	void testLengthOfL88AndGroupFieldsForDataDictionaryCandidates() {
		final EntityId moduleId = getModule("WMIN106.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(moduleId);
		assertThat(entries, hasSize(15));

		final List<String> format = entries.stream().map(d -> d.getFormat().orElseThrow()).collect(Collectors.toList());
		assertTrue("Format list should contain L88", format.contains("L88"));
		assertTrue("Format list should contain GROUP", format.contains("Group"));
		assertTrue("Length must be null for L88 format field", entries.get(format.indexOf("L88")).getLength().isEmpty());
		assertTrue("Length must be null for GROUP format field", entries.get(format.indexOf("Group")).getLength().isEmpty());
	}

	@Test
	void testIdentifyDataDictionaryDefaultLocation() {
		final EntityId moduleId = getModule("wmin477.cbl").identity();
		final EntityId moduleId2 = getModule("MMABCD.cbl").identity();
		final EntityId copyBookId = getModule("SQLCPY.cpy").identity();
		createReference(RelationshipType.INCLUDES, moduleId, copyBookId);

		/* Run Data Dictionary Candidate Identification job on Cobol Copybook */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "wmin477.cbl");

		/* Verify that associated data dictionaries defined location is Copybook */
		List<DataDictionaryPojo> dataDictionaries = getDataDictionaryEntries(copyBookId);
		assertEquals(2, dataDictionaries.size());
		dataDictionaries.stream().forEach(entry -> {
			assertEquals(DefinedLocation.COPYBOOK, entry.getDefinedLocation().orElseThrow());
		});

		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, RESOURCE_PATH + "MMABCD.cbl");

		/* Verify that associated data dictionaries defined location is Program */
		dataDictionaries = getDataDictionaryEntries(moduleId2);
		assertEquals(8, dataDictionaries.size());
		dataDictionaries.stream().forEach(entry -> {
			assertEquals(DefinedLocation.PROGRAM, entry.getDefinedLocation().orElseThrow());
		});
	}

	@Test
	void testCandidateIdentificationIdentifiesAdvanceDataDictionaries() {
		final EntityId moduleId = getModule("MMRS7101.cbl").identity();
		/* Run the Candidate Identification on the COBOL Program Module */
		submitJob(jobManager, new IdentifyCandidatesJob(PROJECT_ID_1, new ModuleMatcher(Collections.emptyList(),
				Arrays.asList(RESOURCE_PATH + "MMRS7101.cbl"))));
		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = getDataDictionaryEntries(moduleId);
		assertThat(dataDictionaries, hasSize(15));

		/* Verify Candidate Identification also Identifies Advanced Data Dictionary Data */
		final List<Long> fieldLevel = dataDictionaries.stream().map(d -> d.getFieldLevel().orElseThrow()).collect(Collectors.toList());
		assertFalse("FieldLevel must not be empty", fieldLevel.isEmpty());
		final List<String> groupPath = dataDictionaries.stream().map(d -> d.getGroupPath().orElseThrow()).collect(Collectors.toList());
		assertTrue("groupPath list should contain MY-BIN-FIELDS", groupPath.contains("MY-BIN-FIELDS"));
		assertTrue("groupPath list should contain MY-HEX-ORIGIN-LEN", groupPath.contains("MY-HEX-ORIGIN-LEN"));
		final List<String> parentGroup = dataDictionaries.stream().map(d -> d.getParentGroup().orElse(null)).collect(Collectors.toList());
		assertTrue("parentGroup list should contain MY-BIN-FIELDS", parentGroup.contains("MY-BIN-FIELDS"));

		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 12);
		expectedScopes.put(DataDictionaryVariableScope.PARAMETER, 3);
		assertScopes(expectedScopes, dataDictionaries);

		final List<DataDictionaryPojo> parameterDictionaries = dataDictionaries.stream()
				.filter(dde -> dde.getScopes().containsKey(DataDictionaryVariableScope.PARAMETER)
						&& "OUTPUT".equals(dde.getScopes().get(DataDictionaryVariableScope.PARAMETER).get("accessType")))
				.collect(Collectors.toList());
		assertEquals(3, parameterDictionaries.size());
	}

	@Test
	void testIdentifyDataDictionaryShowsCorrectInitialValue() {
		final EntityId moduleId = getModule("TESTPGM.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(moduleId);
		assertThat(entries, hasSize(21));
		final Map<String, String> actualInitialValues = entries.stream()
				.filter(dde -> dde.getInitialValue().isPresent())
				.collect(Collectors.toMap(DataDictionaryPojo::getName, d -> d.getInitialValue().get()));

		final Map<String, String> expectedInitialValues = Map.ofEntries(
				Map.entry("MY-PROGRAM-NAME", "'TESTPGM'"), Map.entry("I1", "9968"),
				Map.entry("FILLER-1", "LOW-VALUES"), Map.entry("MY-HEX-SHOW-CHARS", "'0123456789ABCDEF'"));
		assertEquals(expectedInitialValues, actualInitialValues);
	}

	@Test
	void testIdentifyDataDictionaryHavingMultipleIntialValueShowsCorrectInitialValue() {
		final EntityId moduleId = getModule("TESTInitialValue.cbl").identity();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> entries = getDataDictionaryEntries(moduleId);
		assertThat(entries, hasSize(4));
		final Map<String, String> actualInitialValues = entries.stream()
				.filter(dde -> dde.getInitialValue().isPresent())
				.collect(Collectors.toMap(DataDictionaryPojo::getName, d -> d.getInitialValue().get()));

		final Map<String, String> expectedInitialValues = Map.ofEntries(
				Map.entry("VALID-OTH-COV-STATE", "'SC'"), Map.entry("RFM-OTH-COV-STATE", "'SC';'OH'"));
		assertEquals(expectedInitialValues, actualInitialValues);
	}

	@Test
	void testIdentifyOnlyDataDictionaryWhenIdentifyDdeOnlyFlagIsEnable() {
		/* Enable the Identify DDE only flag */
		ff4j.enable(FeatureId.IDENTIFY_DDE_ONLY.getId());

		final Optional<EntityId> moduleEid = moduleService.findAnyModuleId(q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "MMRS7112.cbl"));
		assertTrue("Module must exists: " + RESOURCE_PATH + "MMRS7112.cbl", moduleEid.isPresent());

		final var moduleId = moduleEid.get();
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> entries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertThat(entries, hasSize(7));
		final List<String> actualDataElementNames = entries.stream()
				.map(DataDictionaryPojo::getName)
				.collect(Collectors.toList());

		final List<String> expectedDataElementNames = Arrays.asList(
				"KSDS-PRIMARY-INDEX",
				"MY-COUNTER",
				"MY-INFO",
				"MY-PROGRAM-NAME",
				"MYSQLCA-SQLCODE",
				"MYSQLIN-COUNTER",
				"MYSQLIN-DISPLAY"
		);


		/* Check if the content of both lists is the same */
		Collections.sort(actualDataElementNames);
		Collections.sort(expectedDataElementNames);
		assertEquals(expectedDataElementNames, actualDataElementNames);

		/* Check that the annotation should not be identified */
		List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleId));
		assertThat(annotations, hasSize(0));

		/* Disable the Identify DDE only flag */
		ff4j.disable(FeatureId.IDENTIFY_DDE_ONLY.getId());
		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);

		/* After disabling the flag, annotations should be identified */
		annotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(moduleId));
		assertThat(entries, hasSize(7));
	}

	@Test
	void testIdentifyDataDictionaryShowsCorrectUpdatedByUserName() {
		final Optional<EntityId> moduleEid = moduleService.findAnyModuleId(q -> q.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + "MMRS71B1.cbl"));
		assertTrue("Module must exists: " + RESOURCE_PATH + "MMRS71B1.cbl", moduleEid.isPresent());

		final var moduleId = moduleEid.get();

		submitIdentifyDataDictionaryCandidatesJob(PROJECT_ID_1, moduleId);
		final List<DataDictionaryPojo> entries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertEquals(23, entries.size());
		final List<DataDictionaryPojo> entriesWithUserNameAsEmpty = entries.stream()
				.filter(dde -> dde.getUpdatedByUserName().isEmpty())
				.collect(Collectors.toList());
		assertEquals(23, entriesWithUserNameAsEmpty.size());
	}

	@Test
	void testCandidateIdentificationScopesCallFieldsAsParameter() {
		final EntityId moduleId = getModule("MMRS7101.cbl").identity();
		/* Run the Candidate Identification on the COBOL Program Module */
		submitJob(jobManager, new IdentifyCandidatesJob(PROJECT_ID_1, new ModuleMatcher(Collections.emptyList(),
				Arrays.asList(RESOURCE_PATH + "MMRS7101.cbl"))));
		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = getDataDictionaryEntries(moduleId);
		assertThat(dataDictionaries, hasSize(15));

		final Map<DataDictionaryVariableScope, Integer> expectedScopes = new HashedMap<>();
		expectedScopes.put(DataDictionaryVariableScope.OTHER, 12);
		expectedScopes.put(DataDictionaryVariableScope.PARAMETER, 3);
		assertScopes(expectedScopes, dataDictionaries);

		final List<DataDictionaryPojo> parameterDictionaries = dataDictionaries.stream()
				.filter(dde -> dde.getScopes().containsKey(DataDictionaryVariableScope.PARAMETER)
						&& "OUTPUT".equals(dde.getScopes().get(DataDictionaryVariableScope.PARAMETER).get("accessType")))
				.collect(Collectors.toList());
		assertEquals(3, parameterDictionaries.size());
	}
	
	protected void submitIdentifyDataDictionaryCandidatesJob(final EntityId projectId, final EntityId moduleId) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
	}

	protected void submitIdentifyDataDictionaryCandidatesJob(final EntityId projectId, final String path) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Collections.emptyList(), Arrays.asList(path))));
	}

	private void assertScopes(final Map<DataDictionaryVariableScope, Integer> expectedScopes, final List<DataDictionaryPojo> entries) {
		final Map<DataDictionaryVariableScope, Integer> scopeCounts = new HashedMap<>();
		entries.forEach(dde -> dde.getScopes().forEach((scope, attr) -> scopeCounts.merge(scope, 1, Integer::sum)));
		assertEquals(expectedScopes, scopeCounts);
	}

	private DataDictionaryPojo createDataDictionaryPojo(final DataDictionaryPojo dictionaryEntry) {
		final DataDictionaryPojoPrototype dataDictionaryPojo = new DataDictionaryPojoPrototype()
				.setName(dictionaryEntry.getName())
				.setModule(dictionaryEntry.getModule())
				.setLocation(dictionaryEntry.getLocation().orElse(null))
				.setDescription(dictionaryEntry.getDescription())
				.setCreatedByUserId(dictionaryEntry.getCreatedByUserId());
		dictionaryEntry.getFormat().ifPresent(f -> dataDictionaryPojo.setFormat(f));
		dictionaryEntry.getLength().ifPresent(f -> dataDictionaryPojo.setLength(f));
		dictionaryEntry.getUpdatedByUserId().ifPresent(f -> dataDictionaryPojo.setUpdatedByUserId(f));
		return dataDictionaryService.create(dataDictionaryPojo);
	}

	private ModulePojo getModule(final String fileName) {
		return moduleService.findAnyModule(b -> b.ofProject(PROJECT_ID_1).withPath(RESOURCE_PATH + fileName))
				.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with path: " + RESOURCE_PATH + fileName));
	}
}
