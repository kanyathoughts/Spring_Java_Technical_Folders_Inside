/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.poi.EncryptedDocumentException;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Advance Data Dictionary Data collection tests.
 */
@WithMockUser
class AdvancedDataDictinaryDataCollectionTest extends AbstractIdentificationTest {

	protected static final String RESOURCE_PATH2 = "/test-resources/innowake/mining/server/job/identification/wmin-7267";

	@Autowired
	private JobManager jobManager;

	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private AutowireCapableBeanFactory beanFactory;

	@Test
	void testTargeOutputForFileScopeVariables() throws EncryptedDocumentException, InvalidFormatException, IOException {
		final BaseDiscoveryTest test = new BaseDiscoveryTest() {

			@Override
			protected String getTestFolder() {
				return "csv-export/WMIN-1466";
			}

			@Override
			public void execute() throws EncryptedDocumentException, InvalidFormatException, IOException {
				uploadResources(PROJECT_ID_1);
				submitJob(jobManager, tracer, new DiscoverCodeJob(PROJECT_ID_1));
				submitJob(jobManager, tracer, createDiscoverMetricsJob(PROJECT_ID_1));
			}
		};

		final Map<String, String> expectedFileTargetOutputs = new HashMap<>();
		expectedFileTargetOutputs.put("MYVSAMR-RECORD", "MMRS00C.AWA.VSAMR");
		expectedFileTargetOutputs.put("MYVSAMK-RECORD", "MMRS00C.AWA.VSAMK");
		expectedFileTargetOutputs.put("MYVSAME-RECORD", "MMRS00C.AWA.VSAME");
		expectedFileTargetOutputs.put("MYFXOUT-RECORD", "MMRS00C.AWA.MMRS7111.FIX");
		expectedFileTargetOutputs.put("MYVBOUT-RECORD", "MMRS00C.AWA.MMRS7111.VAR.BLK");
		expectedFileTargetOutputs.put("MYFBOUT-RECORD", "MMRS00C.AWA.MMRS7111.FIX.BLK");
		expectedFileTargetOutputs.put("MYVXOUT-RECORD", "MMRS00C.AWA.MMRS7111.VAR");
		beanFactory.autowireBean(test);
		test.execute();

		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(PROJECT_ID_1).withName("MMRS7111"));
		final EntityId moduleId = modules.get(0).identity();
		submitJob(jobManager, new IdentifyCandidatesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final Map<String, String> actualTargetOutputs = dataDictionaries.stream()
				.filter(d -> d.getScopes().containsKey(DataDictionaryVariableScope.FILE) && "Group".equals(d.getFormat().orElse(null)) && d.getTargetOutput().isPresent())
				.collect(Collectors.toMap(DataDictionaryPojo::getName, d -> d.getTargetOutput().orElse(null)));
		final Map<String, String> actualSourceInputs = dataDictionaries.stream()
				.filter(d -> d.getScopes().containsKey(DataDictionaryVariableScope.FILE) && "Group".equals(d.getFormat().orElse(null)) && d.getSourceInput().isPresent())
				.collect(Collectors.toMap(DataDictionaryPojo::getName, d -> d.getSourceInput().orElse(null)));

		assertEquals(expectedFileTargetOutputs, actualTargetOutputs);
		assertEquals(Collections.emptyMap(), actualSourceInputs);

		List<List<String>> ddeToAccessType = dataDictionaries.stream()
				.filter(d -> d.getScopes().containsKey(DataDictionaryVariableScope.FILE)
						&& ! d.getScopes().get(DataDictionaryVariableScope.FILE).isEmpty())
				.map(d -> List.of(
								d.getName(),
								d.getScopes().get(DataDictionaryVariableScope.FILE).getOrDefault("accessType", "")
						)
				)
				.collect(Collectors.toList());

		MatcherAssert.assertThat(ddeToAccessType, Matchers.containsInAnyOrder(
				List.of("MYFXOUT-RECORD", "WRITE"),
				List.of("MYFXOUT-ALL", "WRITE"),
				List.of("MYFBOUT-RECORD", "WRITE"),
				List.of("MYFBOUT-ALL", "WRITE"),
				List.of("MYVXOUT-RECORD", "WRITE"),
				List.of("MYVXOUT-ALL", "WRITE"),
				List.of("MYVBOUT-RECORD", "WRITE"),
				List.of("MYVBOUT-ALL", "WRITE"),
				List.of("MYVSAMK-RECORD", "WRITE"),
				List.of("MYVSAMK-ALL", "WRITE"),
				List.of("MYVSAMR-RECORD", "WRITE"),
				List.of("MYVSAMR-ALL", "WRITE"),
				List.of("MYVSAME-RECORD", "WRITE"),
				List.of("MYVSAME-ALL", "WRITE")
		));
	}

	@Test
	void testTargeOutputSourceInputForFileScopeVariables() throws EncryptedDocumentException, InvalidFormatException, IOException {
		final BaseDiscoveryTest test = new BaseDiscoveryTest() {

			@Override
			protected String getTestFolder() {
				return "cobol-dataset-identification";
			}

			@Override
			public void execute() throws EncryptedDocumentException, InvalidFormatException, IOException {
				uploadResources(PROJECT_ID_1);
				submitJob(jobManager, tracer, new DiscoverCodeJob(PROJECT_ID_1));
				submitJob(jobManager, tracer, createDiscoverMetricsJob(PROJECT_ID_1));
			}
		};

		final Map<String, String> expectedFileTargetOutputs = new HashMap<>();
		expectedFileTargetOutputs.put("MYFBIO-RECORD", "DATASET.TRANSITIVE.FOR.READWRITE");
		expectedFileTargetOutputs.put("MYEXTEND-RECORD", "DATASET.TRANSITIVE.FOR.EXTEND");
		expectedFileTargetOutputs.put("MYFXOUT-RECORD", "DATASET.TRANSITIVE.FOR.WRITE");
		final Map<String, String> expectedFileSourceInputs = new HashMap<>();
		expectedFileSourceInputs.put("MYFBIN-RECORD", "DATASET.TRANSITIVE.FOR.READ");
		expectedFileSourceInputs.put("MYFBIO-RECORD", "DATASET.TRANSITIVE.FOR.READWRITE");
		beanFactory.autowireBean(test);
		test.execute();

		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(PROJECT_ID_1).withName("MAINPGM"));
		final EntityId moduleId = modules.get(0).identity();
		submitJob(jobManager, new IdentifyCandidatesJob(PROJECT_ID_1, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final Map<String, String> actualTargetOutputs = dataDictionaries.stream()
				.filter(d -> d.getScopes().containsKey(DataDictionaryVariableScope.FILE) && "Group".equals(d.getFormat().orElse(null)) && d.getTargetOutput().isPresent())
				.collect(Collectors.toMap(DataDictionaryPojo::getName, d -> d.getTargetOutput().orElse(null)));
		final Map<String, String> actualSourceInputs = dataDictionaries.stream()
				.filter(d -> d.getScopes().containsKey(DataDictionaryVariableScope.FILE) && "Group".equals(d.getFormat().orElse(null)) && d.getSourceInput().isPresent())
				.collect(Collectors.toMap(DataDictionaryPojo::getName, d -> d.getSourceInput().orElse(null)));

		assertEquals(expectedFileTargetOutputs, actualTargetOutputs);
		assertEquals(expectedFileSourceInputs, actualSourceInputs);
	}

	@Test
	void testTargetOutputAndFieldTransformationDataCalculation() {
		final EntityId moduleAId = createModule(PROJECT_ID_1, "MMRS7101", "MMRS7101.cbl", RESOURCE_PATH, Technology.COBOL, Type.PROGRAM);

		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitAdvancedDataCollectionJob(PROJECT_ID_1, RESOURCE_PATH + "MMRS7101.cbl");

		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionariesForModuleA = dataDictionaryService.find(q -> q.ofModule(moduleAId));
		assertEquals(15, dataDictionariesForModuleA.size());

		final DataDictionaryPojo dde1 = dataDictionariesForModuleA.stream().filter(d -> "MY-PROGRAM-NAME".equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MY-PROGRAM-NAME"));

		final DataDictionaryPojo dde2 = dataDictionariesForModuleA.stream().filter(d -> "MY-COPY-NAME".equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MY-COPY-NAME"));

		final DataDictionaryPojo dde3 = dataDictionariesForModuleA.stream().filter(d -> "MY-ADD-SOURCE".equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MY-ADD-SOURCE"));

		final DataDictionaryPojo dde4 = dataDictionariesForModuleA.stream().filter(d -> "MY-ADD-TARGET".equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MY-ADD-TARGET"));

		final DataDictionaryPojo dde5 = dataDictionariesForModuleA.stream().filter(d -> "MY-HEX-ORIGIN".equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MY-HEX-ORIGIN"));

		final DataDictionaryPojo dde6 = dataDictionariesForModuleA.stream().filter(d -> "MY-HEX-TRANS".equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MY-HEX-TRANS"));

		assertEquals("'MMRS7101:'", dde1.getTargetOutput().orElseThrow());
		assertEquals("01 MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS7101:'.", dde1.getFieldTransformation().orElseThrow());
		assertTrue(dde1.getParentGroup().isEmpty());
		assertEquals("MY-PROGRAM-NAME", dde1.getGroupPath().orElseThrow());
		assertEquals(0, dde1.getIndentation().orElseThrow());
		assertEquals(1, dde1.getFieldLevel().orElseThrow());

		assertEquals("'MMRS710A: '", dde2.getTargetOutput().orElseThrow());
		assertEquals("05 MY-COPY-NAME PIC X(10) VALUE 'MMRS710A: '.", dde2.getFieldTransformation().orElseThrow());
		assertEquals("MY-BIN-FIELDS", dde2.getParentGroup().orElseThrow());
		assertEquals("MY-BIN-FIELDS/MY-COPY-NAME", dde2.getGroupPath().orElseThrow());
		assertEquals(1, dde2.getIndentation().orElseThrow());
		assertEquals(5, dde2.getFieldLevel().orElseThrow());

		assertEquals("12345.11", dde3.getTargetOutput().orElseThrow());
		assertEquals("01 MY-ADD-SOURCE PIC 9(5)V9(2) VALUE 12345.11.", dde3.getFieldTransformation().orElseThrow());

		assertTrue(dde4.getTargetOutput().isEmpty());
		assertTrue(dde4.getFieldTransformation().isEmpty());

		assertTrue(dde5.getTargetOutput().isEmpty());
		assertTrue(dde5.getFieldTransformation().isEmpty());

		assertEquals("'MMRS71Z1'", dde6.getTargetOutput().get());
		assertEquals("01 MY-HEX-TRANS PIC X(8) VALUE 'MMRS71Z1'.", dde6.getFieldTransformation().orElseThrow());
	}

	@Test
	void testTargetOutputAndFieldTransformation() {
		final EntityId moduleId = createModule(PROJECT_ID_1, "MMRS7111", "MMRS7111.cbl", RESOURCE_PATH2, Technology.COBOL, Type.PROGRAM);

		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitAdvancedDataCollectionJob(PROJECT_ID_1, RESOURCE_PATH2 + "MMRS7111.cbl");

		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionariesForModule = dataDictionaryService.find(q -> q.ofModule(moduleId));
		assertEquals(86, dataDictionariesForModule.size());

		final DataDictionaryPojo dde1 = dataDictionariesForModule
				.stream()
				.filter(d -> "MYSYSIN-STATUS"
						.equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MYSYSIN-STATUS"));
		assertEquals("MOVE '01' TO MYSYSIN-STATUS", dde1.getFieldTransformation().orElseThrow());
		assertEquals("'01'", dde1.getTargetOutput().orElseThrow());

		final DataDictionaryPojo dde2 = dataDictionariesForModule
				.stream()
				.filter(d -> "MY-PROGRAM-NAME"
						.equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MY-PROGRAM-NAME"));
		assertEquals("01 MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS7111:'.", dde2.getFieldTransformation().orElseThrow());
		assertEquals("'MMRS7111:'", dde2.getTargetOutput().orElseThrow());

		final DataDictionaryPojo dde3 = dataDictionariesForModule
				.stream()
				.filter(d -> "MYVXOUT-COUNTER"
						.equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MYVXOUT-COUNTER"));
		assertEquals("MOVE MYVXOUT-COUNTER TO MYVXOUT-DISPLAY", dde3.getFieldTransformation().orElseThrow());
		assertTrue(dde3.getTargetOutput().isEmpty());

		final DataDictionaryPojo dde4 = dataDictionariesForModule
				.stream()
				.filter(d -> "MYSYSIN-CONSTANT"
						.equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MYSYSIN-CONSTANT"));
		assertEquals("ADD 1111111111111 TO MYSYSIN-CONSTANT", dde4.getFieldTransformation().orElseThrow());
		assertTrue(dde4.getTargetOutput().isEmpty());

		final DataDictionaryPojo dde5 = dataDictionariesForModule
				.stream()
				.filter(d -> "MYSYSIN-NODEFAULT"
						.equals(d.getName()))
				.findAny().orElseThrow(() -> new IllegalStateException("Expected a dde with name MYSYSIN-NODEFAULT"));
		assertTrue(dde5.getFieldTransformation().isEmpty());
		assertTrue(dde5.getTargetOutput().isEmpty());
	}

	@Test
	void testAdvancedDataDictinaryDataCollectionWithPath() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "MEE4424A", "MEE4424A.cbl", RESOURCE_PATH);

		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());

		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitAdvancedDataCollectionJob(PROJECT_ID_1, RESOURCE_PATH + "MEE4424A.cbl");

		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));
		final DataDictionaryPojo lvl1 = dataDictionaries.stream().filter(d -> "VSAM-RECORD-OTHER".equals(d.getName())).findAny()
				.orElseThrow(() -> new IllegalStateException("Expected a dde with name VSAM-RECORD-OTHER"));
		final DataDictionaryPojo lvl5 = dataDictionaries.stream().filter(d -> "CMF-KEY9-OTHER".equals(d.getName())).findAny()
				.orElseThrow(() -> new IllegalStateException("Expected a dde with name CMF-KEY9-OTHER"));
		final DataDictionaryPojo lvl10 = dataDictionaries.stream().filter(d -> "CMF-NUMBER-OTHER".equals(d.getName())).findAny()
				.orElseThrow(() -> new IllegalStateException("Expected a dde with name CMF-NUMBER-OTHER"));

		assertEquals(16, dataDictionaries.size());
		assertEquals(9, dataDictionaries.stream().filter(d -> ! d.getIsReferenced().orElseThrow()).count());

		assertEquals(1, lvl1.getFieldLevel().orElseThrow());
		assertTrue("Level 1 dde should not have a parent group", lvl1.getParentGroup().isEmpty());
		assertEquals("VSAM-RECORD-OTHER", lvl1.getGroupPath().orElseThrow());
		assertEquals(0, lvl1.getIndentation().orElseThrow());

		assertEquals(5, lvl5.getFieldLevel().orElseThrow());
		assertEquals("VSAM-RECORD-OTHER", lvl5.getParentGroup().orElseThrow());
		assertEquals("VSAM-RECORD-OTHER/CMF-KEY9-OTHER", lvl5.getGroupPath().orElseThrow());
		assertEquals(1, lvl5.getIndentation().orElseThrow());

		assertEquals(10, lvl10.getFieldLevel().orElseThrow());
		assertEquals("CMF-KEY9-OTHER", lvl10.getParentGroup().orElseThrow());
		assertEquals("VSAM-RECORD-OTHER/CMF-KEY9-OTHER/CMF-NUMBER-OTHER", lvl10.getGroupPath().orElseThrow());
		assertEquals(2, lvl10.getIndentation().orElseThrow());
	}

	@Test
	void testAdvancedDataDictinaryDataCollectionWithModuleId() {
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "MMRS7100", "MMRS7100.cbl", RESOURCE_PATH);

		assertEquals(0, dataDictionaryService.find(q -> q.ofModule(moduleId)).size());

		/* Run the Data Dictionary Candidate Identification on the COBOL Program Module */
		submitAdvancedDataCollectionJob(PROJECT_ID_1, moduleId);

		/* Verify that the COBOL Program Module has the data dictionaries associated */
		final List<DataDictionaryPojo> dataDictionaries = dataDictionaryService.find(q -> q.ofModule(moduleId));

		final DataDictionaryPojo dde1 = dataDictionaries.stream().filter(d -> "MYSYSIN-RECORD".equals(d.getName())).findAny()
				.orElseThrow(() -> new IllegalStateException("Expected a dde with name MYSYSIN-RECORD"));
		final DataDictionaryPojo dde2 = dataDictionaries.stream().filter(d -> "MYSYSIN-COMMAND".equals(d.getName())).findAny()
				.orElseThrow(() -> new IllegalStateException("Expected a dde with name MYSYSIN-COMMAND"));
		final DataDictionaryPojo dde3 = dataDictionaries.stream().filter(d -> "MYSYSIN-COMMAND-F".equals(d.getName())).findAny()
				.orElseThrow(() -> new IllegalStateException("Expected a dde with name MYSYSIN-COMMAND-F"));

		assertEquals(6, dataDictionaries.size());
		assertEquals(5, dataDictionaries.stream().filter(d -> ! d.getIsReferenced().orElseThrow()).count());

		assertEquals(1, dde1.getFieldLevel().orElseThrow());
		assertTrue("Level 1 dde should not have a parent group", dde1.getParentGroup().isEmpty());
		assertEquals("MYSYSIN-RECORD", dde1.getGroupPath().orElseThrow());
		assertEquals(0, dde1.getIndentation().orElseThrow());

		assertEquals(5, dde2.getFieldLevel().orElseThrow());
		assertEquals("MYSYSIN-RECORD", dde2.getParentGroup().orElseThrow());
		assertEquals("MYSYSIN-RECORD/MYSYSIN-COMMAND", dde2.getGroupPath().orElseThrow());
		assertEquals(1, dde2.getIndentation().orElseThrow());

		assertEquals(88, dde3.getFieldLevel().orElseThrow());
		assertEquals("MYSYSIN-COMMAND", dde3.getParentGroup().orElseThrow());
		assertEquals("MYSYSIN-RECORD/MYSYSIN-COMMAND/MYSYSIN-COMMAND-F", dde3.getGroupPath().orElseThrow());
		assertEquals(2, dde3.getIndentation().orElseThrow());
	}

	private void submitAdvancedDataCollectionJob(final EntityId projectId, final String path) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Collections.emptyList(), Arrays.asList(path))));
	}

	private void submitAdvancedDataCollectionJob(final EntityId projectId, final EntityId moduleId) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
	}
}
