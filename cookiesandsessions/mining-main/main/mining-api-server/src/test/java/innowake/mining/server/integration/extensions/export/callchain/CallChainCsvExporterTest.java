/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.extensions.export.callchain;

import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_CALL_TYPE;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_DIRECTIONS;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_END_MODULE_TYPES;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_FILTERED_TYPE;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_START_MODULE_IDS;
import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_START_MODULE_TYPES;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static java.util.Collections.singletonList;
import static java.util.Arrays.asList;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.core.util.text.TextUtil;
import innowake.mining.extensions.export.callchain.CallChainExporter;

/**
 * Tests for {@link CallChainExporter}.
 */
@TestInstance(Lifecycle.PER_CLASS) /* required for access to testData */
@WithMockUser
@TestMethodOrder(OrderAnnotation.class)
class CallChainCsvExporterTest extends AbstractCallChainExporterTest {
	
	private static final String CSV_EXPORT_FOLDER = "./test-resources/innowake/mining/server/integration/extensions/export/callchain/csv/";

	@Test
	@Order(2)
	void testExportCallChainWithInvalidStartModuleTypes() {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put(PARAMETER_START_MODULE_TYPES, Collections.singletonList("INVALID"));
		parameters.put(PARAMETER_START_MODULE_IDS, Collections.singletonList(testData.get("ROOTJOB.STEPA").getId().toString()));
		parameters.put(PARAMETER_DIRECTIONS, Collections.singletonList("IN"));
		parameters.put(PARAMETER_FILTERED_TYPE, Collections.singletonList("JOB"));

		assertThrows(IllegalArgumentException.class, () -> {
			callChainJob(parameters);
		});
	}
	
	@Test
	@Order(3)
	void testExportCallChainWithInvalidEndModuleTypes() {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put(PARAMETER_END_MODULE_TYPES, Collections.singletonList("INVALID"));
		parameters.put(PARAMETER_START_MODULE_IDS, Collections.singletonList(testData.get("ROOTJOB.STEPA").getId().toString()));
		parameters.put(PARAMETER_DIRECTIONS, Collections.singletonList("IN"));
		parameters.put(PARAMETER_FILTERED_TYPE, Collections.singletonList("JOB"));

		assertThrows(IllegalArgumentException.class, () -> {
			callChainJob(parameters);
		});
	}
	
	@Test
	@Order(4)
	void testExportCallChainWithInvalidfilteredTypes() {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put(PARAMETER_START_MODULE_IDS, Collections.singletonList(testData.get("ROOTJOB.STEPA").getId().toString()));
		parameters.put(PARAMETER_DIRECTIONS, Collections.singletonList("IN"));
		parameters.put(PARAMETER_FILTERED_TYPE, Collections.singletonList("INVALID"));

		assertThrows(IllegalArgumentException.class, () -> {
			callChainJob(parameters);
		});
	}
	
	@Test
	@Order(5)
	void testExportCallChainWithInvalidCallTypes() {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put(PARAMETER_START_MODULE_IDS, Collections.singletonList(testData.get("ROOTJOB.STEPA").getId().toString()));
		parameters.put(PARAMETER_DIRECTIONS, Collections.singletonList("IN"));
		parameters.put(PARAMETER_FILTERED_TYPE, Collections.singletonList("JOB"));
		parameters.put(PARAMETER_CALL_TYPE, Collections.singletonList("InvalidCall"));

		assertThrows(IllegalArgumentException.class, () -> {
			callChainJob(parameters);
		});
	}
	
	@Test
	@Order(6)
	void testExportCallChainWithTypeUnknown() {
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put(PARAMETER_START_MODULE_TYPES, Collections.singletonList("Unknown"));
		parameters.put(PARAMETER_START_MODULE_IDS, Collections.singletonList(testData.get("ROOTJOB.STEPA").getId().toString()));
		parameters.put(PARAMETER_DIRECTIONS, Collections.singletonList("IN"));
		parameters.put(PARAMETER_FILTERED_TYPE, Collections.singletonList("JOB"));

		assertDoesNotThrow(() -> {
			callChainJob(parameters);
		});
	}

	@Test
	@Order(9)
	void testExportCallChainForWMIN8413() throws Exception {
		final TestData data = createTestDataForWMIN8413();
		runTest("Both End Modules", createParametersWithEndModuleIds(data, asList(moduleId(data, "EndA"), moduleId(data, "EndB"))), data);
		runTest("End Module with P1", createParametersWithEndModuleIds(data, singletonList(moduleId(data, "EndA"))), data);
		runTest("End Module with P2", createParametersWithEndModuleIds(data, singletonList(moduleId(data, "EndB"))), data);
		cleanupTestData(data);
	}

	@Test
	@Order(10)
	void testExportCallChainForUnknownTechnologyAndType() {
		final TestData testData = new TestData();
		final ModulePojo moduleTest = createModule("ModuleTest", Technology.UNKNOWN, Type.PROGRAM, Storage.FILE, "/src/ModuleTest.C");
		testData.add("ModuleTest", moduleTest);
		final Map<String, List<String>> parameters = new HashMap<>();
		parameters.put(PARAMETER_START_MODULE_IDS, Collections.singletonList(moduleTest.getId().toString()));
		parameters.put(PARAMETER_DIRECTIONS, Collections.singletonList("OUT"));
		assertDoesNotThrow(() -> {
			callChainJob(parameters);
		});
		cleanupTestData(testData);
	}
	
	@Test
	@Order(7)
	void testExportCallChainConditionalDependency() throws IOException {
		final TestData data = createTestDataConditionalDependencyOneModule();
		/* Tests behavior for an outgoing graph with a conditional dependency when condition is met. */
		runTest("exportCallChainConditionalDependencyOut", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_A"))), data);
		
		/* Tests behavior for an outgoing graph with a conditional dependency when condition is not met. */
		runTest("exportCallChainConditionalDependencyOutConditionNotMet", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_B"))), data);
		
		/* Tests behavior for an incoming graph with a conditional dependency. */
		runTest("exportCallChainConditionalDependencyIn", parameters(DIRECTIONS, asList("IN"), START_MODULE_IDS,
				asList(moduleId(data, "ConditionalFile"))), data);
		cleanupTestData(data);
	}
	
	@Test
	@Order(8)
	void testExportCallChainMultipleConditionalDependencies() throws Exception {
		final TestData data = createTestDataMultipleConditionalDependencies();
		/* Tests behavior for an outgoing graph when condition for both conditional dependencies is met. */
		runTest("exportCallChainMultipleConditionalDependenciesA", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_A"))), data);
		
		/* Tests behavior for an outgoing graph when condition for one of the conditional dependencies is met. */
		runTest("exportCallChainMultipleConditionalDependenciesB", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_B"))), data);
		
		/* Tests behavior for an outgoing graph when condition for none of the conditional dependencies is met. */
		runTest("exportCallChainMultipleConditionalDependenciesC", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_C"))), data);
		cleanupTestData(data);
	}
	
	@Test
	@Order(11)
	void testExportCallChainWMIN8745WithDirectionIn() throws Exception {
		final TestData data = createTestDataOneForWMIN8745();
		runTest("exportCallChainForWMIN8745DirectionIn", parameters(START_MODULE_IDS, asList(moduleId(data, "Start")), DIRECTIONS, asList("IN"), DEPTH,
				asList("3"), END_MODULE_IDS, asList(moduleId(data, "End"))), data);
		cleanupTestData(data);
	}

	@Test
	@Order(12)
	void testExportCallChainTwoWMIN8745WithDirectionOut() throws Exception {
		final TestData data = createTestDataTwoForWMIN8745();
		runTest("exportCallChainForWMIN8745DirectionOut", parameters(START_MODULE_IDS, asList(moduleId(data, "Start")), DIRECTIONS, asList("OUT"), DEPTH,
				asList("6"), END_MODULE_IDS, asList(moduleId(data, "End"))), data);
		cleanupTestData(data);
	}
	
	@Test
	@Order(13)
	void testExportCallChainCommonProc() throws Exception {
		final TestData data = createTestDataConditionalDependencyWithCommonProc();
		
		/* Tests behavior for an outgoing graph when condition for both conditional dependencies is met. */
		runTest("exportCallChainCommonProcOut", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_A"))), data);
		
		runTest("exportCallChainCommonProcInOut", parameters(DIRECTIONS, asList("IN", "OUT"), START_MODULE_IDS,
				asList(moduleId(data, "APROC"))), data);
		
		runTest("exportCallChainCommonProcProgramInOut", parameters(DIRECTIONS, asList("IN", "OUT"), START_MODULE_IDS,
				asList(moduleId(data, "STEPAPGM"))), data);
		
		cleanupTestData(data);
	}
	
	/**
	 * Parameterized test, executing different cases for edgePropertyFilter.
	 * 
	 * @param name the Display name
	 * @param parameters a map of parameters that were passed to get call chain job result.
	 * @throws IOException if an I/O error occurs writing to or creating the expected file
	 */
	@DisplayName("CSV call chain export tests")
	@ParameterizedTest(name = "{0}")
	@MethodSource("testCases")
	@Order(1)
	void test(final String name, final Map<String, List<String>> parameters) throws IOException {
		runTest(name, parameters, testData);
	}
	
	private void runTest(final String name, final Map<String, List<String>> parameters, final TestData testData) throws IOException {
		String actual = callChainJob(parameters);
		final List<String> ids = new ArrayList<>(testData.modules.size());
		final List<String> repl = new ArrayList<>(testData.modules.size());
		
		testData.modules.values().forEach(module -> {
			ids.add(module.getId().toString());
			repl.add("ID_" + module.getName());
		});
		/* Replace module id's to its respective module name to make it readable and replacing counter from the output file as counter value may differ for 
		 * each run and cause test failures.
		 */
		actual = StringUtils.replaceEach(actual, ids.toArray(new String[0]), repl.toArray(new String[0]));

		final List<String> lines = TextUtil.splitLinesToList(actual);
		Collections.sort(lines.subList(2, lines.size()));
		final String actualCsvResult = String.join("\n", lines);
		final File file = new File(CSV_EXPORT_FOLDER + name + ".csv");
		final Path path = Paths.get(CSV_EXPORT_FOLDER, name + ".csv");
		if (WRITE_EXPECTED) {
			Files.write(path, actualCsvResult.getBytes(StandardCharsets.UTF_8));
		} else {
			final List<String> expLines = FileUtils.readLines(file, StandardCharsets.UTF_8);
			assertEquals(String.join("\n", expLines), actualCsvResult);
		}
	}
}
