/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.extensions.export.callchain;

import static innowake.mining.extensions.export.callchain.Parameters.PARAMETER_EXPORT_FORMAT;
import static java.util.Arrays.asList;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.skyscreamer.jsonassert.JSONAssert;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.extensions.export.callchain.CallChainExporterJob.ExportFormat;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Tests for the {@link ExportFormat#DEPENDENCY_GRAPH DEPENDENCY_GRAPH} export format.
 */
@TestInstance(Lifecycle.PER_CLASS) /* required for access to testData */
@WithMockUser
@TestMethodOrder(OrderAnnotation.class)
class CallChainDependencyGraphExporterTest extends AbstractCallChainExporterTest {
	
	private static final String DEPENDENCY_GRAPH_EXPORT_FOLDER = "./test-resources/innowake/mining/server/integration/extensions/export/callchain/dependency_graph/";

	@DisplayName("Dependency Graph call chain export tests")
	@ParameterizedTest(name = "{0}")
	@MethodSource("testCases")
	@Order(1)
	void test(final String name, final Map<String, List<String>> parameters) throws Exception {
		test(name, parameters, testData);
	}

	void test(final String name, final Map<String, List<String>> parameters, final TestData testData) throws Exception {
		parameters.put(PARAMETER_EXPORT_FORMAT, Collections.singletonList("DEPENDENCY_GRAPH"));
		String actual = callChainJob(parameters);

		final int numberOfModules = testData.modules.size();
		final String[] searchNids = new String[numberOfModules];
		final String[] replaceIds = new String[numberOfModules];
		final Map<String, String> replacements = new HashMap<>(numberOfModules);
		final Map<String, String> replacementNids = new HashMap<>(numberOfModules);

		final Iterator<ModulePojo> it = testData.modules.values().iterator();
		for (int i = 0; it.hasNext(); i++) {
			final var module = it.next();
			searchNids[i] = module.getId().toString();
			final String uidString = module.getUid().toString();
			replaceIds[i] = "\"ID_" + module.getName() + "\"";
			replacements.put(uidString, "ID_" + module.getName());
			replacementNids.put(module.getId().toString(), "ID_" + module.getName());
		}

		/* Replace module nid's and uid's to its respective module name to make it readable and replacing counter from the output file as counter value may differ 
		 * for each run and cause test failures. For the modules the IDs are not in quotes while for the references they are. That's why we do two replaces. */
		final int start = actual.indexOf("rootModuleIds");
		assertNotEquals(-1, start, "Start index for 'rootModuleIds' must not be -1");
		final int end = actual.indexOf("]", start);
		assertNotEquals(-1, end, "End index for 'rootModuleIds' must not be -1");
		actual = actual.substring(0, start) + StringUtils.replaceEach(actual.substring(start, end + 1), searchNids, replaceIds) + actual.substring(end + 1);

		final Map<String, String> projectUidNidMap = testData.modules.values().stream()
				.map(ModulePojo::getProject)
				.collect(Collectors.toMap(e -> e.getUid().toString(), e -> e.getNid().toString(), (e1, e2) -> e1));
		actual = replaceProjectUids(actual, projectUidNidMap);

		//Replace all module UIDs,
		final JSONObject jsonObject = new JSONObject(actual);
		final JSONArray referenceArray = jsonObject.getJSONArray("references");
		for (int i = 0; i < referenceArray.length(); i++) {
			final JSONObject referenceObj = referenceArray.getJSONObject(i);
			final String src = replacements.get(referenceObj.getString("srcModule"));
			assertNotNull(src, "Src name must not be null");
			final String dst = replacements.get(referenceObj.getString("dstModule"));
			assertNotNull(dst, "Dst name must not be null");
			referenceObj.put("srcModule", src);
			referenceObj.put("dstModule", dst);
			referenceObj.put("id", src + "_" + referenceObj.getString("relationship") + "_" + referenceObj.getString("properties") + "_" + dst);
		}

		final JSONArray moduleArray = jsonObject.getJSONArray("modules");
		for (int i = 0; i < moduleArray.length(); i++) {
			final JSONObject moduleObj = moduleArray.getJSONObject(i);
			final String newId = "ID_" + moduleObj.getString("name");
			moduleObj.put("id", newId);
			moduleObj.put("uid", newId);
		}

		final JSONArray modulesWithMissingDependencies = jsonObject.getJSONArray("modulesWithMissingDependencies");
		for (int i = 0; i < modulesWithMissingDependencies.length(); i++) {
			final String moduleUid = modulesWithMissingDependencies.getString(i);
			modulesWithMissingDependencies.remove(i);
			modulesWithMissingDependencies.put(i, replacementNids.get(moduleUid));
		}

		if (WRITE_EXPECTED) {
			actual = jsonObject.toString(4);
			final Path path = Paths.get(DEPENDENCY_GRAPH_EXPORT_FOLDER, name + ".json");
			Files.write(path, actual.getBytes());
		} else {
			final var fileName = DEPENDENCY_GRAPH_EXPORT_FOLDER + name + ".json";
			final String exp = FileUtils.readFileToString(new File(fileName), StandardCharsets.UTF_8);
			JSONAssert.assertEquals(exp, jsonObject, false);
		}
	}

	private String replaceProjectUids(final String actual, final Map<String, String> projectUidNidMap) {
		String newActual = actual;
		for (Map.Entry<String, String> e : projectUidNidMap.entrySet()) {
			var search = e.getKey();
			var newStr = "project" + e.getValue() + "_uid";

			newActual = newActual.replace(search, newStr);
		}
		return newActual;
	}

	@Test
	@Order(2)
	void testExportCallChainConditionalDependency() throws Exception {
		final TestData data = createTestDataConditionalDependencyOneModule();
		/* Tests behavior for an outgoing graph with a conditional dependency when condition is met. */
		test("exportCallChainConditionalDependencyOut", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_A"))), data);
		
		/* Tests behavior for an outgoing graph with a conditional dependency when condition is not met. */
		test("exportCallChainConditionalDependencyOutConditionNotMet", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_B"))), data);
		
		/* Tests behavior for an incoming graph with a conditional dependency. */
		test("exportCallChainConditionalDependencyIn", parameters(DIRECTIONS, asList("IN"), START_MODULE_IDS,
				asList(moduleId(data, "ConditionalFile"))), data);
		cleanupTestData(data);
	}
	
	@Test
	@Order(3)
	void testExportCallChainMultipleConditionalDependencies() throws Exception {
		final TestData data = createTestDataMultipleConditionalDependencies();
		/* Tests behavior for an outgoing graph when condition for both conditional dependencies is met. */
		test("exportCallChainMultipleConditionalDependenciesA", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_A"))), data);
		
		/* Tests behavior for an outgoing graph when condition for one of the conditional dependencies is met. */
		test("exportCallChainMultipleConditionalDependenciesB", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_B"))), data);
		
		/* Tests behavior for an outgoing graph when condition for none of the conditional dependencies is met. */
		test("exportCallChainMultipleConditionalDependenciesC", parameters(DIRECTIONS, asList("OUT"), START_MODULE_IDS,
				asList(moduleId(data, "JOB_C"))), data);
		cleanupTestData(data);
	}

}
