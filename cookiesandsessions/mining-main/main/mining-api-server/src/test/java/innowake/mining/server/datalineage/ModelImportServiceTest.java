/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * This test starts tracing on a specified field, imports the model and then checks if the nodes are as expected
 */
class ModelImportServiceTest extends BaseDataLineageTest {

	protected EntityId test1Module;
	protected EntityId test2Module;
	protected EntityId test3Module;
	protected EntityId test4Module;
	protected EntityId test5Module;
	protected EntityId test6Module;
	protected EntityId test7aModule;
	protected EntityId test7bModule;
	protected EntityId test8aModule;
	protected EntityId test8bModule;
	protected EntityId test9aModule;
	protected EntityId test9bModule;
	protected EntityId test10aModule;
	protected EntityId test10bModule;
	protected EntityId test11Module;
	protected EntityId test12aModule;
	protected EntityId test12bModule;
	protected EntityId test13aModule;
	protected EntityId test13bModule;
	protected EntityId test15Module;
	protected EntityId test16aModule;
	protected EntityId test16bModule;
	protected EntityId testFillerRedefines;

	protected void testNodes(final List<EntityId> testModules, @SuppressWarnings("unused") @Nullable final ModuleLocation location, final EntityId entryModuleId, final String expectedPath) throws IOException, IllegalArgumentException {
		
		/* the ModuleLocation must represent the location of the field inside the file that you want to trace - open the file in a text editor and find
		 * the correct offset */																				
		
		final DataLineageContext context = new DataLineageContext(PROJECT_ID);
		dataLineageCoreService.traceModule(context, entryModuleId);

		final List<DataFlowNodePojo> nodes = dataFlowService.find(q -> q.ofModules(testModules));
		final List<ProxyContainerPojo> containers = dataFlowService.findProxyContainers(q -> q.ofModules(testModules));
		final String generatedNodesAndContainers = nodesToString(nodes).concat(containersToString(containers));

		compareResults(generatedNodesAndContainers, expectedPath);
	}

	
	@BeforeAll
	void createTestModules() throws IOException {
		sourceService.resetCaches();
		/* create all test modules in fixed order so they have fixed module ids */
		test1Module = createModule("modelimporttest/TEST1.cbl", "TEST1", Technology.COBOL, Type.PROGRAM);
		test2Module = createModule("modelimporttest/TEST2.cbl", "TEST2", Technology.COBOL, Type.PROGRAM);
		test3Module = createModule("modelimporttest/TEST3.cbl", "TEST3", Technology.COBOL, Type.PROGRAM);
		test4Module = createModule("modelimporttest/TEST4.cbl", "TEST4", Technology.COBOL, Type.PROGRAM);
		test5Module = createModule("modelimporttest/TEST5.cbl", "TEST5", Technology.COBOL, Type.PROGRAM);
		test6Module = createModule("modelimporttest/TEST6.cbl", "TEST6", Technology.COBOL, Type.PROGRAM);
		test7aModule = createModule("modelimporttest/TEST7a.cbl", "TEST7a", Technology.COBOL, Type.PROGRAM);
		test7bModule = createModule("modelimporttest/TEST7b.cbl", "TEST7b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test7aModule, test7bModule, new ModuleLocation(765, 55));
		test8aModule = createModule("modelimporttest/TEST8a.cbl", "TEST8a", Technology.COBOL, Type.PROGRAM);
		test8bModule = createModule("modelimporttest/TEST8b.cbl", "TEST8b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test8bModule, test8aModule);
		test9aModule = createModule("modelimporttest/TEST9a.cbl", "TEST9a", Technology.COBOL, Type.PROGRAM);
		test9bModule = createModule("modelimporttest/TEST9b.cbl", "TEST9b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test9bModule, test9aModule);
		test10aModule = createModule("modelimporttest/TEST10a.cbl", "TEST10a", Technology.COBOL, Type.PROGRAM);
		test10bModule = createModule("modelimporttest/TEST10b.cpy", "TEST10b", Technology.COBOL, Type.COPYBOOK);
		makeIncludesReference(test10aModule, test10bModule);
		test11Module = createModule("modelimporttest/TEST11.cbl", "TEST11", Technology.COBOL, Type.PROGRAM);
		test12aModule = createModule("modelimporttest/TEST12a.cbl", "TEST12a", Technology.COBOL, Type.PROGRAM);
		test12bModule = createModule("modelimporttest/TEST12b.cbl", "TEST12b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test12bModule, test12aModule);
		test13aModule = createModule("modelimporttest/TEST13a.cbl", "TEST13a", Technology.COBOL, Type.PROGRAM);
		test13bModule = createModule("modelimporttest/TEST13b.cbl", "TEST13b", Technology.COBOL, Type.PROGRAM);
		makeCallReference(test13bModule, test13aModule);
		test15Module = createModule("modelimporttest/TEST15.cbl", "TEST15", Technology.COBOL, Type.PROGRAM);
		test16aModule = createModule("modelimporttest/TEST16a.cbl", "TEST16a", Technology.COBOL, Type.PROGRAM);
		test16bModule = createModule("modelimporttest/TEST16b.cpy", "TEST16b", Technology.COBOL, Type.COPYBOOK);
		makeIncludesReference(test16aModule, test16bModule);
		testFillerRedefines = createModule("modelimporttest/TESTFILLERREDEFINES.cbl", "TESTFILLERREDEFINES", Technology.COBOL, Type.PROGRAM);
	}

	@Test
	void test1() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test1Module);
		
		testNodes(testModules, new ModuleLocation(170, 8), test1Module, "modelimporttest/TEST1.txt");
	}
	
	@Test
	void test2() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test2Module);
		
		testNodes(testModules, new ModuleLocation(132, 11), test2Module, "modelimporttest/TEST2.txt");
	}
	
	@Test
	void test3() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test3Module);
		
		testNodes(testModules, new ModuleLocation(384, 10), test3Module, "modelimporttest/TEST3.txt");
	}
	
	@Test
	void test4() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test4Module);
		
		testNodes(testModules, new ModuleLocation(132, 2), test4Module, "modelimporttest/TEST4.txt");
	}

	@Test
	void test5() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test5Module);
		
		testNodes(testModules, new ModuleLocation(792, 11), test5Module, "modelimporttest/TEST5.txt");
	}
	
	@Test
	void test6() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test6Module);
		
		testNodes(testModules, new ModuleLocation(500, 7), test6Module, "modelimporttest/TEST6a.txt");
		testNodes(testModules, new ModuleLocation(777, 10), test6Module, "modelimporttest/TEST6b.txt");
		testNodes(testModules, new ModuleLocation(311, 10), test6Module, "modelimporttest/TEST6c.txt");
	}
	
	@Disabled("WMIN-10550: Disabled due to data flow node order get changed")
	@Test
	void test8() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test8aModule);
		testModules.add(test8bModule);
		
		testNodes(testModules, new ModuleLocation(132, 5), test8bModule, "modelimporttest/TEST8a.txt");
		testNodes(testModules, new ModuleLocation(189, 2), test8aModule, "modelimporttest/TEST8b.txt");
	}

	@Disabled("WMIN-10550: Disabled due to data flow node order get changed")
	@Test
	void test9() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		
		testModules.add(test9aModule);
		testModules.add(test9bModule);
		
		testNodes(testModules, new ModuleLocation(132, 5), test9bModule, "modelimporttest/TEST9.txt");
	}

	@Test
	void test10() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		
		testModules.add(test10aModule);
		testModules.add(test10bModule);
		
		testNodes(testModules, new ModuleLocation(193, 1), test10aModule, "modelimporttest/TEST10.txt");
	}
	
	/* We expect this to throw an Exception because the copybook containing the fieldDefinition is missing.
	 * If this happens in a DataFlowQuery the Exception will be caught and added as an error marker.*/
	@Test
	void test11() throws IllegalArgumentException, IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test11Module);
		final ModuleLocation location = new ModuleLocation(193, 1);
		testNodes(testModules, location, test11Module, "modelimporttest/TEST11.txt");
	}
	
	@Test
	void test12() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test12aModule);
		testModules.add(test12bModule);
		
		testNodes(testModules, new ModuleLocation(214, 2), test12aModule, "modelimporttest/TEST12b.txt");
	}
	
	/* This test is currently producing a broken result, due to a bug in the fieldtracer. 
	 * Addressed in https://iriseu.deloitte.com/browse/WNDT-3303 */
	@Test
	void test13() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test13aModule);
		testModules.add(test13bModule);
		
		testNodes(testModules, new ModuleLocation(214, 2), test13aModule, "modelimporttest/TEST13b.txt");
	}
	
	/* Starts trace on statement */
	@Test
	void test14() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test1Module);
		
		testNodes(testModules, new ModuleLocation(264, 8), test1Module, "modelimporttest/TEST14.txt");
	}
	
	@Test
	void test15() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		testModules.add(test15Module);
		
		testNodes(testModules, new ModuleLocation(1603, 18), test15Module, "modelimporttest/TEST15.txt");
	}
	
	@Test
	void test16() throws IOException {
		final ArrayList<EntityId> testModules = new ArrayList<>();
		
		testModules.add(test16aModule);
		testModules.add(test16bModule);
		
		testNodes(testModules, new ModuleLocation(228, 1), test16aModule, "modelimporttest/TEST16.txt");
	}
	
	@Test
	void testFillerRedefines() throws IOException {
		testNodes(List.of(testFillerRedefines), new ModuleLocation(216, 27), testFillerRedefines, "modelimporttest/TESTFILLERREDEFINES.txt");
	}

}

