/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.datalineage;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.server.datalineage.query.DataFlowGraphQuery;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.server.datalineage.query.QueryDirection;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.DataInterfaceNode;
import innowake.mining.shared.model.datalineage.graph.FieldNode;
import innowake.mining.shared.model.datalineage.graph.ModuleNode;
import innowake.mining.shared.model.datalineage.graph.StatementNode;

/**
 * Tests for {@link DataFlowGraphQuery} having dataFlow between multiple modules. 
 */
class RealWorldDataFlowGraphQueryTest extends BaseDataLineageTest {

	protected EntityId testMMRS71Z1ModuledisconnectedModule;
	protected EntityId testMMRS71AModule;
	protected EntityId testMMRS71BModule;
	protected EntityId testMMRS71CModule;
	protected EntityId testMMRS710AModule;
	protected EntityId testMMRS710DModule;
	protected EntityId testMMRS710EModule;
	protected EntityId testDFHAIDModule;
	protected EntityId testMMRS7101Module;
	protected EntityId testMMRS7102Module;
	protected EntityId testMMRS71Z2Module;
	protected EntityId testMMRS71Z3Module;
	protected EntityId testMMRS7111Module;
	protected EntityId testMMRS7112Module;
	protected EntityId testMMRS71B1Module;
	protected EntityId testMMRS71C1Module;
	protected EntityId testMMRS71D1Module;
	protected EntityId testMMRS71Z1Module;
	protected EntityId testConstants;
	protected EntityId testComp;

	@BeforeAll
	void createTestModules() throws IOException {
		sourceService.resetCaches();
		testMMRS71Z1ModuledisconnectedModule = createModule("realmodule/testMMRS71Z1ModuledisconnectedModule.cbl", "testMMRS71Z1ModuledisconnectedModule", Technology.COBOL, Type.PROGRAM);
		testMMRS71AModule = createModule("realmodule/MMRS71A.cpy", "MMRS71A", Technology.COBOL, Type.COPYBOOK);
		testMMRS71BModule = createModule("realmodule/MMRS71B.cpy", "MMRS71B", Technology.COBOL, Type.COPYBOOK);
		testMMRS71CModule = createModule("realmodule/MMRS71C.cpy", "MMRS71C", Technology.COBOL, Type.COPYBOOK);
		testMMRS710AModule = createModule("realmodule/MMRS710A.cpy", "MMRS710A", Technology.COBOL, Type.COPYBOOK);
		testMMRS710DModule = createModule("realmodule/MMRS710D.cpy", "MMRS710D", Technology.COBOL, Type.COPYBOOK);
		testMMRS710EModule = createModule("realmodule/MMRS710E.cpy", "MMRS710E", Technology.COBOL, Type.COPYBOOK);
		testDFHAIDModule = createModule("realmodule/DFHAID.cpy", "DFHAID", Technology.COBOL, Type.COPYBOOK);
		testMMRS7101Module = createModule("realmodule/MMRS7101.cbl", "MMRS7101", Technology.COBOL, Type.PROGRAM);
		testMMRS7102Module = createModule("realmodule/MMRS7102.cbl", "MMRS7102", Technology.COBOL, Type.PROGRAM);
		testMMRS71Z2Module = createModule("realmodule/MMRS71Z2.cbl", "MMRS71Z2", Technology.COBOL, Type.PROGRAM);
		testMMRS71Z3Module = createModule("realmodule/MMRS71Z3.cbl", "MMRS71Z3", Technology.COBOL, Type.PROGRAM);
		testMMRS7111Module = createModule("realmodule/MMRS7111.cbl", "MMRS7111", Technology.COBOL, Type.PROGRAM);
		testMMRS7112Module = createModule("realmodule/MMRS7112.cbl", "MMRS7112", Technology.COBOL, Type.PROGRAM);
		testMMRS71B1Module = createModule("realmodule/MMRS71B1.cbl", "MMRS71B1", Technology.COBOL, Type.PROGRAM);
		testMMRS71C1Module = createModule("realmodule/MMRS71C1.cbl", "MMRS71C1", Technology.COBOL, Type.PROGRAM);
		testMMRS71D1Module = createModule("realmodule/MMRS71D1.cbl", "MMRS71D1", Technology.COBOL, Type.PROGRAM);
		testMMRS71Z1Module = createModule("realmodule/MMRS71Z1.cbl", "MMRS71Z1", Technology.COBOL, Type.PROGRAM);
		testConstants = createModule("dataflowgraphquerytest/TestConstants.cbl", "TestConstants", Technology.COBOL, Type.PROGRAM);
		testComp = createModule("realmodule/TestComp.cbl", "TestComp", Technology.COBOL, Type.PROGRAM);
		makeIncludesReference(testMMRS7101Module, testMMRS710AModule);
		makeCallReference(testMMRS7101Module, testMMRS71Z1Module, new ModuleLocation(3249, 112));
		makeCallReference(testMMRS7101Module, testMMRS71Z1Module, new ModuleLocation(3678, 112));
		makeIncludesReference(testMMRS7102Module, testMMRS710AModule);
		makeCallReference(testMMRS7102Module, testMMRS71Z2Module, new ModuleLocation(4999, 31));
		makeIncludesReference(testMMRS71Z2Module, testMMRS710AModule);
		makeIncludesReference(testMMRS71Z3Module, testMMRS710AModule);
		makeIncludesReference(testMMRS71Z3Module, testMMRS710DModule);
		makeIncludesReference(testMMRS7111Module, testMMRS710AModule);
		makeIncludesReference(testMMRS7112Module, testMMRS710AModule);
		makeIncludesReference(testMMRS71B1Module, testMMRS71AModule);
		makeIncludesReference(testMMRS71B1Module, testMMRS71BModule);
		makeIncludesReference(testMMRS71B1Module, testMMRS710DModule);
		makeIncludesReference(testMMRS71B1Module, testMMRS710EModule);
		makeCallReference(testMMRS71B1Module, testMMRS71D1Module, new ModuleLocation(6347, 66));
		makeCallReference(testMMRS71B1Module, testMMRS71Z1Module, new ModuleLocation(9270, 50));
		makeIncludesReference(testMMRS71C1Module, testMMRS71AModule);
		makeIncludesReference(testMMRS71C1Module, testMMRS71CModule);
		makeIncludesReference(testMMRS71C1Module, testMMRS710DModule);
		makeIncludesReference(testMMRS71C1Module, testMMRS710EModule);
		makeCallReference(testMMRS71C1Module, testMMRS71Z1Module, new ModuleLocation(3358, 80));
		makeIncludesReference(testMMRS71D1Module, testMMRS710AModule);
		makeIncludesReference(testMMRS71D1Module, testMMRS710DModule);
		makeCallReference(testMMRS71D1Module, testMMRS71Z3Module, new ModuleLocation(1864, 33));
		makeCallReference(testMMRS71D1Module, testMMRS71Z3Module, new ModuleLocation(1947, 33));
		makeCallReference(testMMRS71D1Module, testMMRS71Z3Module, new ModuleLocation(2030, 33));
		makeCallReference(testMMRS71D1Module, testMMRS71Z3Module, new ModuleLocation(2113, 33));
		makeCallReference(testMMRS71D1Module, testMMRS71Z3Module, new ModuleLocation(2196, 33));
	}

	@Disabled("Expected values need to be investigated")
	@Test
	void testMMRS71Z1disconnected() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(PROJECT_ID)
				.addStartField(testMMRS71Z1ModuledisconnectedModule, 1603)
				.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
				.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(7, statementNodes.size());
		assertEquals(17, fieldNodes.size());
		assertEquals(1, moduleNodes.size());
		assertEquals(2, dataInterfaceNodes.size());
	}

	@Disabled("WMIN-9348: produces an incorrect result")
	@Test
	void testMMRS7101Module() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testMMRS7101Module))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(51, statementNodes.size());
		assertEquals(17, fieldNodes.size());
		assertEquals(2, moduleNodes.size());
		assertEquals(11, dataInterfaceNodes.size());
	}

	@Disabled("WMIN-9348: produces an incorrect result")
	@Test
	void testMMRS7102Module() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testMMRS7102Module))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(84, statementNodes.size());
		assertEquals(94, fieldNodes.size());
		assertEquals(2, moduleNodes.size());
		assertEquals(17, dataInterfaceNodes.size());
	}

	@Disabled("WMIN-9348: produces an incorrect result")
	@Test
	void testMMRS71Z2Module() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testMMRS71Z2Module))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(67, statementNodes.size());
		assertEquals(88, fieldNodes.size());
		assertEquals(2, moduleNodes.size());
		assertEquals(17, dataInterfaceNodes.size());
	}

	@Disabled("WMIN-11581: After this ticket is done the statement nodes will be populated correctly.")
	@Test
	void testMMRS71Z3Module() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testMMRS71Z3Module))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(2, statementNodes.size());
		assertEquals(45, fieldNodes.size());
		assertEquals(1, moduleNodes.size());
		assertEquals(1, dataInterfaceNodes.size());
	}

	@Disabled("WMIN-11581: After this ticket is done the statement nodes will be populated correctly.")
	@Test
	void testMMRS7111Module() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testMMRS7111Module))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(93, statementNodes.size());
		assertEquals(75, fieldNodes.size());
		assertEquals(1, moduleNodes.size());
		assertEquals(0, dataInterfaceNodes.size());
	}

	@Disabled("WMIN-11581: After this ticket is done the statement nodes will be populated correctly.")
	@Test
	void testMMRS7112Module() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testMMRS7112Module))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(14, statementNodes.size());
		assertEquals(7, fieldNodes.size());
		assertEquals(1, moduleNodes.size());
		assertEquals(0, dataInterfaceNodes.size());
	}

	@Disabled("WMIN-11581: After this ticket is done the statement nodes will be populated correctly.")
	@Test
	void testMMRS71B1Module() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testMMRS71B1Module))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(33, statementNodes.size());
		assertEquals(23, fieldNodes.size());
		assertEquals(1, moduleNodes.size());
		assertEquals(3, dataInterfaceNodes.size());
	}

	@Disabled("WMIN-11151: After this ticket is done field nodes will be populated correctly.")
	@Test
	void testMMRS71C1Module() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testMMRS71C1Module))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(16, statementNodes.size());
		assertEquals(9, fieldNodes.size());
		assertEquals(1, moduleNodes.size());
		assertEquals(4, dataInterfaceNodes.size());
	}

	@Disabled("WMIN-11581: After this ticket is done the statement nodes will be populated correctly.")
	@Test
	void testMMRS71D1Module() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testMMRS71D1Module))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(12, statementNodes.size());
		assertEquals(4, fieldNodes.size());
		assertEquals(1, moduleNodes.size());
		assertEquals(5, dataInterfaceNodes.size());
	}
	
	@Disabled("WMIN-11581: After this ticket is done the statement nodes will be populated correctly.")
	@Test
	void testMMRS71Z1Module() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testMMRS71Z1Module))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(15, statementNodes.size());
		assertEquals(22, fieldNodes.size());
		assertEquals(1, moduleNodes.size());
		assertEquals(3, dataInterfaceNodes.size());
	}
	
	@Test
	void testComp() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(testComp))
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true)
						.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast).collect(Collectors.toList());

		final Set<DataInterfaceNode> dataInterfaceNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.DATA_INTERFACE)).map(DataInterfaceNode.class::cast).collect(Collectors.toSet());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(1, statementNodes.size());
		assertEquals(2, fieldNodes.size());
		assertEquals(1, moduleNodes.size());
		assertEquals(0, dataInterfaceNodes.size());
	}
	
	/**
	 * Test to ensure data lineage does not throw any exception.
	 */
	@Test
	void testConstants() {
		assertDoesNotThrow(() -> {
			dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
					new Parameters.Builder().setProjectId(PROJECT_ID)
							.setStartModuleIds(Collections.singletonList(testConstants))
							.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true)
							.build());
		});
	}
}
