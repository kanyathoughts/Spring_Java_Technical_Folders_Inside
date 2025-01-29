package innowake.mining.server.datalineage;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.stream.Collectors;

import innowake.mining.shared.model.datalineage.graph.*;
import org.apache.commons.collections4.ListValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.server.datalineage.query.QueryDirection;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.entities.FieldInfoPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

class ExecSqlDataFlowGraphQueryTest extends BaseDataLineageTest {

	@Autowired
	private FieldInfoService fieldInfoService;

	@Test
	void testExecSqlDataFlowGraphInsert() throws IOException {
		final var cobolModule = createModule("execsqldataflowgraphquerytest/ExecSqlDataFlowGraphQueryTest.cbl",
				"ExecSqlDataFlowGraphQueryTest", Technology.COBOL, Type.PROGRAM);
		final var execSqlTable = createModule("IW_SQL_TEST", Technology.SQL, Type.TABLE);
		final var fieldInfoList = Arrays.asList(newFieldInfo(1, "ALPHA_SHORT"), newFieldInfo(2, "N_5_0"),
				newFieldInfo(3, "N_0_3"), newFieldInfo(4, "P_7_2"), newFieldInfo(5, "DATUM"));
		final var execSqlTableModule = moduleService.getModule(execSqlTable);
		insertFieldInfo(execSqlTableModule, fieldInfoList);
		makeReadsWritesReference(cobolModule, execSqlTable, new ModuleLocation(696, 490), "STORE");

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(153, 5), DetailLevel.STATEMENT);
		assertStatementNodes(dataFlowGraph, Set.of("Move", "Display", "Exec Sql Insert"),
				getExpectedOutgoingConnectionsForInsert(), getExpectedIncomingConnectionsForInsert());

		final Map<String, String> expectedConnectionsForCaller = Map.of("ALPHA_SHORT", "ALPHA", "N_5_0", "NUMB50",
				"N_0_3", "NUMB03", "P_7_2", "PACKED72", "DATUM", "DATEF");

		assertDataInterfaceMapping(dataFlowGraph, execSqlTableModule,
				Set.of("ALPHA_SHORT", "N_5_0", "N_0_3", "P_7_2", "DATUM"), expectedConnectionsForCaller, true);
	}

	@Test
	void testExecSqlDataFlowGraphNoFieldInfo() throws IOException {
		final var cobolModule = createModule(
				"execsqldataflowgraphquerytest/ExecSqlDataFlowGraphQueryTestNoFieldInfo.cbl",
				"ExecSqlDataFlowGraphQueryTestNoFieldInfo", Technology.COBOL, Type.PROGRAM);
		final var execSqlTable = createModule("IW_SQL_TEST", Technology.SQL, Type.TABLE);
		makeReadsWritesReference(cobolModule, execSqlTable, new ModuleLocation(696, 490), "STORE");

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(153, 5), DetailLevel.STATEMENT);

		assertStatementNodes(dataFlowGraph, Set.of("Move", "Display", "Exec Sql Insert"),
				getExpectedOutgoingConnectionsForInsert(), getExpectedIncomingConnectionsForInsert());

		final Map<String, String> expectedConnectionsForCaller = Map.of("ALPHA", "(dataTable)", "NUMB50", "(dataTable)",
				"NUMB03", "(dataTable)", "PACKED72", "(dataTable)", "DATEF", "(dataTable)");

		assertDataInterfaceMapping(dataFlowGraph, moduleService.getModule(execSqlTable), Set.of("(dataTable)"), expectedConnectionsForCaller,
				false);
	}

	@Test
	void testExecSqlDataFlowGraphSelect() throws IOException {
		final var cobolModule = createModule("execsqldataflowgraphquerytest/ExecSqlDataFlowGraphQueryTestSelect.cbl",
				"ExecSqlDataFlowGraphQueryTestSelect", Technology.COBOL, Type.PROGRAM);
		final var execSqlTable = createModule("IW_SQL_TEST", Technology.SQL, Type.TABLE);
		makeReadsWritesReference(cobolModule, execSqlTable, new ModuleLocation(553, 257), "READ");
		final var fieldInfoList = Arrays.asList(newFieldInfo(1, "ALPHA_SHORT"), newFieldInfo(1, "BETA_SHORT"));
		insertFieldInfo(moduleService.getModule(execSqlTable), fieldInfoList);
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(251, 5), DetailLevel.STATEMENT);

		assertDataInterfaces(dataFlowGraph, moduleService.getModule(cobolModule), new TreeSet<>(Arrays.asList("ALPHA-SHORT", "BETA-SHORT")));

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Exec Sql Select"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Exec Sql Select",
				Arrays.asList("ALPHA-SHORT", "BETA-SHORT"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Exec Sql Select", Arrays.asList("ALPHA-SHORT", "BETA-SHORT"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
	}

	@Test
	void testExecSqlDataFlowGraphDelete() throws IOException {
		final var cobolModule = createModule("execsqldataflowgraphquerytest/ExecSqlDataFlowGraphQueryTestDelete.cbl",
				"ExecSqlDataFlowGraphQueryTestDelete", Technology.COBOL, Type.PROGRAM);
		final var execSqlTable = createModule("DEL.TABLE1", Technology.SQL, Type.TABLE);
		makeReadsWritesReference(cobolModule, execSqlTable, new ModuleLocation(512, 114), "DELETE");

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(251, 5), DetailLevel.STATEMENT);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Exec Sql Delete"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Exec Sql Delete", Arrays.asList("ALPHA-SHORT"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.empty(),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
	}

	@Test
	void testExecSqlDataFlowGraphUpdate() throws IOException {
		final var cobolModule = createModule("execsqldataflowgraphquerytest/ExecSqlDataFlowGraphQueryTestUpdate.cbl",
				"ExecSqlDataFlowGraphQueryTestUpdate", Technology.COBOL, Type.PROGRAM);
		final var execSqlTable = createModule("UPD.DEPT", Technology.SQL, Type.TABLE);
		makeReadsWritesReference(cobolModule, execSqlTable, new ModuleLocation(511, 134), "UPDATE");
		final var fieldInfoList = Arrays.asList(newFieldInfo(1, "MGRNO"), newFieldInfo(2, "DEPTNO"));
		final var execSqlTableModule = moduleService.getModule(execSqlTable);
		insertFieldInfo(execSqlTableModule, fieldInfoList);

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(251, 5), DetailLevel.STATEMENT);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Exec Sql Update"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Exec Sql Update", Arrays.asList("ALPHA-SHORT"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Exec Sql Update", Arrays.asList("ALPHA-SHORT"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
		assertDataInterfaces(dataFlowGraph, execSqlTableModule, new TreeSet<>(Arrays.asList("MGRNO")));

		final List<DataInterfaceNode> dataInterfaces = getDataInterfaceNodes(dataFlowGraph).stream()
				.filter(n -> n.getId().contains("DATABASE_ACCESS")).toList();
		assertEquals(1, dataInterfaces.size());
		final List<ProxyContainerPojo> proxyContainer =  dataFlowService.findProxyContainers(q -> q.ofModule(cobolModule));
		assertEquals(1, proxyContainer.size());
		assertEquals(1, proxyContainer.get(0).getFieldNodes().size());

	}

	@Test
	void testExecSqlDataFlowGraphCursor() throws IOException {
		final var cobolModule = createModule("execsqldataflowgraphquerytest/ExecSqlDataFlowGraphQueryTestCursor.cbl",
				"ExecSqlDataFlowGraphQueryTestCursor", Technology.COBOL, Type.PROGRAM);
		final var execSqlTable = createModule("EMPLOYEE", Technology.SQL, Type.TABLE);
		makeReadsWritesReference(cobolModule, execSqlTable, new ModuleLocation(512, 121), "READ");
		final var fieldInfoList = Arrays.asList(newFieldInfo(1, "LNAME"), newFieldInfo(2, "FNAME"),
				newFieldInfo(3, "PAYRATE"), newFieldInfo(4, "HOURS"));
		final ModulePojo module = moduleService.getModule(execSqlTable);
		insertFieldInfo(module, fieldInfoList);

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(251, 5), DetailLevel.STATEMENT);

		assertDataInterfaces(dataFlowGraph, module, new TreeSet<>(Arrays.asList("LNAME", "FNAME", "PAYRATE", "HOURS")));

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Exec Sql Declare Cursor"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Exec Sql Declare Cursor", Arrays.asList("HOST-VARS"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Exec Sql Declare Cursor",
				Arrays.asList("HOST-VARS", "ALPHA-SHORT"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
	}

	@Test
	@Disabled("Ignoring the flaky test, needs to be investigated and fixed as part of WMIN-12562")
	void testExecSqlDataFlowGraphStatements() throws IOException {
		final var cobolModule = createModule(
				"execsqldataflowgraphquerytest/ExecSqlDataFlowGraphQueryTestStatements.cbl",
				"ExecSqlDataFlowGraphQueryTestStatements", Technology.COBOL, Type.PROGRAM);
		final var execSqlTable1 = createModule("IW_SQL_TEST", Technology.SQL, Type.TABLE);
		makeReadsWritesReference(cobolModule, execSqlTable1, new ModuleLocation(498, 195), "READ");
		final var execSqlTable2 = createModule("EMPLOYEE", Technology.SQL, Type.TABLE);
		makeReadsWritesReference(cobolModule, execSqlTable2, new ModuleLocation(746, 148), "READ");
		makeReadsWritesReference(cobolModule, execSqlTable2, new ModuleLocation(961, 105), "DELETE");
		makeReadsWritesReference(cobolModule, execSqlTable2, new ModuleLocation(1299, 188), "READ");
		final var execSqlTable3 = createModule("UPD.DEPT", Technology.SQL, Type.TABLE);
		final var fieldInfoList1 = Arrays.asList(newFieldInfo(1, "ALPHA_SHORT"));
		final ModulePojo execSqlTableM1 = moduleService.getModule(execSqlTable1);
		insertFieldInfo(execSqlTableM1, fieldInfoList1);
		final var fieldInfoList2 = Arrays.asList(newFieldInfo(1, "LNAME"), newFieldInfo(2, "FNAME"),
				newFieldInfo(3, "PAYRATE"), newFieldInfo(4, "HOURS"), newFieldInfo(5, "DEPT"));
		insertFieldInfo(moduleService.getModule(execSqlTable2), fieldInfoList2);
		final var fieldInfoList3 = Arrays.asList(newFieldInfo(1, "MGRNO"), newFieldInfo(2, "DEPTNO"));
		final ModulePojo execSqlTableM3 = moduleService.getModule(execSqlTable3);
		insertFieldInfo(execSqlTableM3, fieldInfoList3);
		makeReadsWritesReference(cobolModule, execSqlTable3, new ModuleLocation(1111, 138), "UPDATE");

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(251, 5), DetailLevel.STATEMENT);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Exec Sql Declare Cursor", "Exec Sql Delete", "Exec Sql Update", "Exec Sql Select"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Exec Sql Delete", Arrays.asList("ALPHA-SHORT"));
		expectedConnectionsForImplOutgoing.put("Exec Sql Update", Arrays.asList("ALPHA-SHORT"));
		expectedConnectionsForImplOutgoing.put("Exec Sql Select", Arrays.asList("ALPHA-SHORT"));
		expectedConnectionsForImplOutgoing.put("Exec Sql Declare Cursor", Arrays.asList("ALPHA-SHORT"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Exec Sql Delete", Arrays.asList("ALPHA-SHORT"));
		expectedConnectionsForImplIncoming.put("Exec Sql Update", Arrays.asList("ALPHA-SHORT"));
		expectedConnectionsForImplIncoming.put("Exec Sql Select", Arrays.asList("ALPHA-SHORT"));
		expectedConnectionsForImplIncoming.put("Exec Sql Declare Cursor", Arrays.asList("ALPHA-SHORT"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
		assertDataInterfaces(dataFlowGraph, execSqlTableM1, new TreeSet<>(Arrays.asList("ALPHA_SHORT")));
		assertDataInterfaces(dataFlowGraph, execSqlTableM3, new TreeSet<>(Arrays.asList("MGRNO")));
	}

	@Test
	void testExecSqlDataFlowGraphDeleteNotPreProcessed() throws IOException {
		final var cobolModule = createModule(
				"execsqldataflowgraphquerytest/ExecSqlDataFlowGraphQueryTestDeleteNotPreProcessed.cbl",
				"ExecSqlDataFlowGraphQueryTestDeleteNotPreProcessed", Technology.COBOL, Type.PROGRAM);
		final var execSqlTable = createModule("TINVOIC", Technology.SQL, Type.TABLE);
		makeReadsWritesReference(cobolModule, execSqlTable, new ModuleLocation(2962, 255), "DELETE");
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(2388, 6), DetailLevel.STATEMENT);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Exec Sql Delete"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Exec Sql Delete", Arrays.asList("INVOIC-PO-NBR", "INVOIC-INVC-ID"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.empty(),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);

	}

	@Test
	void testDataFowGraphForTableColumn() throws IOException {
		final var cobolModule = createModule("execsqldataflowgraphquerytest/TraceTableColumn.cbl",
				"TraceTableColumn", Technology.COBOL, Type.PROGRAM);
		final var execSqlTable = createModule("ACCOUNTS", Technology.SQL, Type.TABLE);
		makeReadsWritesReference(cobolModule, execSqlTable, new ModuleLocation(434, 215), "READ");
		makeReadsWritesReference(cobolModule, execSqlTable, new ModuleLocation(798, 222), "UPDATE");
		makeReadsWritesReference(cobolModule, execSqlTable, new ModuleLocation(1064, 114), "DELETE");
		final var fieldInfoList = Arrays.asList(newFieldInfo(1, "ACCOUNT_ID"), newFieldInfo(2, "ACCOUNT_NAME"), newFieldInfo(3, "ACCOUNT_BALANCE"));
		final ModulePojo execSqlTableModule = moduleService.getModule(execSqlTable);
		insertFieldInfo(execSqlTableModule, fieldInfoList);

		/* Trace table */
		DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryServiceByModule(execSqlTable);
		assertDataInterfaces(dataFlowGraph, moduleService.getModule(execSqlTable), new TreeSet<>(Arrays.asList("ACCOUNT_ID", "ACCOUNT_NAME",
				"ACCOUNT_BALANCE")));
		Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(Set.of("Move", "Exec Sql Select", "Exec Sql Update"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		/* Trace table column */
		dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartProxyField(execSqlTable, ProxyContainerPojo.Type.DATABASE_TABLE, "ACCOUNT_NAME")
						.setQueryDirection(QueryDirection.BOTH)
						.build());
		assertDataInterfaces(dataFlowGraph, moduleService.getModule(execSqlTable), new TreeSet<>(Arrays.asList("ACCOUNT_ID", "ACCOUNT_NAME",
				"ACCOUNT_BALANCE")));
		statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(Set.of("Move", "Exec Sql Select", "Exec Sql Update"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
	}

	private <T> void assertDataInterfaces(final DataFlowGraph dataFlowGraph, final ModulePojo module,
			final TreeSet<String> expectedStringSet) {

		final Set<ModuleNode> moduleNodes = getModuleNodes(dataFlowGraph);

		final Map<String, DataFlowGraphNode> nodeMap = dataFlowGraph.getNodes().stream()
				.collect(Collectors.toMap(DataFlowGraphNode::getId, Function.identity()));

		final ModuleNode callerModuleNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(module.getId()))
				.findAny().get();

		final List<DataFlowGraphNode> callerDataInterfaces = callerModuleNode.getDataInterfaces().stream()
				.map(nodeMap::get).collect(Collectors.toList());
		final TreeSet<String> actual = callerDataInterfaces.stream().map(DataFlowGraphNode::getName)
				.collect(Collectors.toCollection(TreeSet::new));
		assertIterableEquals(expectedStringSet, actual);
	}

	private <T> void assertDataInterfaceMapping(final DataFlowGraph dataFlowGraph, final ModulePojo module,
			final Set<String> expectedStringSet, final Map<String, String> expectedConnectionsForCaller,
			final boolean isFieldInfoPresent) {

		final Set<ModuleNode> moduleNodes = getModuleNodes(dataFlowGraph);

		final Map<String, DataFlowGraphNode> nodeMap = dataFlowGraph.getNodes().stream()
				.collect(Collectors.toMap(DataFlowGraphNode::getId, Function.identity()));

		final ModuleNode callerModuleNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(module.getId()))
				.findAny().get();

		final List<DataFlowGraphNode> callerDataInterfaces = callerModuleNode.getDataInterfaces().stream()
				.map(nodeMap::get).collect(Collectors.toList());

		assertEquals(expectedStringSet,
				callerDataInterfaces.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
		for (final DataFlowGraphNode callerDataInterface : callerDataInterfaces) {
			final String name = callerDataInterface.getName();
			if (isFieldInfoPresent) {
				callerDataInterface.getIncomings().stream().map(nodeMap::get).findAny().ifPresentOrElse(
						n -> assertEquals(expectedConnectionsForCaller.get(name), n.getName()),
						() -> fail(name + " field not linked to stored procedure (incoming)"));
			} else {
				callerDataInterface.getIncomings().stream().map(nodeMap::get).findAny().ifPresentOrElse(
						n -> assertEquals(expectedConnectionsForCaller.get(n.getName()), name),
						() -> fail(name + " field not linked to stored procedure (incoming)"));
			}
		}
	}

	private <T> void assertStatementNodes(final DataFlowGraph dataFlowGraph, final Set<String> expectedStatementSet,
			final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing,
			final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming) {

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(expectedStatementSet,
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);

	}

	private <T> ListValuedMap<String, List<String>> getExpectedOutgoingConnectionsForInsert() {

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("ALPHA"));
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("NUMB50"));
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("NUMB03"));
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("PACKED72"));
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("DATEF"));
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("BEFOR"));
		expectedConnectionsForImplOutgoing.put("Exec Sql Insert",
				Arrays.asList("ALPHA", "NUMB50", "NUMB03", "PACKED72", "DATEF"));

		return expectedConnectionsForImplOutgoing;
	}

	private <T> ListValuedMap<String, List<String>> getExpectedIncomingConnectionsForInsert() {

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", Arrays.asList("12345"));
		expectedConnectionsForImplIncoming.put("Move", Arrays.asList("'0.321'"));
		expectedConnectionsForImplIncoming.put("Move", Arrays.asList("123.456"));
		expectedConnectionsForImplIncoming.put("Move", Arrays.asList("HOST-VARS"));
		expectedConnectionsForImplIncoming.put("Display", Arrays.asList("BEFOR", "PACKED72"));
		expectedConnectionsForImplIncoming.put("Exec Sql Insert",
				Arrays.asList("ALPHA", "NUMB50", "NUMB03", "PACKED72", "DATEF"));
		return expectedConnectionsForImplIncoming;
	}

	private void insertFieldInfo(final ModulePojo module, final List<FieldInfoPojoPrototype> fieldInfo) {
		final EntityId recordId = module.identity();
		for (int i = 0; i < fieldInfo.size(); i++) {
			fieldInfoService.create(fieldInfo.get(i)
										.setModule(recordId));
		}
	}

	private FieldInfoPojoPrototype newFieldInfo(final int ordinal, final String name) {
		return new FieldInfoPojoPrototype()
					.setOrdinal(ordinal)
					.setName(name);
	}
}
