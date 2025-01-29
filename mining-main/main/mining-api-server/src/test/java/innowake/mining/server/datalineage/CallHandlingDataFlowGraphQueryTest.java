/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.collection.IsEmptyCollection.empty;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.DataInterfaceNode;
import innowake.mining.shared.model.datalineage.graph.FieldNode;
import innowake.mining.shared.model.datalineage.graph.ModuleNode;
import innowake.mining.shared.model.datalineage.graph.StatementNode;
import org.apache.commons.collections4.ListValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.junit.jupiter.api.Test;

import com.google.common.collect.Iterables;

import innowake.mining.server.datalineage.query.DataFlowGraphQuery;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for {@link DataFlowGraphQuery} related to Call handling
 */
class CallHandlingDataFlowGraphQueryTest extends BaseDataLineageTest {
	
	@Test
	void statementAndGroupFieldsTest() throws IOException {
		final EntityId statementAndGroupFieldsA = createModule("callhandlingdataflowgraphquerytest/StatementAndGroupFieldsA.cbl", "StatementAndGroupFieldsA",
				Technology.COBOL, Type.PROGRAM);
		final EntityId display2 = createModule("callhandlingdataflowgraphquerytest/Display2.cbl", "Display2", Technology.COBOL, Type.PROGRAM);
		makeCallReference(statementAndGroupFieldsA, display2, new ModuleLocation(777, 79));
		
		DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(statementAndGroupFieldsA, new ModuleLocation(265, 13), DetailLevel.STATEMENT);
		
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		
		assertEquals(6, statementNodes.size());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertEquals(3, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", List.of("A-GROUP-FIELD"));
		expectedConnectionsForImplIncoming.put("Call", List.of("A-GROUP-FIELD", "ANOTHER-GROUP-FIELD"));
		expectedConnectionsForImplIncoming.put("Display", List.of("PARAMETER-FIELD"));
		expectedConnectionsForImplIncoming.put("Display", List.of("PARAMETER-FIELD-2"));
		expectedConnectionsForImplIncoming.put("Display", List.of("MY-PROGRAM-NAME"));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", List.of("A-GROUP-FIELD"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("ANOTHER-GROUP-FIELD"));
		/* The duplication of A-GROUP-FIELD and ANOTHER-GROUP-FIELD is because they represent both Field and Data Interfaces */
		expectedConnectionsForImplOutgoing.put("Call", List.of("A-GROUP-FIELD", "ANOTHER-GROUP-FIELD", "A-GROUP-FIELD", "ANOTHER-GROUP-FIELD"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);
		
		dataFlowGraph = invokeDataFlowGraphQueryService(statementAndGroupFieldsA, new ModuleLocation(265, 13), DetailLevel.FIELD);
		final List<String> fieldNodes = getFieldNodes(dataFlowGraph).stream().map(FieldNode::getName).sorted().collect(Collectors.toList());
		assertEquals(5, fieldNodes.size());
		assertEquals(List.of("A-GROUP-FIELD", "ANOTHER-GROUP-FIELD", "MY-PROGRAM-NAME", "PARAMETER-FIELD", "PARAMETER-FIELD-2"), fieldNodes);
	}
	
	@Test
	void call8Display1Test() throws IOException {
		final EntityId call8Display1A = createModule("callhandlingdataflowgraphquerytest/Call8Display1A.cbl", "Call8Display1A", Technology.COBOL, Type.PROGRAM);
		final EntityId call8Display1B = createModule("callhandlingdataflowgraphquerytest/Call8Display1B.cbl", "Call8Display1B", Technology.COBOL, Type.PROGRAM);
		makeCallReference(call8Display1B, call8Display1A, new ModuleLocation(455, 40));
		makeCallReference(call8Display1B, call8Display1A, new ModuleLocation(498, 40));
		makeCallReference(call8Display1B, call8Display1A, new ModuleLocation(541, 40));
		makeCallReference(call8Display1B, call8Display1A, new ModuleLocation(584, 40));
		makeCallReference(call8Display1B, call8Display1A, new ModuleLocation(627, 40));
		makeCallReference(call8Display1B, call8Display1A, new ModuleLocation(677, 40));
		makeCallReference(call8Display1B, call8Display1A, new ModuleLocation(727, 40));
		makeCallReference(call8Display1B, call8Display1A, new ModuleLocation(777, 40));
		
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(call8Display1B, new ModuleLocation(132, 5), DetailLevel.STATEMENT);
		
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		
		assertEquals(9, statementNodes.size());
		assertEquals(8, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-1"));
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-2"));
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-3"));
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-4"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "WS-FIELD-B-1"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "WS-FIELD-B-2"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "WS-FIELD-B-3"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "WS-FIELD-B-4"));
		expectedConnectionsForImplIncoming.put("Display", List.of("G1", "G2"));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		/* The duplication in the below values is because it represents both Field and Data Interfaces */
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-1", "WS-FIELD-A-1"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-2", "WS-FIELD-A-2"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-3", "WS-FIELD-A-3"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-4", "WS-FIELD-A-4"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY", "WS-FIELD-B-1", "WS-FIELD-B-1"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY", "WS-FIELD-B-2", "WS-FIELD-B-2"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY", "WS-FIELD-B-3", "WS-FIELD-B-3"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY", "WS-FIELD-B-4", "WS-FIELD-B-4"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph); 
	}
	
	@Test
	void call16Display1Test() throws IOException {
		final EntityId call16Display1A = createModule("callhandlingdataflowgraphquerytest/Call16Display1A.cbl", "Call16Display1A", Technology.COBOL, Type.PROGRAM);
		final EntityId call16Display1B = createModule("callhandlingdataflowgraphquerytest/Call16Display1B.cbl", "Call16Display1B", Technology.COBOL, Type.PROGRAM);
		makeCallReference(call16Display1B, call16Display1A, new ModuleLocation(457, 41));
		makeCallReference(call16Display1B, call16Display1A, new ModuleLocation(592, 41));
		makeCallReference(call16Display1B, call16Display1A, new ModuleLocation(730, 41));
		makeCallReference(call16Display1B, call16Display1A, new ModuleLocation(868, 41));
		makeCallReference(call16Display1B, call16Display1A, new ModuleLocation(1006, 48));
		makeCallReference(call16Display1B, call16Display1A, new ModuleLocation(1162, 48));
		makeCallReference(call16Display1B, call16Display1A, new ModuleLocation(1310, 48));
		makeCallReference(call16Display1B, call16Display1A, new ModuleLocation(1458, 48));
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(call16Display1B, new ModuleLocation(132, 5), DetailLevel.STATEMENT);
		
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		
		assertEquals(17, statementNodes.size());
		assertEquals(16, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-1"));
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-2"));
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-3"));
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-4"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "WS-FIELD-B-1"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "WS-FIELD-B-2"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "WS-FIELD-B-3"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "WS-FIELD-B-4"));
		expectedConnectionsForImplIncoming.put("Display", List.of("G1", "G2"));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		/* The duplication in the below values is because it represents both Field and Data Interfaces */
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-1", "WS-FIELD-A-1"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-2", "WS-FIELD-A-2"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-3", "WS-FIELD-A-3"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-4", "WS-FIELD-A-4"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY", "WS-FIELD-B-1", "WS-FIELD-B-1"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY", "WS-FIELD-B-2", "WS-FIELD-B-2"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY", "WS-FIELD-B-3", "WS-FIELD-B-3"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY", "WS-FIELD-B-4", "WS-FIELD-B-4"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph); 
	}
	
	@Test
	void call2Display1Test() throws IOException {
		final EntityId call2Display1A = createModule("callhandlingdataflowgraphquerytest/Call2Display1A.cbl", "Call2Display1A", Technology.COBOL, Type.PROGRAM);
		final EntityId call2Display1B = createModule("callhandlingdataflowgraphquerytest/Call2Display1B.cbl", "Call2Display1B", Technology.COBOL, Type.PROGRAM);
		makeCallReference(call2Display1B, call2Display1A, new ModuleLocation(220, 41));
		
		/* G2 - STATEMENT */
		DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(call2Display1A, new ModuleLocation(209, 2), DetailLevel.STATEMENT);
		Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		
		/* Here two statements are coming 1 is Procedure division */
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertStatements(dataFlowGraph, statementNodes);

		/* G2 - FIELD */
		dataFlowGraph = invokeDataFlowGraphQueryService(call2Display1A, new ModuleLocation(209, 2), DetailLevel.FIELD);
		final List<String> fieldNodes = getFieldNodes(dataFlowGraph).stream().map(FieldNode::getName).sorted().collect(Collectors.toList());
		assertEquals(List.of("DUMMY", "FIELD1","G1", "G2"), fieldNodes);
		
		/* G2 - MODULE */
		dataFlowGraph = invokeDataFlowGraphQueryService(call2Display1A, new ModuleLocation(209, 2), DetailLevel.MODULE);
		final List<String> moduleNodes = getModuleNodes(dataFlowGraph).stream().map(ModuleNode::getName).sorted().collect(Collectors.toList());
		assertEquals(2, moduleNodes.size());
		assertEquals(List.of("Call2Display1A", "Call2Display1B"), moduleNodes);
		
		/* G1 - STATEMENT */
		dataFlowGraph = invokeDataFlowGraphQueryService(call2Display1A, new ModuleLocation(189, 2), DetailLevel.STATEMENT);
		statementNodes = getStatementNodes(dataFlowGraph);
		/* Here two statements are coming 1 is Procedure division */
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertStatements(dataFlowGraph, statementNodes);

		/* DUMMY */
		dataFlowGraph = invokeDataFlowGraphQueryService(call2Display1B, new ModuleLocation(132, 5), DetailLevel.STATEMENT);
		statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(3, statementNodes.size());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());

		ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("G1", "G2"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "FIELD1"));

		ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		/* The duplication of DUMMY and FIELD1 is because they represent both Field and Data Interfaces */
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY", "FIELD1", "FIELD1"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);
		
		/* FIELD1 */
		dataFlowGraph = invokeDataFlowGraphQueryService(call2Display1B, new ModuleLocation(170, 6), DetailLevel.STATEMENT);
		statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(3, statementNodes.size());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("G1", "G2"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "FIELD1"));
		
		expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		/* The duplication of DUMMY and FIELD1 is because they represent both Field and Data Interfaces */
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY", "FIELD1", "FIELD1"));
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);
	}

	/**
	 * Tests whether DataFlowNodes are deleted when module is deleted.
	 *
	 * @throws IOException while creating module
	 */
	@Test
	void call1Display3Move2Test() throws IOException {
		final EntityId call1Display3Move2A = createModule("callhandlingdataflowgraphquerytest/Call1Display3Move2A.cbl", "Call1Display3Move2A", Technology.COBOL,
				Type.PROGRAM);
		final EntityId call1Display3Move2B = createModule("callhandlingdataflowgraphquerytest/Call1Display3Move2B.cbl", "Call1Display3Move2B", Technology.COBOL,
				Type.PROGRAM);
		makeCallReference(call1Display3Move2A, call1Display3Move2B, new ModuleLocation(478, 56));
		
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(call1Display3Move2A, new ModuleLocation(241, 7), DetailLevel.STATEMENT);
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(6, statementNodes.size());
		assertEquals(3, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", List.of("A-FIELD"));
		expectedConnectionsForImplIncoming.put("Call", List.of("A-FIELD", "ANOTHER-FIELD"));
		expectedConnectionsForImplIncoming.put("Display", List.of("PARAMETER-FIELD"));
		expectedConnectionsForImplIncoming.put("Display", List.of("PARAMETER-FIELD-2"));
		expectedConnectionsForImplIncoming.put("Display", List.of("MY-PROGRAM-NAME"));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", List.of("A-FIELD"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("ANOTHER-FIELD"));
		/* The duplication of A-FIELD and ANOTHER-FIELD is because they represent both Field and Data Interfaces */
		expectedConnectionsForImplOutgoing.put("Call", List.of("A-FIELD", "ANOTHER-FIELD", "A-FIELD", "ANOTHER-FIELD"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);
		
		final List<DataFlowNodePojo> nodesForAModule = dataFlowService.find(q -> q.ofModule(call1Display3Move2A));
		assertThat(nodesForAModule, is(not(empty())));
		final List<DataFlowNodePojo> nodesForBModule = dataFlowService.find(q -> q.ofModule(call1Display3Move2B));
		assertThat(nodesForBModule, is(not(empty())));
		assertThat(Iterables.all(nodesForBModule,
				n -> Iterables.all(dataFlowService.find(q -> q.withRelationshipFrom(n.getId(), DataFlowNodeRelationshipType.RELATED_FIELD)), f -> ! f.getModuleId().equals(call1Display3Move2A))), is(Boolean.FALSE));

		moduleService.deleteModule(call1Display3Move2A, true);
		final List<DataFlowNodePojo> nodesForAModule2 = dataFlowService.find(q -> q.ofModule(call1Display3Move2A));
		assertThat(nodesForAModule2, is(empty()));
		final List<DataFlowNodePojo> nodesForBModule2 = dataFlowService.find(q -> q.ofModule(call1Display3Move2B));
		assertThat(Iterables.all(nodesForBModule2, n -> ! n.isTraced()), is(true));
		assertThat(Iterables.all(nodesForBModule2,
				n -> Iterables.all(dataFlowService.find(q -> q.withRelationshipFrom(n.getId(), DataFlowNodeRelationshipType.RELATED_FIELD)), f -> ! f.getModuleId().equals(call1Display3Move2A))), is(Boolean.TRUE));
	}
	
	/**
	 * Tests whether DataFlowNodes are deleted when {@link innowake.mining.shared.access.ModuleService#deleteModules(BuildingConsumer)} is called.
	 *
	 * @throws IOException on running data-flow-graph-query
	 */
	@Test
	void dataFlowNodeDeleteTest() throws IOException {
		final EntityId deleteAllProjectId2 = projectService.create(new ProjectPojoPrototype()
				.setClient(EntityId.of(2L))
				.setName("Delete-all-modules-data-flow-nodes-test-project-2")
				.setNatures(Collections.emptySet())
			).identity();
		final EntityId dataFlowNodeDeleteA = createModule("callhandlingdataflowgraphquerytest/DataFlowNodeDeleteA.cbl", "DataFlowNodeDeleteA", Technology.COBOL,
				Type.PROGRAM, deleteAllProjectId2);
		final EntityId dataFlowNodeDeleteB = createModule("callhandlingdataflowgraphquerytest/DataFlowNodeDeleteB.cbl", "DataFlowNodeDeleteB", Technology.COBOL,
				Type.PROGRAM, deleteAllProjectId2);
		makeCallReference(dataFlowNodeDeleteA, dataFlowNodeDeleteB, new ModuleLocation(478, 55));
		
		invokeDataFlowGraphQueryService(dataFlowNodeDeleteA, new ModuleLocation(265, 13), DetailLevel.STATEMENT, deleteAllProjectId2);
		
		final List<DataFlowNodePojo> nodesA = dataFlowService.find(q -> q.ofModule(dataFlowNodeDeleteA));
		assertThat(nodesA, is(not(empty())));
		final List<DataFlowNodePojo> nodesB = dataFlowService.find(q -> q.ofModule(dataFlowNodeDeleteB));
		assertThat(nodesB, is(not(empty())));
		moduleService.deleteModules(deleteAllProjectId2, true, true);
		final List<DataFlowNodePojo> nodesA2 = dataFlowService.find(q -> q.ofModule(dataFlowNodeDeleteA));
		assertThat(nodesA2, is(empty()));
		final List<DataFlowNodePojo> nodesB2 = dataFlowService.find(q -> q.ofModule(dataFlowNodeDeleteB));
		assertThat(nodesB2, is(empty()));
	}
	
	/**
	 * Tests race conditions where groups were not connected correctly
	 * @throws IOException when create module fails
	 */
	@Test
	void callWithParamAndMoveTest() throws IOException {
		final EntityId callWithParamAndMoveA = createModule("callhandlingdataflowgraphquerytest/CallWithParamAndMoveA.cbl", "CallWithParamAndMoveA",
				Technology.COBOL, Type.PROGRAM);
		final EntityId callWithParamAndMoveB = createModule("callhandlingdataflowgraphquerytest/CallWithParamAndMoveB.cbl", "CallWithParamAndMoveB",
				Technology.COBOL, Type.PROGRAM);
		makeCallReference(callWithParamAndMoveA, callWithParamAndMoveB, new ModuleLocation(189, 48));
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(callWithParamAndMoveB, new ModuleLocation(155, 5), DetailLevel.STATEMENT);
	
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		/* Procedure division is also coming as a Statement node */
		
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", List.of("G1-F1"));
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-1"));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", List.of("G1-F2"));
		/* The duplication of WS-FIELD-A-1 is because it represents both Field and Data Interfaces */
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-1", "WS-FIELD-A-1"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);
	}
	
	/**
	 * Tests race conditions where groups were not connected correctly
	 * @throws IOException when create module fails
	 */
	@Test
	void call1Move1Test() throws IOException {
		final EntityId call1Move1A = createModule("callhandlingdataflowgraphquerytest/Call1Move1A.cbl", "Call1Move1A", Technology.COBOL, Type.PROGRAM);
		final EntityId call1Move1B = createModule("callhandlingdataflowgraphquerytest/Call1Move1B.cbl", "Call1Move1B", Technology.COBOL, Type.PROGRAM);
		makeCallReference(call1Move1A, call1Move1B, new ModuleLocation(189, 38));
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(call1Move1A, new ModuleLocation(131, 5), DetailLevel.STATEMENT);
		
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		
		assertEquals(2, statementNodes.size());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-4"));
		expectedConnectionsForImplIncoming.put("Move", List.of("G1-F1"));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		/* The duplication of WS-FIELD-A-4 is because it represents both Field and Data Interfaces */
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-4", "WS-FIELD-A-4"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("G1-F2"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);
	}
	
	@Test
	void testSameVariableName() throws IOException {
		final EntityId sameVariableNameA = createModule("callhandlingdataflowgraphquerytest/SameVariableNameA.cbl", "SameVariableNameA", Technology.COBOL,
				Type.PROGRAM);
		final EntityId sameVariableNameB = createModule("callhandlingdataflowgraphquerytest/SameVariableNameB.cbl", "SameVariableNameB", Technology.COBOL,
				Type.PROGRAM);
		makeCallReference(sameVariableNameA, sameVariableNameB, new ModuleLocation(181, 27));
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(sameVariableNameA, new ModuleLocation(133, 5), DetailLevel.STATEMENT);
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		
		assertEquals(2, statementNodes.size());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("DUMMY"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY"));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		/* The duplication of WS-FIELD-A-1 is because it represents both Field and Data Interfaces */
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "DUMMY"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);
	}

	@Test
	void testCallWithSameParam() throws IOException {
		final EntityId link01Module = createModule("callhandlingdataflowgraphquerytest/LINK01.cbl", "LINK01", Technology.COBOL,
				Type.PROGRAM);
		final EntityId link02Module = createModule("callhandlingdataflowgraphquerytest/LINK02.cbl", "LINK02", Technology.COBOL,
				Type.PROGRAM);
		makeCallReference(link01Module, link02Module, new ModuleLocation(381, 26));
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(link01Module, new ModuleLocation(402, 1), DetailLevel.STATEMENT);
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(6, statementNodes.size());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertEquals(3, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Call", List.of("G1"));
		expectedConnectionsForImplIncoming.put("Move", List.of("G1-F1"));
		expectedConnectionsForImplIncoming.put("Move", List.of("G2-F1"));
		expectedConnectionsForImplIncoming.put("Display", List.of("WS-FIELD"));
		expectedConnectionsForImplIncoming.put("Display", List.of("WS-FIELD"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Call", List.of("G1", "G1", "G1"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("G1-F1"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);
	}

	@Test
	void testCallWithUnknownTypeModule() throws IOException {
		final EntityId unknownModule = createModule("callhandlingdataflowgraphquerytest/UNKNOWNMODULE.cbl", "UNKNOWNMODULE", Technology.COBOL,
				Type.PROGRAM);
		final EntityId calledModule = createModule(null, "UNKNOWNUTILITY", Technology.UNKNOWN, Type.UNKNOWN);
		makeCallReference(unknownModule, calledModule, new ModuleLocation(324, 31));
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(unknownModule, new ModuleLocation(352, 2), DetailLevel.STATEMENT);
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(2, statementNodes.size());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Call", List.of("G1", "G1"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("G1-F1"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Call", List.of("G1"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);

		final Set<DataInterfaceNode> dataInterfaceNodes = getDataInterfaceNodes(dataFlowGraph);
		assertEquals(2, dataInterfaceNodes.size());
		assertEquals(1, dataInterfaceNodes.stream().filter(node -> node.getName().equals("(unknown)")).count());
		assertEquals(1, dataInterfaceNodes.stream().filter(node -> node.getName().equals("G1")).count());
	}

	@Test
	void testCallWithUtilityTypeModule() throws IOException {
		final EntityId unknownModule = createModule("callhandlingdataflowgraphquerytest/UTILITYMODULE.cbl", "UTILITYMODULE", Technology.COBOL,
				Type.PROGRAM);
		final EntityId calledModule = createModule(null, "CBLTDLI", Technology.UNKNOWN, Type.UTILITY);
		makeCallReference(unknownModule, calledModule, new ModuleLocation(334, 27));
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(unknownModule, new ModuleLocation(353, 1), DetailLevel.STATEMENT);
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(2, statementNodes.size());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Call")).count());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Call", List.of("A", "A", "B", "B", "C", "C"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("A"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Call", List.of("A", "B", "C"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);

		final Set<DataInterfaceNode> dataInterfaceNodes = getDataInterfaceNodes(dataFlowGraph);
		assertEquals(4, dataInterfaceNodes.size());
		assertEquals(1, dataInterfaceNodes.stream().filter(node -> node.getName().equals("(unknown)")).count());
		assertEquals(1, dataInterfaceNodes.stream().filter(node -> node.getName().equals("A")).count());
		assertEquals(1, dataInterfaceNodes.stream().filter(node -> node.getName().equals("B")).count());
		assertEquals(1, dataInterfaceNodes.stream().filter(node -> node.getName().equals("C")).count());
	}

	private static void assertStatements(final DataFlowGraph dataFlowGraph, final Set<StatementNode> statementNodes) {
		final Map<String, DataFlowGraphNode> nodeMap = dataFlowGraph.getNodes().stream()
				.collect(Collectors.toMap(DataFlowGraphNode::getId, Function.identity()));

		statementNodes.stream().filter(node -> node.getName().equals("Display")).forEach(node -> {
			assertThat(List.of("G1", "G2"), containsInAnyOrder(node.getIncomings().stream().map(nodeMap::get).map(DataFlowGraphNode::getName).toArray()));
			assertEquals(0, node.getOutgoings().size());
		});

		statementNodes.stream().filter(node -> node.getName().equals("Call")).forEach(node -> {
			final ModuleLocation location = node.getLocation();
			if (location != null && location.getOffset() == 220) {
				assertThat(List.of("DUMMY", "FIELD1", "DUMMY", "FIELD1"), containsInAnyOrder(
						node.getOutgoings().stream().map(nodeMap::get).map(DataFlowGraphNode::getName).toArray()));
			} else {
				assertThat(List.of("DUMMY", "FIELD1"), containsInAnyOrder(node.getOutgoings().stream().map(nodeMap::get)
						.map(DataFlowGraphNode::getName).toArray()));
			}
			assertThat(List.of("DUMMY", "FIELD1"), containsInAnyOrder(node.getIncomings().stream().map(nodeMap::get)
					.map(DataFlowGraphNode::getName).toArray()));
		});
	}

}
