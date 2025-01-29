/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.datalineage;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.ListValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.server.datalineage.query.DataFlowGraphQuery;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.server.datalineage.query.QueryDirection;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.FieldNode;
import innowake.mining.shared.model.datalineage.graph.StatementNode;

/**
 * Tests for {@link DataFlowGraphQuery} having dataFlow within a single module. 
 */
class IntraModuleDataFlowGraphQueryTests extends BaseDataLineageTest {
	
	@Autowired
	private AstService astService;

	protected EntityId moveAndComputeStatementModule;
	protected EntityId computeStatementModule;
	protected EntityId displayStatementModule;
	protected EntityId disconnectedModule;
	protected EntityId moveAndDisplayStatementModule;
	protected EntityId moveStatementModule;
	protected EntityId moduleWithConstantFields;
	protected EntityId moduleWithConstantFields2;
	protected EntityId redefinesModule;
	protected EntityId addStatementModule;
	protected EntityId setStatementModule;
	protected EntityId offsetDuplicationModule;
	protected EntityId fillerRedefinesModule;
	protected EntityId performanceTestModule;
	protected EntityId ifStatementModule;
	protected EntityId ifStatementNestedModule;
	protected EntityId ifStatementCompModule;
	protected EntityId ifStatementCompNestedModule;
	protected EntityId mathStatementModule;
	protected EntityId moveWithNumberModule;

	protected EntityId moveWithMultipleConstantsModule;

	protected EntityId groupGraphNodes;

	protected EntityId moduleWithFunctionStatements;

	@BeforeAll
	void createTestModules() throws IOException {
		sourceService.resetCaches();
		moveAndComputeStatementModule = createModule("intramodule/MoveAndComputeStatementModule.cbl", "MoveAndComputeStatementModule",
				Technology.COBOL, Type.PROGRAM);
		computeStatementModule = createModule("intramodule/ComputeStatementModule.cbl", "ComputeStatementModule", Technology.COBOL, Type.PROGRAM);
		displayStatementModule = createModule("intramodule/DisplayStatementModule.cbl", "DisplayStatementModule", Technology.COBOL, Type.PROGRAM);
		disconnectedModule = createModule("intramodule/DisconnectedModule.cbl", "DisconnectedModule", Technology.COBOL, Type.PROGRAM);
		moveAndDisplayStatementModule = createModule("intramodule/MoveAndDisplayStatementModule.cbl", "MoveAndDisplayStatementModule",
				Technology.COBOL, Type.PROGRAM);
		moveStatementModule = createModule("intramodule/MoveStatementModule.cbl", "MoveStatementModule", Technology.COBOL, Type.PROGRAM);
		moduleWithConstantFields = createModule("intramodule/MoveWithConstantFields.cbl", "MoveWithConstantFields", Technology.COBOL, Type.PROGRAM);
		moduleWithConstantFields2 = createModule("intramodule/MoveWithConstantFields2.cbl", "MoveWithConstantFields2", Technology.COBOL, Type.PROGRAM);
		redefinesModule = createModule("intramodule/redefines.cbl", "Redefines", Technology.COBOL, Type.PROGRAM);
		addStatementModule = createModule("intramodule/AddStatement.cbl", "AddStatement", Technology.COBOL, Type.PROGRAM);
		setStatementModule = createModule("intramodule/SetStatement.cbl", "SetStatement", Technology.COBOL, Type.PROGRAM);
		offsetDuplicationModule = createModule("intramodule/OffsetDuplication.cbl", "OffsetDuplication", Technology.COBOL, Type.PROGRAM);
		fillerRedefinesModule = createModule("intramodule/FillerRedefines.cbl", "FillerRedefines", Technology.COBOL, Type.PROGRAM);
		performanceTestModule = createModule("intramodule/PerformanceTest.cbl", "PerformanceTest", Technology.COBOL, Type.PROGRAM);
		ifStatementModule = createModule("intramodule/IfStatement.cbl", "IfStatement", Technology.COBOL, Type.PROGRAM);
		ifStatementNestedModule = createModule("intramodule/IfStatementNested.cbl", "IfStatementNested", Technology.COBOL, Type.PROGRAM);
		ifStatementCompModule = createModule("intramodule/IfStatementComparison.cbl", "IfStatementComparison", Technology.COBOL, Type.PROGRAM);
		ifStatementCompNestedModule = createModule("intramodule/IfStatementComparisonNested.cbl", "IfStatementComparisonNested", Technology.COBOL, Type.PROGRAM);
		mathStatementModule = createModule("intramodule/MathStatement.cbl", "MathStatement", Technology.COBOL, Type.PROGRAM);
		moveWithNumberModule = createModule("intramodule/MoveWithNumber.cbl", "MoveWithNumber", Technology.COBOL, Type.PROGRAM);
		moveWithMultipleConstantsModule = createModule("intramodule/MoveWithMultipleConstantsToSameField.cbl", "MoveWithMultipleConstants", Technology.COBOL,
				Type.PROGRAM);
		groupGraphNodes = createModule("intramodule/GroupGraphNodes.cbl", "GroupGraphNodes", Technology.COBOL, Type.PROGRAM);
		moduleWithFunctionStatements =  createModule("intramodule/ModuleWithFunctionStatements.cbl", "ModuleWithFunctionStatements", Technology.COBOL, Type.PROGRAM);
	}
	
	@Test
	void testIntraModuleForMoveWithConstantFields() {
		/* Test with HIGH-VALUES and SPACES */
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(moduleWithConstantFields, new ModuleLocation(250, 11),
				DetailLevel.STATEMENT);
		
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		
		assertEquals(4, statementNodes.size());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Comparison")).count());
		assertEquals(3, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
		
		final List<String> fieldNodes = getFieldNodes(dataFlowGraph).stream().map(FieldNode::getName).sorted().collect(Collectors.toList());
		assertEquals(7, fieldNodes.size());
		
		assertEquals(Arrays.asList("EDR-CUR-DOC-NO", "HIGH-VALUES", "NEW-CUR-DOC-NO", "SPACES", "WS-SORT-CUR-DOC-PREFIX",
				"WS-SORT-CUR-DOC-SUFFIX", "WS-SORT-NEW-DOC-NO"), fieldNodes);
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("WS-SORT-CUR-DOC-PREFIX", "WS-SORT-CUR-DOC-SUFFIX",
				"WS-SORT-NEW-DOC-NO"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", Arrays.asList("EDR-CUR-DOC-NO", "HIGH-VALUES"));
		expectedConnectionsForImplIncoming.put("Comparison", Arrays.asList("NEW-CUR-DOC-NO", "SPACES"));
	}
	
	@Test
	void testIntraModuleForMoveWithConstantFields2() {
		/* Test LOW-VALUES, SPACE, ZERO, ZEROS, TRUE, FALSE */
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(moduleWithConstantFields2, new ModuleLocation(250, 11),
				DetailLevel.STATEMENT);
		
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		
		assertEquals(6, statementNodes.size());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Comparison")).count());
		assertEquals(5, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
		
		final List<String> fieldNodes = getFieldNodes(dataFlowGraph).stream().map(FieldNode::getName).sorted().collect(Collectors.toList());
		assertEquals(11, fieldNodes.size());
		
		assertEquals(Arrays.asList("FALSE", "LOW-VALUES", "NEW-CUR-DOC-NO", "SPACE", "TRUE", "WS-SORT-CUR-DOC-PREFIX",
				"WS-SORT-CUR-DOC-SUFFIX", "WS-SORT-NEW-DOC-NO", "WS-SPACE-FLG", "ZERO", "ZEROS"), fieldNodes);
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("WS-SORT-CUR-DOC-PREFIX", "WS-SORT-CUR-DOC-SUFFIX",
				"WS-SORT-NEW-DOC-NO", "WS-SPACE-FLG"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", Arrays.asList("FALSE", "LOW-VALUES", "TRUE", "ZERO", "ZEROS"));
		expectedConnectionsForImplIncoming.put("Comparison", Arrays.asList("NEW-CUR-DOC-NO", "SPACES"));
	}

	@Test
	void testIntraModuleForMoveAndCompute() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(moveAndComputeStatementModule, 159)
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		assertEquals(Set.of("Compute", "Move"), statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("WS-INPUT"));
		expectedConnectionsForImplOutgoing.put("Compute", Arrays.asList("WS-OUTPUT"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", Arrays.asList("WS-ONE"));
		expectedConnectionsForImplIncoming.put("Compute", Arrays.asList("WS-INPUT"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);	
	}

	@Test
	void testIntraModuleForMoveAndComputeFieldLevel() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(moveAndComputeStatementModule, 159)
						.setDetailLevel(DetailLevel.FIELD)
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(Set.of("WS-INPUT", "WS-ONE", "WS-OUTPUT"), fieldNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("WS-ONE", Arrays.asList("WS-INPUT"));
		expectedConnectionsForImplOutgoing.put("WS-INPUT", Arrays.asList("WS-OUTPUT"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("WS-OUTPUT", Arrays.asList("WS-INPUT"));
		expectedConnectionsForImplIncoming.put("WS-INPUT", Arrays.asList("WS-ONE"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				fieldNodes, dataFlowGraph);	
	}

	@Test
	void testIntraModuleForCompute() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(computeStatementModule, 123)
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		final List<StatementNode> computeNodes = statementNodes.stream().filter(node -> node.getName().equals("Compute")).collect(Collectors.toList());

		assertEquals(1, computeNodes.size());
		assertEquals("COMPUTE WS-OUTPUT = MY-HEX-CONV-NUM * 2", computeNodes.get(0).getStatementLabel());

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Compute", Arrays.asList("WS-OUTPUT"));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Compute", Arrays.asList("MY-HEX-CONV-NUM"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);
	}

	@Test
	void testIntraModuleForComputeFieldLevel() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(computeStatementModule, 123)
						.setDetailLevel(DetailLevel.FIELD)
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(Set.of("MY-HEX-CONV-NUM", "WS-OUTPUT"),
				fieldNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("MY-HEX-CONV-NUM", Arrays.asList("WS-OUTPUT"));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("WS-OUTPUT", Arrays.asList("MY-HEX-CONV-NUM"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				fieldNodes, dataFlowGraph);	
	}

	@Test
	void testIntraModuleForDisplay() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(displayStatementModule, 124)
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());

		assertEquals(Set.of("Display"), statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", Arrays.asList("G1"));
		expectedConnectionsForImplIncoming.put("Display", Arrays.asList("PARENT"));
		expectedConnectionsForImplIncoming.put("Display", Arrays.asList("FLAG-ENABLED"));
		expectedConnectionsForImplIncoming.put("Display", Arrays.asList("FLAG-DISABLED"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.empty(), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);	
	}

	@Test
	void testIntraModuleForDisplayFieldLevel() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(displayStatementModule, 124)
						.setDetailLevel(DetailLevel.FIELD)
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(Set.of("FLAG-DISABLED", "FLAG-ENABLED", "PARENT", "G1"),
				fieldNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
		
	}

	@Test
	void testIntraModuleForDisconnectedField() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(disconnectedModule, 792)
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertTrue(fieldNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()).isEmpty());
	}

	@Test
	void testIntraModuleForMoveAndDisplay() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(moveAndDisplayStatementModule, 481)
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());
		
		assertEquals(Set.of("Move"), statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("TESTNUM"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", Arrays.asList("111111"));
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);	
	}

	@Test
	void testIntraModuleForMoveAndDisplayFieldLevel() {

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(moveAndDisplayStatementModule, 481)
						.setDetailLevel(DetailLevel.FIELD)
						.setQueryDirection(QueryDirection.BOTH).setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());

		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());

		assertEquals(Set.of("TESTNUM", "111111"),
				fieldNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
	}

	@Test
	void testIntraModuleForMoveStatement() {
	
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(moveStatementModule, 165)
						.setQueryDirection(QueryDirection.BOTH)
						.setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());
		
		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());
		
		assertEquals(Set.of("Move"), statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", Arrays.asList("G1-F2"));	
		

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", Arrays.asList("G1-F1"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing), Optional.of(expectedConnectionsForImplIncoming),
				statementNodes, dataFlowGraph);
	}
	
	@Test
	void testRedefines() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(redefinesModule, 131)
						.setQueryDirection(QueryDirection.BOTH)
						.setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.build());
		
		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());
		assertEquals(4, statementNodes.size());
		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());
		assertEquals(4, fieldNodes.size());
		
		final Set<StatementNode> statements1Incoming = statementNodes.stream().filter(node -> node.getIncomings().size() == 1).collect(Collectors.toSet());
		assertEquals(4, statements1Incoming.size());
		final List<FieldNode> fieldNode3Incoming = fieldNodes.stream().filter(node -> node.getIncomings().size() == 3).collect(Collectors.toList());
		assertEquals(1, fieldNode3Incoming.size());
	}
	
	@Test
	void testCobolExpressionRoundedFlag() {
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(addStatementModule, new ModuleLocation(129, 1),
				DetailLevel.STATEMENT);
		
		final List<AstNodePojo> astNodes = astService.find(q -> q.ofModule(addStatementModule).withType("CobolExpressionWithRoundedFlag"));
		assertTrue( ! astNodes.isEmpty());
		
		final Set<String> statementNodes = getStatementNodes(dataFlowGraph).stream().map(StatementNode::getName).collect(Collectors.toSet());
		assertEquals(1, statementNodes.size());
		assertFalse("Verify that the StatementNodes does'nt have *CobolExpressionWithRoundedFlag", statementNodes.contains("*CobolExpressionWithRoundedFlag"));
	}
	
	@Test
	void testCobolMathStatements() {
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(mathStatementModule, new ModuleLocation(129, 1),
				DetailLevel.STATEMENT);
		
		final List<AstNodePojo> astNodes = astService.find(q -> q.ofModule(mathStatementModule).withType("CobolMathExpression"));
		assertTrue( ! astNodes.isEmpty());
		
		final Set<String> statementNodes = getStatementNodes(dataFlowGraph).stream().map(StatementNode::getName).collect(Collectors.toSet());
		assertEquals(1, statementNodes.size());
		assertTrue("Verify that the StatementNodes does'nt have Math Statements ", ! statementNodes.contains("Math"));
	}
	
	@Test
	void testCobolSingleSetContainer() {
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(setStatementModule, new ModuleLocation(129, 1),
				DetailLevel.STATEMENT);

		final List<AstNodePojo> astNodes = astService.find(q -> q.ofModule(setStatementModule).withType("CobolSingleSetContainer"));
		assertTrue( ! astNodes.isEmpty());
		
		final Set<String> statementNodes = getStatementNodes(dataFlowGraph).stream().map(StatementNode::getName).collect(Collectors.toSet());
		assertEquals(1, statementNodes.size());
		assertTrue("Verify that the StatementNodes does'nt have *CobolSingleSetContainer", ! statementNodes.contains("*CobolSingleSetContainer"));
	}

	@Test
	void testAstAssembledQuery() {
		final EntityId moduleId = offsetDuplicationModule;
		final boolean hasAst = executorService.executeStoreAst(PROJECT_ID, moduleId);
		assertTrue("Verify the AstNodes have been stored in database", hasAst);

		final List<AstNodePojo> astNodes = astService.find(q -> q.ofModule(moduleId)
																.withAssembledOffset(Comperator.EQUAL, 218));

		assertTrue("There should be more than 1 AstNode present in the module for given offset", astNodes.size() > 1);

		final List<AstNodePojo> filteredNodes = astService.find(q -> q.ofModule(moduleId)
																 .withAssembledOffset(Comperator.EQUAL, 218)
																 .withParent(q2 -> q2.ofModule(moduleId)
																					 .withAssembledOffset(Comperator.EQUAL, 218),
																			false));
		assertEquals(1, filteredNodes.size());
		assertEquals("A NOT NUMERIC OR B = '9'", filteredNodes.get(0).getLabel());
	}

	@Test
	void testFillerRedefines() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(fillerRedefinesModule, 206)
						.setQueryDirection(QueryDirection.BOTH)
						.setStrictTracing(true)
						.build());
		
		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());
		assertEquals(3, statementNodes.size());
		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());
		assertEquals(2, fieldNodes.size());
		
		final Set<StatementNode> statements1Incoming = statementNodes.stream().filter(node -> node.getIncomings().size() == 1).collect(Collectors.toSet());
		assertEquals(3, statements1Incoming.size());
		final List<FieldNode> fieldNode1Incoming = fieldNodes.stream().filter(node -> node.getIncomings().size() == 1).collect(Collectors.toList());
		assertEquals(2, fieldNode1Incoming.size());
		final List<FieldNode> fieldNode3Outgoing = fieldNodes.stream().filter(node -> node.getOutgoings().size() == 3).collect(Collectors.toList());
		assertEquals(1, fieldNode3Outgoing.size());
		final List<FieldNode> fieldNode2Outgoing = fieldNodes.stream().filter(node -> node.getOutgoings().size() == 2).collect(Collectors.toList());
		assertEquals(1, fieldNode2Outgoing.size());
	}
	
	@Disabled("This test is purely for performance testing and should not be used when running all tests")
	@Test
	void testPerformance() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.addStartField(performanceTestModule, 140)
						.setQueryDirection(QueryDirection.BOTH)
						.setStrictTracing(true)
						.build());
		
		final Set<StatementNode> statementNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.STATEMENT))
				.map(StatementNode.class::cast).collect(Collectors.toSet());
		assertEquals(39, statementNodes.size());
		final Set<FieldNode> fieldNodes = dataFlowGraph.getNodes().stream().filter(node -> node.getType().equals(DataFlowGraphNode.Type.FIELD))
				.map(FieldNode.class::cast).collect(Collectors.toSet());
		assertEquals(27, fieldNodes.size());
	}
	
	@Test
	void testReferenceStatements() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
				.setStartModuleIds(Collections.singletonList(ifStatementModule))
				.setDetailLevel(DetailLevel.STATEMENT)
				.setQueryDirection(QueryDirection.BOTH)
				.setStrictTracing(true)
				.build());
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(4, statementNodes.size());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Reference")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
		
		final Set<FieldNode> fieldNodes = getFieldNodes(dataFlowGraph);
		assertEquals(5, fieldNodes.size());
	}
	
	@Test
	void testNestedReferenceStatements() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
				.setStartModuleIds(Collections.singletonList(ifStatementNestedModule))
				.setDetailLevel(DetailLevel.STATEMENT)
				.setQueryDirection(QueryDirection.BOTH)
				.setStrictTracing(true)
				.build());
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(5, statementNodes.size());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Reference")).count());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Comparison")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
		
		final Set<FieldNode> fieldNodes = getFieldNodes(dataFlowGraph);
		assertEquals(5, fieldNodes.size());
	}
	
	@Test
	void testComparisonStatements() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(ifStatementCompModule))
						.setDetailLevel(DetailLevel.STATEMENT)
						.setQueryDirection(QueryDirection.BOTH)
						.setStrictTracing(true)
						.build());
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(8, statementNodes.size());
		assertEquals(4, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Comparison")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
		
		final Set<FieldNode> fieldNodes = getFieldNodes(dataFlowGraph);
		assertEquals(6, fieldNodes.size());
	}
	
	@Test
	void testNestedComparisonStatements() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(ifStatementCompNestedModule))
						.setDetailLevel(DetailLevel.STATEMENT)
						.setQueryDirection(QueryDirection.BOTH)
						.setStrictTracing(true)
						.build());
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(9, statementNodes.size());
		assertEquals(4, statementNodes.stream().filter(node -> node.getName().equals("Display")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Comparison")).count());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
		assertEquals(1, statementNodes.stream().filter(node -> node.getName().equals("Reference")).count());
		
		final Set<FieldNode> fieldNodes = getFieldNodes(dataFlowGraph);
		assertEquals(7, fieldNodes.size());
	}
	
	@Test
	void testStatementWithNumericConstant() {
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryServiceByModule(moveWithNumberModule);
		
		final Set<String> fieldNodes = getFieldNodes(dataFlowGraph).stream().map(FieldNode::getName).sorted().collect(Collectors.toSet());
		assertEquals(4, fieldNodes.size());
		assertEquals(Set.of("1", "A", "B", "ZERO"), fieldNodes);
		
		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(2, statementNodes.size());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
	}

	@Test
	void testMoveStatementsWithDifferentNumericConstant() {
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryServiceByModule(moveWithMultipleConstantsModule);

		final Set<String> fieldNodes = getFieldNodes(dataFlowGraph).stream().map(FieldNode::getName).sorted().collect(Collectors.toSet());
		assertEquals(3, fieldNodes.size());
		assertEquals(Set.of("1", "10", "A"), fieldNodes);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(2, statementNodes.size());
		assertEquals(2, statementNodes.stream().filter(node -> node.getName().equals("Move")).count());
	}

	@Test
	void testNodeGrouping() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(PROJECT_ID)
				.addStartField(groupGraphNodes, 410, null)
				.setDetailLevel(DetailLevel.STATEMENT)
				.setAssembled(false)
				.build());

		final Set<String> fieldNodes = getFieldNodes(dataFlowGraph).stream().map(FieldNode::getName).sorted().collect(Collectors.toSet());
		assertEquals(7, fieldNodes.size());

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(8, statementNodes.size());

		final List<DataFlowGraphNode> dataFlowGraphNodes = dataFlowGraph.getNodes();
		assertEquals(5, dataFlowGraphNodes.stream().filter(node -> DataFlowGraphNode.NodeDirection.INCOMING.equals(node.getDirection())).count());
		assertEquals(3,
				dataFlowGraphNodes.stream().filter(node -> DataFlowGraphNode.NodeDirection.OUTGOING.equals(node.getDirection())).count());
		assertEquals(7, dataFlowGraphNodes.stream().filter(node -> DataFlowGraphNode.NodeDirection.BOTH.equals(node.getDirection())).count());
	}

	@Test
	void testFunctionStatements() {
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryServiceByModule(moduleWithFunctionStatements);

		final Set<String> fieldNodes = getFieldNodes(dataFlowGraph).stream().map(FieldNode::getName).sorted().collect(Collectors.toSet());
		assertEquals(6, fieldNodes.size());

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(10, statementNodes.size());
		assertEquals(6, statementNodes.stream().filter(node -> node.getName().equals("Function Reference")).count());
	}

}

