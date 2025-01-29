/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.datalineage;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.server.datalineage.query.DataFlowGraphQuery;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.server.datalineage.query.QueryDirection;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.FieldNode;
import innowake.mining.shared.model.datalineage.graph.StatementNode;
import org.apache.commons.collections4.ListValuedMap;
import org.apache.commons.collections4.multimap.ArrayListValuedHashMap;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Tests for {@link DataFlowGraphQuery} having dataFlow related to Call handling of copybook. 
 */
class CopybookDataFlowGraphQueryTest extends BaseDataLineageTest {

	/**
	 * Test for the CopyBook and display statements in it.
	 * @throws IOException when create module fails
	 */
	@Test
	void testCopybookFieldsWithDisplayStatement() throws IOException {
		/* Create Modules */
		final var cobolModule = createModule("copybookdataflowgraphquerytest/CopybookWithDisplayStmts.cbl", "CopybookWithDisplayStmts",
				Technology.COBOL, Type.PROGRAM);
		final var copybookModule = createModule("copybookdataflowgraphquerytest/copybook/Copybook.cpy", "Copybook", Technology.COBOL, Type.COPYBOOK);
		makeIncludesReference(cobolModule, copybookModule);

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(184, 1));

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Display"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("A"));
		expectedConnectionsForImplIncoming.put("Display", List.of("B"));
		expectedConnectionsForImplIncoming.put("Display", List.of("C"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.empty(),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);

		final Set<FieldNode> fieldNode = getFieldNodes(dataFlowGraph);

		assertEquals(Set.of("A", "B", "C"),
				fieldNode.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
	}
	
	/**
	 * Test for the CopyBook and display statements in it invoking traceAll.
	 * @throws IOException when create module fails.
	 */
	@Test
	void testCopybookFieldsInvokingTraceAll() throws IOException {
		/* Create Modules */
		final var cobolModule = createModule("copybookdataflowgraphquerytest/CopybookTraceAll.cbl", "CopybookTraceAll",
				Technology.COBOL, Type.PROGRAM);
		final var copybookModule = createModule("copybookdataflowgraphquerytest/copybook/CopybookFields.cpy", "CopybookFields", Technology.COBOL, Type.COPYBOOK);
		makeIncludesReference(cobolModule, copybookModule);

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryServiceByModule(cobolModule);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Display"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("A"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.empty(),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);

		final Set<FieldNode> fieldNode = getFieldNodes(dataFlowGraph);

		assertEquals(Set.of("A"),
				fieldNode.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
	}
	
	
	/**
	 * Test for traceAll and traceField invoked on the CopyBook.
	 * @throws IOException when create module fails.
	 */
	@Test
	void testTraceCopybookFields() throws IOException {
		/* Create Modules */
		final var cobolModule1 = createModule("copybookdataflowgraphquerytest/CobolModule1.cbl", "CobolModule1", Technology.COBOL, Type.PROGRAM);
		final var cobolModule2 = createModule("copybookdataflowgraphquerytest/CobolModule2.cbl", "CobolModule2", Technology.COBOL, Type.PROGRAM);
		final var copybookModule = createModule("copybookdataflowgraphquerytest/copybook/CopybookWithMultipleReferences.cpy", "CopybookWithMultipleReferences", 
				Technology.COBOL, Type.COPYBOOK);
		makeIncludesReference(cobolModule1, copybookModule);
		makeIncludesReference(cobolModule2, copybookModule);

		/* TraceAll */
		DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryServiceByModule(copybookModule);
		
		Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		
		assertEquals(4, statementNodes.size());
		assertEquals(Set.of("Display", "Move"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
		
		ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("B"));
		expectedConnectionsForImplIncoming.put("Display", List.of("Test"));
		expectedConnectionsForImplIncoming.put("Move", List.of("HIGH-VALUES"));
		
		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", List.of("B"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
		
		Set<FieldNode> fieldNode = getFieldNodes(dataFlowGraph);
		
		assertEquals(Set.of("B", "Test", "HIGH-VALUES"),
				fieldNode.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
		
		/* Field Tracing */
		
		dataFlowGraph = invokeDataFlowGraphQueryService(copybookModule, new ModuleLocation(25, 0));

		statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(3, statementNodes.size());
		assertEquals(Set.of("Display", "Move"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("B"));
		expectedConnectionsForImplIncoming.put("Move", List.of("HIGH-VALUES"));
		
		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);

		fieldNode = getFieldNodes(dataFlowGraph);

		assertEquals(Set.of("B", "HIGH-VALUES"),
				fieldNode.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
	}
	
	@Test
	void testTraceCopybookFields_2() throws IOException {
		/* Create Modules */
		final var cobolModule1 = createModule("copybookdataflowgraphquerytest/TEST10a.cbl", "TEST10a", Technology.COBOL, Type.PROGRAM);
		final var cobolModule2 = createModule("copybookdataflowgraphquerytest/TEST11.cbl", "TEST11", Technology.COBOL, Type.PROGRAM);
		final var copybookModule = createModule("copybookdataflowgraphquerytest/copybook/TEST10b.cpy", "TEST10b", 
				Technology.COBOL, Type.COPYBOOK);
		makeIncludesReference(cobolModule1, copybookModule);
		makeIncludesReference(cobolModule2, copybookModule);
		
		/* Field Tracing on copybook*/
		
		DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(copybookModule, new ModuleLocation(25, 0));

		Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(4, statementNodes.size());

		/* Field Tracing */
		
		dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule2, new ModuleLocation(209, 0));

		statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(3, statementNodes.size());
	}

	/**
	 * Test for the Missing CopyBook.
	 * @throws IOException when create module fails.
	 */
	@Test
	void testCallWithRelatedFields() throws IOException {
		/* Create Modules */
		final var cobolModule = createModule("copybookdataflowgraphquerytest/MissingFile.cbl", "MissingCopybook", Technology.COBOL,
				Type.PROGRAM);

		assertThrows(Exception.class, () -> invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(183, 1), DetailLevel.STATEMENT));
	}

	/**
	 * Test for Call with the Related Fields.
	 * @throws IOException when create module fails.
	 */
	@Test
	void testCallWithRelatedFieldsCopyBook() throws IOException {
		/* Create Modules */
		final var cobolModuleA = createModule("copybookdataflowgraphquerytest/RelativeCopybook.cbl", "RelativeCopybook", Technology.COBOL,
				Type.PROGRAM);
		final var cobolModuleB = createModule("copybookdataflowgraphquerytest/CalleeCopybook.cbl", "CalleeCopybook", Technology.COBOL,
				Type.PROGRAM);
		makeCallReference(cobolModuleB, cobolModuleA, new ModuleLocation(275, 40));
		makeCallReference(cobolModuleB, cobolModuleA, new ModuleLocation(320, 40));

		DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModuleA, new ModuleLocation(198, 2), DetailLevel.STATEMENT);

		Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Call","Display"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("G1", "G2"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "FIELD1"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "FIELD2"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "FIELD1", "DUMMY", "FIELD1"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("DUMMY", "FIELD2", "DUMMY", "FIELD2"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);

		/* For different location */

		dataFlowGraph = invokeDataFlowGraphQueryService(cobolModuleA, new ModuleLocation(177, 2), DetailLevel.STATEMENT);

		statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Call", "Display"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);

		/* For different location with cobolModuleB. */

		dataFlowGraph = invokeDataFlowGraphQueryService(cobolModuleB, new ModuleLocation(124, 5), DetailLevel.STATEMENT);

		statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Call", "Display"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("G1", "G2"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "FIELD1"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "FIELD2"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);

		/* For different location with cobolModuleB. */

		dataFlowGraph = invokeDataFlowGraphQueryService(cobolModuleB, new ModuleLocation(160, 6), DetailLevel.STATEMENT);

		statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Call", "Display"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("G1", "G2"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "FIELD1"));
		expectedConnectionsForImplIncoming.put("Call", List.of("DUMMY", "FIELD2"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.empty(),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
	}

	/**
	 * Test for Display with CopyBook and related fields.
	 * @throws IOException when create module fails.
	 */
	@Test
	void testDisplayWithCopyBook() throws IOException {
		/* Create Modules */
		final var cobolModule = createModule("copybookdataflowgraphquerytest/DisplayCopybookStmts.cbl", "DisplayCopybookStmts",
				Technology.COBOL, Type.PROGRAM);
		final var copybookModule = createModule("copybookdataflowgraphquerytest/copybook/DisplayCopybook.cpy", "Copybook", Technology.COBOL, Type.COPYBOOK);
		
		makeIncludesReference(cobolModule, copybookModule);

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(268, 1), DetailLevel.STATEMENT);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Display"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Display", List.of("A", "D"));
		expectedConnectionsForImplIncoming.put("Display", List.of("B", "E"));
		expectedConnectionsForImplIncoming.put("Display", List.of("C", "F"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.empty(),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);

		final Set<FieldNode> fieldNode = getFieldNodes(dataFlowGraph);

		assertEquals(Set.of("A", "B", "C", "D", "E", "F"),
				fieldNode.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));
	}

	/**
	 * Test for Move and CALL statement with CopyBook.
	 * @throws IOException when create module fails.
	 */
	@Test
	void testMoveStatementWithCopyBook() throws IOException {
		/* Create Modules */
		final var cobolModule = createModule("copybookdataflowgraphquerytest/MoveCopybookStmts.cbl", "MoveCopybookStmts", Technology.COBOL,
				Type.PROGRAM);
		final var copybookModule = createModule("copybookdataflowgraphquerytest/copybook/MoveCopybook.cpy", "MoveCopybook", Technology.COBOL,
				Type.COPYBOOK);
		makeIncludesReference(cobolModule, copybookModule);

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(191, 12), DetailLevel.STATEMENT);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Move"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", List.of("WS-FIELD-A-1"));
		expectedConnectionsForImplIncoming.put("Move", List.of("WS-FIELD-A-2"));
		expectedConnectionsForImplIncoming.put("Move", List.of("WS-FIELD-A-3"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD-A-2"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD-A-3"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD-A-4"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
	}

	/**
	 * Test for Move statement with CopyBook.
	 * @throws IOException when create module fails.
	 */
	@Test
	void testMoveAndCallStatements() throws IOException {
		/* Create Modules */
		final var cobolModuleA = createModule("copybookdataflowgraphquerytest/MoveAndCallCopybookStmts.cbl", "MoveAndCallCopybookStmts",
				Technology.COBOL, Type.PROGRAM);
		final var cobolModuleB = createModule("copybookdataflowgraphquerytest/CallCopybookStmts.cbl", "CallCopybookStmts", Technology.COBOL,
				Type.PROGRAM);
		makeCallReference(cobolModuleA, cobolModuleB, new ModuleLocation(430, 34));

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModuleA, new ModuleLocation(191, 12), DetailLevel.STATEMENT);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Call", "Move", "Display"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", List.of("WS-FIELD-A-1"));
		expectedConnectionsForImplIncoming.put("Move", List.of("WS-FIELD-A-2"));
		expectedConnectionsForImplIncoming.put("Move", List.of("WS-FIELD-A-3"));
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-4"));
		expectedConnectionsForImplIncoming.put("Move", List.of("G1-F1"));
		expectedConnectionsForImplIncoming.put("Display", List.of("G1"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD-A-2"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD-A-3"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD-A-4"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-4", "WS-FIELD-A-4"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("G1-F4"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
	}

	/**
	 * Test for Move and Compute statement with CopyBook.
	 * @throws IOException when create module fails.
	 */
	@Test
	void testMoveAndComputeStatementCopyBook() throws IOException {
		/* Create Modules */
		final var cobolModule = createModule("copybookdataflowgraphquerytest/MoveAndComputeCopyBookStmts.cbl", "MoveAndCallCopybookStmts",
				Technology.COBOL, Type.PROGRAM);
		final var copybookModule = createModule("copybookdataflowgraphquerytest/copybook/MoveAndComputeCopyBook.cpy", "MoveAndComputeCopyBook",
				Technology.COBOL, Type.COPYBOOK);
		makeIncludesReference(cobolModule, copybookModule);

		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(cobolModule, new ModuleLocation(244, 10), DetailLevel.STATEMENT);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(Set.of("Move", "Compute"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", List.of("WS-ONE"));
		expectedConnectionsForImplIncoming.put("Compute", List.of("WS-INPUT"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-INPUT"));
		expectedConnectionsForImplOutgoing.put("Compute", List.of("WS-OUTPUT"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
	}

	@Test
	void testCallAndMoveStatementsInCopybook() throws IOException {
		/* Create Modules */
		final var start01 = createModule("copybookdataflowgraphquerytest/START01.cbl", "START01", Technology.COBOL,
				Type.PROGRAM);
		final var start02 = createModule("copybookdataflowgraphquerytest/START02.cbl", "START02", Technology.COBOL,
				Type.PROGRAM);
		final var copy01 = createModule("copybookdataflowgraphquerytest/copybook/COPY01.cpy", "COPY01", Technology.COBOL,
				Type.COPYBOOK);
		makeIncludesReference(start01, copy01);
		makeCallReference(copy01, start02, new ModuleLocation(52, 34));
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(),
				new Parameters.Builder().setProjectId(PROJECT_ID)
						.setStartModuleIds(Collections.singletonList(start01))
						.addStartField(start01, 191)
						.setDetailLevel(DetailLevel.STATEMENT)
						.setQueryDirection(QueryDirection.BOTH)
						.setStrictTracing(true) /* throw exception when nodes are not marked as traced */
						.setAssembled(true)
						.build());

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);
		assertEquals(Set.of("Call", "Move", "Display"),
				statementNodes.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet()));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplIncoming = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplIncoming.put("Move", List.of("WS-FIELD-A-1"));
		expectedConnectionsForImplIncoming.put("Move", List.of("WS-FIELD-A-2"));
		expectedConnectionsForImplIncoming.put("Move", List.of("WS-FIELD-A-3"));
		expectedConnectionsForImplIncoming.put("Call", List.of("WS-FIELD-A-4"));
		expectedConnectionsForImplIncoming.put("Move", List.of("G1-F1"));
		expectedConnectionsForImplIncoming.put("Display", List.of("G1"));

		final ListValuedMap<String, List<String>> expectedConnectionsForImplOutgoing = new ArrayListValuedHashMap<>();
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD-A-2"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD-A-3"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("WS-FIELD-A-4"));
		expectedConnectionsForImplOutgoing.put("Call", List.of("WS-FIELD-A-4", "WS-FIELD-A-4"));
		expectedConnectionsForImplOutgoing.put("Move", List.of("G1-F4"));

		assertExpectedConnectionsForIncomingAndOutgoing(Optional.of(expectedConnectionsForImplOutgoing),
				Optional.of(expectedConnectionsForImplIncoming), statementNodes, dataFlowGraph);
	}
}

