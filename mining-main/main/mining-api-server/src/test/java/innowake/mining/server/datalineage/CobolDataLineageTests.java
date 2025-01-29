/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.test.context.TestPropertySource;

import innowake.mining.server.datalineage.astmodel.CobolDataLineage;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.StatementNode;

/**
 * Tests for {@link CobolDataLineage}.
 */
@TestPropertySource(properties = {"configuration.enable-new-cobol-data-lineage=true"})
class CobolDataLineageTests extends BaseDataLineageTest {
	
	@Test
	void testComputeStatement() throws IOException {
		final EntityId module = createModule("cobolDataLineage/ComputeStatement.cbl", "ComputeStatement", Technology.COBOL, Type.PROGRAM);
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(module);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(1, statementNodes.size(), "it should have created one statement node for the COMPUTE statement");

		final StatementNode statementNode = statementNodes.stream().findAny().orElseThrow();

		assertReads(statementNode, dataFlowGraph,"01 NUM1 PIC 9(3) VALUE 456.", "01 NUM2 PIC 9(3) VALUE 789.");
		assertWrites(statementNode, dataFlowGraph, "01 RESULT PIC 9(4).");
	}

	@Test
	void testDisplayStatement() throws IOException {
		final EntityId module = createModule("cobolDataLineage/DisplayStatement.cbl", "DisplayStatement", Technology.COBOL, Type.PROGRAM);
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(module);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(1, statementNodes.size(), "it should have created one statement node for the DISPLAY statement");

		final StatementNode statementNode = statementNodes.stream().findAny().orElseThrow();

		assertReads(statementNode, dataFlowGraph,"01 NUMBER1 PIC 9(3) VALUE 123.");
		assertWrites(statementNode, dataFlowGraph); /* assert writes nothing */
	}

	@Test
	void testMoveStatement() throws IOException {
		final EntityId module = createModule("cobolDataLineage/MoveStatement.cbl", "MoveStatement", Technology.COBOL, Type.PROGRAM);
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(module);

		final Set<StatementNode> statementNodes = getStatementNodes(dataFlowGraph);

		assertEquals(1, statementNodes.size(), "it should have created one statement node for the MOVE statement");

		final StatementNode statementNode = statementNodes.stream().findAny().orElseThrow();

		assertReads(statementNode, dataFlowGraph,"01 NUMBER1 PIC 9(3) VALUE 123.");
		assertWrites(statementNode, dataFlowGraph, "01 NUMBER2 PIC 9(3).");
	}

	private void assertReads(final StatementNode statementNode, final DataFlowGraph graph, String... fieldNames) {
		final Set<String> readFields = graph.getNodes()
				.stream()
				.filter(node -> statementNode.getIncomings().contains(node.getId()))
				.map(DataFlowGraphNode::getName)
				.collect(Collectors.toSet());

		assertEquals(Set.of(fieldNames), readFields, statementNode.getName() + " did not read the expected fields");
	}

	private void assertWrites(final StatementNode statementNode, final DataFlowGraph graph, String... fieldNames) {
		final Set<String> readFields = graph.getNodes()
				.stream()
				.filter(node -> statementNode.getOutgoings().contains(node.getId()))
				.map(DataFlowGraphNode::getName)
				.collect(Collectors.toSet());

		assertEquals(Set.of(fieldNames), readFields, statementNode.getName() + " did not write the expected fields");
	}
}
