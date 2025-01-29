/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo;
import org.junit.jupiter.api.Test;

import innowake.mining.server.datalineage.query.DataFlowGraphQuery;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.DataFlowError;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.FieldNode;
import innowake.mining.shared.model.datalineage.graph.StatementNode;

/**
 * Tests for {@link DataFlowGraphQuery} handling Error Markers
 */
class ErrorHandlingDataFlowGraphQueryTest extends BaseDataLineageTest {

	@Test
	void testStatementNodeErrorMarkers() throws IOException {
		final EntityId readsWritesErrorModule = createModule("errorhandlingdataflowgraphquerytest/ReadsWritesError.cbl", "ReadsWritesError", Technology.COBOL,
				Type.PROGRAM);
		final EntityId readsWritesErrorModuleTable = createModule("IW_SQL_TEST", Technology.SQL, Type.TABLE);
		makeReadsWritesReference(readsWritesErrorModule, readsWritesErrorModuleTable, new ModuleLocation(514, 232), "READ");
		
		final DataFlowGraph dataFlowGraph = invokeDataFlowGraphQueryService(readsWritesErrorModule, new ModuleLocation(251, 5), DetailLevel.STATEMENT);
		
		final List<StatementNode> statementNodes = getStatementNodes(dataFlowGraph).stream().collect(Collectors.toList());
		assertEquals(1, statementNodes.size());
		assertEquals("Exec Sql Select", statementNodes.get(0).getName());
		final DataFlowError error = statementNodes.get(0).getErrors().iterator().next();
		assertEquals(DataFlowErrorPojo.Severity.ERROR, error.getSeverity());
		assertTrue(error.getText().contains("Unable to find the corresponding ReadsWrites to the location of this StatementNode"));
		
		final List<String> fieldNodes = getFieldNodes(dataFlowGraph).stream().map(FieldNode::getName).collect(Collectors.toList());
		Collections.sort(fieldNodes);
		assertEquals(1, fieldNodes.size());
		assertEquals(Arrays.asList("ALPHA-SHORT"), fieldNodes);
	}
}

