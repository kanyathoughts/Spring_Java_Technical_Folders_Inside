/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import innowake.mining.server.datalineage.operations.sqlstoredprocedure.SqlStoredProcedureTracer;
import org.apache.commons.lang3.tuple.Triple;
import org.junit.jupiter.api.Test;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.server.datalineage.query.QueryDirection;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.ModuleNode;

/**
 * Tests for {@link SqlStoredProcedureTracer}.
 */
class SqlStoredProcedureDataFlowGraphTest extends BaseDataLineageTest {

	static final String CALLER_FILE = "storedprocedure/CALLPROC.cbl";
	static final String SQL_SCRIPT_FILE = "storedprocedure/CreateProcedure2.sql";
	static final String IMPL_FILE = "storedprocedure/PRGPROC2.cbl";

	/**
	 * Tests that the correct data flow graph is produced for a stored procedure module
	 */
	@Test
	void testStoredProcedureDataFlowGraph() throws IOException {
		final var testModules = createTestModules();
		final EntityId callerModule = testModules.getLeft();

		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(moduleService.getProject(callerModule))
				.setStartModuleIds(Collections.singletonList(callerModule))
				.setDetailLevel(DetailLevel.MODULE) /* only interested in the module to module interactions */
				.setQueryDirection(QueryDirection.BOTH)
				.setStrictTracing(true) /* throw exception when nodes are not marked as traced */
				.build());

		assertExpectedResult(testModules, dataFlowGraph);
	}

	private Triple<EntityId, EntityId, EntityId> createTestModules() throws IOException {
		/* the Cobol module containing the EXEC SQL CALL to the stored procedure */
		final EntityId callerModule = createModule(CALLER_FILE, "CALLPROC", Technology.COBOL, Type.PROGRAM);
		/* the SQL Stored Procedure
		 * - not 100% accurate because discovery creates the stored procedure as a sub-module of the SQL script - should work the same way though */
		final EntityId storedProcedureModule = createModule(SQL_SCRIPT_FILE, "SP_SUBSCRIBE", Technology.SQL, Type.STORED_PROCEDURE);
		/* the Cobol module that implements the stored procedure */
		final EntityId implementationModule = createModule(IMPL_FILE, "PRGPROC2", Technology.COBOL, Type.PROGRAM);

		makeCallReference(callerModule, storedProcedureModule, new ModuleLocation(343, 251));
		makeCallReference(storedProcedureModule, implementationModule);

		return Triple.of(callerModule, storedProcedureModule, implementationModule);
	}

	private void assertExpectedResult(final Triple<EntityId, EntityId, EntityId> testModules, final DataFlowGraph dataFlowGraph) {
		final EntityId callerModule = testModules.getLeft();
		final EntityId storedProcedureModule = testModules.getMiddle();
		final EntityId implementationModule = testModules.getRight();

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast)
				.collect(Collectors.toList());

		final Map<String, DataFlowGraphNode> nodeMap = dataFlowGraph.getNodes().stream()
				.collect(Collectors.toMap(DataFlowGraphNode::getId, Function.identity()));

		final ModuleNode callerModuleNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(callerModule.getNid())).findAny().get();
		final ModuleNode storedProcedureModuleNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(storedProcedureModule.getNid())).findAny().get();
		final ModuleNode implementationModuleNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(implementationModule.getNid())).findAny().get();

		final List<DataFlowGraphNode> callerDataInterfaces = callerModuleNode.getDataInterfaces().stream().map(nodeMap::get).collect(Collectors.toList());

		/* assert that all data interface fields for the caller module were created (literal arguments on the EXEC SQL CALL are ignored) */
		assertEquals(
				Set.of("ALPHA-LONG", "ALPHA-SHORT", "NUMBER-FIELD", "OTHER-FIELD"),
				callerDataInterfaces.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet())
		);

		/* assert that they are linked to the corresponding data interface fields on the stored procedure module - this linking is currently always
		* bidirectional, regardless whether the parameter is IN, OUT, INOUT. */
		final Map<String, String> expectedConnectionsForCaller = Map.of(
				"ALPHA-LONG", "RETURN_VALUE",
				"ALPHA-SHORT", "PUBLICATION",
				"NUMBER-FIELD", "ISSUE",
				"OTHER-FIELD", "OTHER"
		);

		for (final DataFlowGraphNode callerDataInterface : callerDataInterfaces) {
			final String name = callerDataInterface.getName();
			for (final String out : callerDataInterface.getOutgoings()) {
				final DataFlowGraphNode node = nodeMap.get(out);
				if (node != null) {
					assertEquals(expectedConnectionsForCaller.get(name), node.getName());
				} else {
					fail(name + " field not linked to stored procedure (outgoing)");
				}
			}
			for (final String in : callerDataInterface.getIncomings()) {
				final DataFlowGraphNode node = nodeMap.get(in);
				if (node != null) {
					assertEquals(expectedConnectionsForCaller.get(name), node.getName());
				} else {
					fail(name + " field not linked to stored procedure (incoming)");
				}
			}
		}

		final List<DataFlowGraphNode> spDataInterfaces = storedProcedureModuleNode.getDataInterfaces().stream().map(nodeMap::get).collect(Collectors.toList());

		/* assert that all data interface fields for the stored procedure are present - note that currently only the fields that are actually accessed
		 * from the caller module are added to the data flow graph */
		assertEquals(
				Set.of("RETURN_VALUE", "PUBLICATION", "ISSUE", "OTHER"),
				spDataInterfaces.stream().map(DataFlowGraphNode::getName).collect(Collectors.toSet())
		);

		final List<DataFlowGraphNode> implDataInterfaces = implementationModuleNode.getDataInterfaces().stream().map(nodeMap::get).collect(Collectors.toList());

		/* assert that all data interface fields for the implementation module were created (again,
		 * only those that are accessed from the caller are visible here) */
		assertEquals(
				Set.of("ISSUE", "OTHERP", "PUBLICATION", "RESULT"),
				implDataInterfaces.stream().map(DataFlowGraphNode::getName).sorted().collect(Collectors.toSet())
		);

		/* assert that they are linked to the corresponding data interface fields on the stored procedure module - this linking respects the IN, OUT, INOUT
		* direction of the parameter */
		final Map<String, String> expectedConnectionsForImplIncoming = Map.of(
				"PUBLICATION", "PUBLICATION",
				"ISSUE", "ISSUE",
				"OTHERP", "OTHER"
		);
		final Map<String, String> expectedConnectionsForImplOutgoing = Map.of(
				"RESULT","RETURN_VALUE"
		);

		for (final DataFlowGraphNode implDataInterface : implDataInterfaces) {
			final String name = implDataInterface.getName();
			if ( ! expectedConnectionsForImplIncoming.containsKey(name) && ! expectedConnectionsForImplOutgoing.containsKey(name)) {
				fail("Did not expect " + name + " field on implementation module to be connected");
			}
			if (expectedConnectionsForImplIncoming.containsKey(name)) {
				final String expectedConnectedField = expectedConnectionsForImplIncoming.get(name);
				implDataInterface.getIncomings().stream()
						.map(nodeMap::get)
						.findAny()
						.ifPresentOrElse(
								n  -> assertEquals(expectedConnectedField, n.getName()),
								() ->    fail(name + " field not linked to stored procedure (outgoing)")
						);
			}
			if (expectedConnectionsForImplOutgoing.containsKey(name)) {
				final String expectedConnectedField = expectedConnectionsForImplOutgoing.get(name);
				implDataInterface.getOutgoings().stream()
						.map(nodeMap::get)
						.findAny()
						.ifPresentOrElse(
								n  -> assertEquals(expectedConnectedField, n.getName()),
								() ->    fail(name + " field not linked to stored procedure (outgoing)")
						);
			}
		}
	}
}
