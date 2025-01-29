/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.datalineage;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
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
 * Tests the DataFlowGraphQuery with different maximum module distances.
 */
class MaxModuleDistanceDataLineageTest extends BaseDataLineageTest {
	static final String A_FILE = "maxmoduledistance/A_module.cbl";
	static final String B_FILE = "maxmoduledistance/B_module.cbl";
	static final String C_FILE = "maxmoduledistance/C_module.cbl";
	static final String D_FILE = "maxmoduledistance/D_module.cbl";
	
	EntityId moduleA;
	EntityId moduleB;
	EntityId moduleC;
	EntityId moduleD;

	
	/**
	 * Creates a graph like this:
	 * A--CALLS-->B--CALLS-->C--CALLS-->D
	 * |                     |
	 * ----------CALLS--------
	 * If we traverse the path ABCD we end up on the D module at a module distance of 3.
	 * If we traverse the path ACD we end up there at a distance of 3. With the changes made in WMIN-8167 the order of traversal should not change the graph.
	 */
	@BeforeAll
	public void createTestModules() throws IOException {
		moduleA = createModule(A_FILE, "A_module", Technology.COBOL, Type.PROGRAM);
		moduleB = createModule(B_FILE, "B_module", Technology.COBOL, Type.PROGRAM);
		moduleC = createModule(C_FILE, "C_module", Technology.COBOL, Type.PROGRAM);
		moduleD = createModule(D_FILE, "D_module", Technology.COBOL, Type.PROGRAM);
		makeCallReference(moduleA, moduleB, new ModuleLocation(181, 28));
		makeCallReference(moduleA, moduleC, new ModuleLocation(218, 28));
		makeCallReference(moduleB, moduleC, new ModuleLocation(219, 28));
		makeCallReference(moduleC, moduleD, new ModuleLocation(219, 28));
	}
	
	/**
	 * With a distance of 0 the graph should be empty.
	 */
	@Test
	void testDistance0() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(PROJECT_ID)
				.setStartModuleIds(Collections.singletonList(moduleA))
				.setQueryDirection(QueryDirection.BOTH)
				.setMaxModuleDistance(0)
				.setStrictTracing(true)
				.build());
		
		assertTrue(dataFlowGraph.getNodes().isEmpty(), "A maxModuleDistance of 0 should produce an empty graph.");
	}
	
	/**
	 * With a distance of 1 the graph should contain only module A.
	 */
	@Test
	void testDistance1() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(PROJECT_ID)
				.setStartModuleIds(Collections.singletonList(moduleA))
				.setQueryDirection(QueryDirection.BOTH)
				.setDetailLevel(DetailLevel.MODULE)
				.setMaxModuleDistance(1)
				.setStrictTracing(true)
				.build());
		
		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast)
				.collect(Collectors.toList());
		
		final Map<String, DataFlowGraphNode> nodeMap = dataFlowGraph.getNodes().stream()
				.collect(Collectors.toMap(DataFlowGraphNode::getId, Function.identity()));

		assertEquals(1, moduleNodes.size());
		assertEquals(moduleNodes.get(0).getModuleId(), moduleA.getNid());
	}

	/**
	 * With a distance of 2 the graph should contain A and its connection to B and C.
	 */
	@Test
	void testDistance2() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(PROJECT_ID)
				.setStartModuleIds(Collections.singletonList(moduleA))
				.setQueryDirection(QueryDirection.BOTH)
				.setDetailLevel(DetailLevel.MODULE)
				.setMaxModuleDistance(2)
				.setStrictTracing(true)
				.build());

		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast)
				.collect(Collectors.toList());

		final Map<String, DataFlowGraphNode> nodeMap = dataFlowGraph.getNodes().stream()
				.collect(Collectors.toMap(DataFlowGraphNode::getId, Function.identity()));

		final ModuleNode moduleANode = moduleNodes.stream().filter(n -> n.getModuleId().equals(moduleA.getNid())).findAny().get();
		final ModuleNode moduleBNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(moduleB.getNid())).findAny().get();
		final ModuleNode moduleCNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(moduleC.getNid())).findAny().get();
		final Optional<ModuleNode> moduleDNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(moduleD.getNid())).findAny();
		assertTrue(moduleDNode.isEmpty(), "There should be no node for module D");

		assertConnected(moduleANode, moduleBNode, nodeMap, 1);
		assertConnected(moduleANode, moduleCNode, nodeMap, 1);
		assertConnected(moduleBNode, moduleCNode, nodeMap, 1);
	}

	
	/**
	 * With a distance of 3 the graph should be complete besides the DISPLAY statement in module D.
	 */
	@Test
	void testDistance3() {
		final DataFlowGraph dataFlowGraph = dataFlowGraphQueryService.buildDataFlowGraph(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(PROJECT_ID)
				.setStartModuleIds(Collections.singletonList(moduleA))
				.setQueryDirection(QueryDirection.BOTH)
				.setMaxModuleDistance(3)
				.setStrictTracing(true)
				.build());
		
		final List<ModuleNode> moduleNodes = dataFlowGraph.getNodes().stream()
				.filter(node -> node.getType().equals(DataFlowGraphNode.Type.MODULE))
				.map(ModuleNode.class::cast)
				.collect(Collectors.toList());
		
		final Map<String, DataFlowGraphNode> nodeMap = dataFlowGraph.getNodes().stream()
				.collect(Collectors.toMap(DataFlowGraphNode::getId, Function.identity()));
		
		final ModuleNode moduleANode = moduleNodes.stream().filter(n -> n.getModuleId().equals(moduleA.getNid())).findAny().get();
		final ModuleNode moduleBNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(moduleB.getNid())).findAny().get();
		final ModuleNode moduleCNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(moduleC.getNid())).findAny().get();
		final ModuleNode moduleDNode = moduleNodes.stream().filter(n -> n.getModuleId().equals(moduleD.getNid())).findAny().get();
		
		assertConnected(moduleANode, moduleBNode, nodeMap, 1);
		assertConnected(moduleANode, moduleCNode, nodeMap, 1);
		assertConnected(moduleBNode, moduleCNode, nodeMap, 1);
		assertConnected(moduleCNode, moduleDNode, nodeMap, 1);
		
		final List<DataFlowGraphNode> children = moduleDNode.getChildren().stream().map(c -> nodeMap.get(c)).collect(Collectors.toList());
		assertTrue(children.stream().anyMatch(c -> "DUMMY5".equals(c.getName())), "module D should have a node \"DUMMY5\"");
		assertTrue(children.stream().anyMatch(c -> "Display".equals(c.getName())), "module D should have a node \"DISPLAY\"");
	}
	
	/**
	 * Asserts that there are a certain amount of edges pointing from data interfaces in one module node to another.
	 *
	 * @param moduleA module node that is supposed to have outgoing edges
	 * @param moduleB module node that is supposed to have incoming edges
	 * @param nodeMap maps node IDs to nodes
	 * @param numOfEdges amount of edges pointing from moduleA to moduleB
	 */
	private void assertConnected(final ModuleNode moduleA, final ModuleNode moduleB, final Map<String, DataFlowGraphNode> nodeMap, final int numOfEdges) {
		final Set<DataFlowGraphNode> childrenAOutgoings = moduleA.getDataInterfaces().stream()
				.flatMap(c -> nodeMap.get(c).getOutgoings().stream())
				.map(s -> nodeMap.get(s)).collect(Collectors.toSet());
		
		final Set<DataFlowGraphNode> childrenB = moduleB.getDataInterfaces().stream().map(c -> nodeMap.get(c)).collect(Collectors.toSet());
		childrenAOutgoings.retainAll(childrenB);
		assertEquals(numOfEdges, childrenAOutgoings.size(), moduleA.getName() + " needs to have " + numOfEdges + " outgoing edges to " + moduleB.getName());
	}
}
