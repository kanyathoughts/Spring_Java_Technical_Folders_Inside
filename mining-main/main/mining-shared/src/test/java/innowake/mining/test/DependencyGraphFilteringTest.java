/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;
import innowake.mining.shared.model.dependency.graph.NodeType;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.function.Predicate;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test class for testing the graph filter utility
 */
public class DependencyGraphFilteringTest {

	private final static Long ONE = Long.valueOf(1);
	private final static Long TWO = Long.valueOf(2);
	private final static Long THREE = Long.valueOf(3);
	private final static Long FOUR = Long.valueOf(4);
	private final static Long FIVE = Long.valueOf(5);
	private final static EntityId PROJECT = EntityId.of(ONE);
	
	private static final UUID DUMMY_UUID = new UUID(0, 0);
	
	private final static Predicate<NodeType> JCL_JOB_PREDICATE = getFilterPredicate(Arrays.asList(NodeType.of(Technology.JCL, Type.JOB)));
	private final static Predicate<NodeType> COBOL_PROGRAM_PREDICATE = getFilterPredicate(Arrays.asList(NodeType.of(Technology.COBOL, Type.PROGRAM)));
	private final static Predicate<NodeType> ALL_NODE_TYPE_PREDICATE = getFilterPredicate(Arrays.asList(NodeType.of(Technology.JCL, Type.EXEC_PGM),
			                                                                                            NodeType.of(Technology.COBOL, Type.PROGRAM),
			                                                                                            NodeType.of(Technology.RESOURCE, Type.VIEW)));
	private final static Predicate<NodeType> EMPTY_NODE_TYPE_PREDICATE = getFilterPredicate(Collections.emptyList());
	
	private final static Predicate<RelationshipType> EMPTY_RELATIONSHIP_PREDICATE = getFilterPredicate(Collections.emptyList());
	private final static Predicate<RelationshipType> ACCESSES_RELATIONSHIP_PREDICATE = getFilterPredicate(Arrays.asList(RelationshipType.ACCESSES));

	/**
	 * Method to test graph filtering on a basic graph based on Node Type
	 * and merge the filtered edge into an Artificial link.
	 * 
	 * Before Filtering:
	 * 
	 * (Module A) --CALLS--> (Module B) --ACCESSES--> (Module C)
	 * 
	 * After Filtering:
	 * 
	 * (Module A) --ARTIFICIAL--> (Module C)
	 */
	@Test
	public void testGraphFilterMergeNodes() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.JOB);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.JCL, Type.EXEC_PGM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.CALLS);
		final ModuleRelationshipPojo refBToC = createReference(moduleB, moduleC, RelationshipType.ACCESSES);

		linkList.add(refAToB);
		linkList.add(refBToC);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(JCL_JOB_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(2, graph.getModules().size());
		assertEquals(1, graph.getReferences().size());

		final ModuleRelationshipPojo filteredLink = graph.getReferences().get(0);
		final UUID filteredFromId = filteredLink.getSrcModule();
		final UUID filteredToId = filteredLink.getDstModule();

		assertEquals(moduleA.getUid(), filteredFromId);
		assertEquals(moduleC.getUid(), filteredToId);
		assertEquals(DUMMY_UUID, filteredLink.getId());
		assertEquals(RelationshipType.ARTIFICIAL, filteredLink.getRelationship());
	}

	/**
	 * Method to test filtering of graph based on Node Type
	 * and merging nodes that are not directly connected after removing a node.
	 * 
	 * In this case, Modules A, and B are connected to the Module C.
	 * Additionally, Module C is connected to Modules D, and E.
	 * Upon filtering, we remove Module C, and create Artificial connections between Modules A, B, D, and E.
	 * 
	 * Graph before filtering:
	 * 
	 * (Module A)                                   (Module B)
	 *      |              ______________               |
	 *      '------------>|              |<-------------'
	 *                    |   Module C   |
	 *                    |______________|
	 *                      |           |
	 * (Module D)<----------'           '---------->(Module E)
	 * 
	 * Graph after filtering Module C:
	 * 
	 * (Module A)                                   (Module B)
	 *  |     |             __________________________|   |
	 * 	|     |____________|____________                  |
	 * 	|                  |            |                 |
	 *  '-->(Module D)<----'            '---->(Module E)<-'
	 */
	@Test
	public void testGraphFilterMergeMultipleNodes() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.JCL, Type.JOB);
		final ModulePojo moduleD = create(FOUR, "Module D", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleE = create(FIVE, "Module E", Technology.JCL, Type.EXEC_PGM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);
		moduleList.add(moduleD);
		moduleList.add(moduleE);

		final ModuleRelationshipPojo refAToC = createReference(moduleA, moduleC, RelationshipType.CALLS);
		final ModuleRelationshipPojo refBToC = createReference(moduleB, moduleC, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refCToD = createReference(moduleC, moduleD, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refCToE = createReference(moduleC, moduleE, RelationshipType.ACCESSES);

		linkList.add(refAToC);
		linkList.add(refBToC);
		linkList.add(refCToD);
		linkList.add(refCToE);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(JCL_JOB_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(4, graph.getModules().size());
		assertEquals(4, graph.getReferences().size());

		boolean isLinkAToD = false;
		boolean isLinkAToE = false;
		boolean isLinkBToD = false;
		boolean isLinkBToE = false;
		boolean isEdgeWithC = false;

		for (final ModuleRelationshipPojo link: graph.getReferences()) {
			if (moduleA.getUid().equals(link.getSrcModule()) && moduleD.getUid().equals(link.getDstModule()) && DUMMY_UUID.equals(link.getId())) {
				isLinkAToD = true;
			}
			if (moduleA.getUid().equals(link.getSrcModule()) && moduleE.getUid().equals(link.getDstModule()) && DUMMY_UUID.equals(link.getId())) {
				isLinkAToE = true;
			}
			if (moduleB.getUid().equals(link.getSrcModule()) && moduleD.getUid().equals(link.getDstModule()) && DUMMY_UUID.equals(link.getId())) {
				isLinkBToD = true;
			}
			if (moduleB.getUid().equals(link.getSrcModule()) && moduleE.getUid().equals(link.getDstModule()) && DUMMY_UUID.equals(link.getId())) {
				isLinkBToE = true;
			}
			if (moduleC.getUid().equals(link.getSrcModule()) || moduleC.getUid().equals(link.getDstModule())) {
				isEdgeWithC = true;
			}
		}

		assertTrue(isLinkAToD);
		assertTrue(isLinkAToE);
		assertTrue(isLinkBToD);
		assertTrue(isLinkBToE);

		assertFalse(isEdgeWithC);
	}

	/**
	 * Method to test filtering of graph based on Node Type
	 * and merging nodes that are not directly connected after removing a node and an edge.
	 * 
	 * In this case, Modules A, and B are connected to the Module C.
	 * Additionally, Module C is connected to Modules D, and E.
	 * Upon filtering, we remove Modules C and E, and create Artificial connections between Modules A, B, and D.
	 * 
	 * Graph before filtering:
	 * 
	 * (Module A)                                   (Module B)
	 *      |              ______________               |
	 *      '------------>|              |<-------------'
	 *                    |   Module C   |
	 *                    |______________|
	 *                      |           |
	 * (Module D)<----------'           '---------->(Module E)
	 * 
	 * Graph after filtering Module C:
	 * 
	 * (Module A)        (Module B)
	 *  |                  |
	 *  |                  |
	 *  |                  |
	 *  '--->(Module D)<---'
	 */
	@Test
	public void testGraphFilterMergeMultipleNodesAndEdge() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.JCL, Type.JOB);
		final ModulePojo moduleD = create(FOUR, "Module D", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleE = create(FIVE, "Module E", Technology.JCL, Type.JOB);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);
		moduleList.add(moduleD);
		moduleList.add(moduleE);

		final ModuleRelationshipPojo refAToC = createReference(moduleA, moduleC, RelationshipType.CALLS);
		final ModuleRelationshipPojo refBToC = createReference(moduleB, moduleC, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refCToD = createReference(moduleC, moduleD, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refCToE = createReference(moduleC, moduleE, RelationshipType.ACCESSES);

		linkList.add(refAToC);
		linkList.add(refBToC);
		linkList.add(refCToD);
		linkList.add(refCToE);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(JCL_JOB_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(3, graph.getModules().size());
		assertEquals(2, graph.getReferences().size());

		boolean isLinkAToD = false;
		boolean isLinkBToD = false;
		boolean isEdgeWithC = false;
		boolean isEdgeWithE = false;
		for (final ModuleRelationshipPojo link: graph.getReferences()) {
			if (moduleA.getUid().equals(link.getSrcModule()) && moduleD.getUid().equals(link.getDstModule()) && DUMMY_UUID.equals(link.getId())) {
				isLinkAToD = true;
			}
			if (moduleB.getUid().equals(link.getSrcModule()) && moduleD.getUid().equals(link.getDstModule()) && DUMMY_UUID.equals(link.getId())) {
				isLinkBToD = true;
			}
			if (moduleC.getUid().equals(link.getSrcModule()) || moduleC.getUid().equals(link.getDstModule())) {
				isEdgeWithC = true;
			}
			if (moduleE.getUid().equals(link.getSrcModule()) || moduleE.getUid().equals(link.getDstModule())) {
				isEdgeWithE = true;
			}
		}

		assertTrue(isLinkAToD);
		assertTrue(isLinkBToD);

		assertFalse(isEdgeWithC);
		assertFalse(isEdgeWithE);
	}

	/**
	 * Method to test filtering the graph based on Node Type after removing a node with only incoming edges.
	 * 
	 * In this case, the Module D is connected by only incoming edges from Modules A, B, and C.
	 * Upon filtering, we remove Module D and the graph only contains Modules A, B, and C.
	 * These modules are connected to each other and are not affected by the filtering.
	 * 
	 * Graph before filtering:
	 * 
	 * (Module A) --------> (Module B) --------> (Module C)
	 *  |                       |                   |
	 *  |   ____________        |                   |
	 *  '->|            |<------'                   |
	 *     | Module D   |                           |
	 *     |____________|<--------------------------'
	 * 
	 * Graph after filtering module D:
	 * 
	 * (Module A) --------> (Module B) --------> (Module C)
	 */
	@Test
	public void testGraphFilterMergeNodesWithIncomingEdgesOnly() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleD = create(FOUR, "Module D", Technology.JCL, Type.JOB);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);
		moduleList.add(moduleD);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.CALLS);
		final ModuleRelationshipPojo refBToC = createReference(moduleB, moduleC, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refAToD = createReference(moduleA, moduleD, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refBToD = createReference(moduleB, moduleD, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refCToD = createReference(moduleC, moduleD, RelationshipType.ACCESSES);

		linkList.add(refAToB);
		linkList.add(refBToC);
		linkList.add(refAToD);
		linkList.add(refBToD);
		linkList.add(refCToD);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(JCL_JOB_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(3, graph.getModules().size());
		assertEquals(2, graph.getReferences().size());

		boolean isLinkAToB = false;
		boolean isLinkBToC = false;
		boolean isEdgeWithD = false;
		for (final ModuleRelationshipPojo link: graph.getReferences()) {
			if (moduleA.getUid().equals(link.getSrcModule()) && moduleB.getUid().equals(link.getDstModule()) && refAToB.getId().equals(link.getId())) {
				isLinkAToB = true;
			}
			if (moduleB.getUid().equals(link.getSrcModule()) && moduleC.getUid().equals(link.getDstModule()) && refBToC.getId().equals(link.getId())) {
				isLinkBToC = true;
			}
			if (moduleD.getUid().equals(link.getSrcModule()) || moduleD.getUid().equals(link.getDstModule())) {
				isEdgeWithD = true;
			}
		}

		assertTrue(isLinkAToB);
		assertTrue(isLinkBToC);

		assertFalse(isEdgeWithD);
	}

	/**
	 * Method to test filtering graph based on an Edge Type.
	 * 
	 * Graph Before Filtering:
	 * (Module A) --CALLS--> (Module B) --ACCESSES--> (Module C)
	 * 
	 * Graph After Filtering on Edge Type ACCESSES:
	 * (Module A) --CALLS--> (Module B)
	 */
	@Test
	public void testGraphFilterRemoveEdgeType() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.JCL, Type.EXEC_PGM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.CALLS);
		final ModuleRelationshipPojo refCToB = createReference(moduleC, moduleB, RelationshipType.ACCESSES);

		linkList.add(refAToB);
		linkList.add(refCToB);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(EMPTY_NODE_TYPE_PREDICATE, ACCESSES_RELATIONSHIP_PREDICATE);

		assertEquals(2, graph.getModules().size());
		assertEquals(1, graph.getReferences().size());

		final ModuleRelationshipPojo filteredLink = graph.getReferences().get(0);

		assertEquals(RelationshipType.CALLS, filteredLink.getRelationship());
		assertEquals(moduleA.getUid(), filteredLink.getSrcModule());
		assertEquals(moduleB.getUid(), filteredLink.getDstModule());
		assertEquals(refAToB.getId(), filteredLink.getId());
	}

	/**
	 * Method to test the filtering of graph based on Edge Type.
	 * In this case, an intermediate node is connected by the Edge Type specified in the filtering parameters.
	 * So, on filtering the graph, it should remove all edges connected to
	 * the intermediate node (incoming and outgoing) irrespective of the type of the edge connected to it.
	 * 
	 * Graph Before Filtering:
	 * (Module A) --CALLS--> (Module B) --ACCESSES--> (Module C) --CALLS--> (Module D)
	 * 
	 * Graph After Filtering on Edge Type ACCESSES:
	 * (Module A) --CALLS--> (Module B)
	 */
	@Test
	public void testGraphFilterRemoveNodesWithEdgeType() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleD = create(FOUR, "Module D", Technology.JCL, Type.JOB);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);
		moduleList.add(moduleD);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.CALLS);
		final ModuleRelationshipPojo refBToC = createReference(moduleB, moduleC, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refCToD = createReference(moduleC, moduleD, RelationshipType.CALLS);

		linkList.add(refAToB);
		linkList.add(refBToC);
		linkList.add(refCToD);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(EMPTY_NODE_TYPE_PREDICATE, ACCESSES_RELATIONSHIP_PREDICATE);

		assertEquals(2, graph.getModules().size());
		assertEquals(1, graph.getReferences().size());

		final ModuleRelationshipPojo filteredLink = graph.getReferences().get(0);

		assertEquals(RelationshipType.CALLS, filteredLink.getRelationship());
		assertEquals(moduleA.getUid(), filteredLink.getSrcModule());
		assertEquals(moduleB.getUid(), filteredLink.getDstModule());
		assertEquals(refAToB.getId(), filteredLink.getId());
	}

	/**
	 * Method to test graph filtering based on Edge Type.
	 * In this case, we have Module A that is connected to Module B.
	 * And, we have Module B connected to Module C by two edges of different types.
	 * So, upon filtering, it should remove only one edge.
	 * 
	 * Graph Before Filtering:
	 * 
	 * (Module A) --CALLS--> (Module B) ________________________________________
	 *                          |                                               |
	 *                          '-----ACCESSES----> (Module C) <---CALLS----'
	 *
	 * Graph After Filtering on Edge Type ACCESSES:
	 *
	 * (Module A) --CALLS--> (Module B) --CALLS--> (Module C)
	 */
	@Test
	public void testGraphFilterRemoveDuplicateEdge() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.JCL, Type.EXEC_PGM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.CALLS);
		final ModuleRelationshipPojo refBToCReadWrite = createReference(moduleB, moduleC, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refBToCCall = createReference(moduleB, moduleC, RelationshipType.CALLS);

		linkList.add(refAToB);
		linkList.add(refBToCReadWrite);
		linkList.add(refBToCCall);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(EMPTY_NODE_TYPE_PREDICATE, ACCESSES_RELATIONSHIP_PREDICATE);

		assertEquals(3, graph.getModules().size());
		assertEquals(2, graph.getReferences().size());

		boolean isEdgeReadWrite = false;
		boolean isEdgeCalls = false;
		boolean isLinkAToB = false;
		boolean isLinkBToC = false;
		for (final ModuleRelationshipPojo filteredLink: graph.getReferences()) {
			if (moduleA.getUid().equals(filteredLink.getSrcModule()) && moduleB.getUid().equals(filteredLink.getDstModule())
					&& refAToB.getId().equals(filteredLink.getId())) {
				isLinkAToB = true;
			}
			if (moduleB.getUid().equals(filteredLink.getSrcModule()) && moduleC.getUid().equals(filteredLink.getDstModule())
					&& refBToCCall.getId().equals(filteredLink.getId())) {
				isLinkBToC = true;
			}
			if (RelationshipType.CALLS == filteredLink.getRelationship()) {
				isEdgeCalls = true;
			}
			if (RelationshipType.ACCESSES == filteredLink.getRelationship()) {
				isEdgeReadWrite = true;
			}
		}

		assertTrue(isEdgeCalls);
		assertTrue(isLinkAToB);
		assertTrue(isLinkBToC);

		assertFalse(isEdgeReadWrite);
	}

	/**
	 * Method to test that we are not removing the root node
	 * even though the filtering parameter may consist of the Node Type
	 * of the root node.
	 * 
	 * Graph Before Filtering (All modules are of type COBOL_PROGRAM):
	 * 
	 * (Module Root) --CALLS--> (Module B) --CALLS--> (Module C)
	 * 
	 * Graph After Filtering on Node Type COBOL_PROGRAM:
	 * 
	 * (Module Root)
	 */
	@Test
	public void testGraphFilterRemoveRootNodeType() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.COBOL, Type.PROGRAM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.COBOL, Type.PROGRAM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.COBOL, Type.PROGRAM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.CALLS);
		final ModuleRelationshipPojo refBToC = createReference(moduleB, moduleC, RelationshipType.CALLS);

		linkList.add(refAToB);
		linkList.add(refBToC);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(COBOL_PROGRAM_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(1, graph.getModules().size());
		assertEquals(0, graph.getReferences().size());

		final ModulePojo filteredModule = graph.getModules().get(0);

		assertEquals(ONE, filteredModule.getId());
	}

	/**
	 * Method to test that filtering works on the basis of {@link Type} and {@link Technology}.
	 * In this case, we have 5 {@link Module}s connected to each other.
	 * These 5 {@link Module} have the same {@link Type} - Type.PROGRAM.
	 * However, each {@link Module} has a different {@link Technology}.
	 * So, upon filtering for COBOL_PROGRAM, it should remove only one node.
	 * Other nodes should not be filtered from the graph.
	 * 
	 * Graph before filtering:
	 * 
	 * (Module A)                                   (Module B)
	 *      |              ______________               |
	 *      '------------>|              |<-------------'
	 *                    |   Module C   |
	 *                    |______________|
	 *                      |           |
	 * (Module D)<----------'           '---------->(Module E)
	 * 
	 * Graph after filtering Module D:
	 * 
	 * (Module A)                                   (Module B)
	 *      |              ______________               |
	 *      '------------>|              |<-------------'
	 *                    |   Module C   |
	 *                    |______________|
	 *                          |
	 *                          '---------->(Module E)
	 * 
	 */
	@Test
	public void testGraphFilterRemoveSameTypeDiffTechnology() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.CSD, Type.PROGRAM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.PL1, Type.PROGRAM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.NATURAL, Type.PROGRAM);
		final ModulePojo moduleD = create(FOUR, "Module D", Technology.COBOL, Type.PROGRAM);
		final ModulePojo moduleE = create(FIVE, "Module E", Technology.EASYTRIEVE, Type.PROGRAM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);
		moduleList.add(moduleD);
		moduleList.add(moduleE);

		final ModuleRelationshipPojo refAToC = createReference(moduleA, moduleC, RelationshipType.CALLS);
		final ModuleRelationshipPojo refBToC = createReference(moduleB, moduleC, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refCToD = createReference(moduleC, moduleD, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refCToE = createReference(moduleC, moduleE, RelationshipType.ACCESSES);

		linkList.add(refAToC);
		linkList.add(refBToC);
		linkList.add(refCToD);
		linkList.add(refCToE);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(COBOL_PROGRAM_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(4, graph.getModules().size());
		assertEquals(3, graph.getReferences().size());

		boolean isLinkAToC = false;
		boolean isLinkBToC = false;
		boolean isLinkCToD = false;
		boolean isLinkCToE = false;
		boolean isEdgeWithD = false;
		for (final ModuleRelationshipPojo link: graph.getReferences()) {
			if (moduleA.getUid().equals(link.getSrcModule()) && moduleC.getUid().equals(link.getDstModule()) && refAToC.getId().equals(link.getId())) {
				isLinkAToC = true;
			}
			if (moduleB.getUid().equals(link.getSrcModule()) && moduleC.getUid().equals(link.getDstModule()) && refBToC.getId().equals(link.getId())) {
				isLinkBToC = true;
			}
			if (moduleC.getUid().equals(link.getSrcModule()) && moduleD.getUid().equals(link.getDstModule())) {
				isLinkCToD = true;
			}
			if (moduleC.getUid().equals(link.getSrcModule()) && moduleE.getUid().equals(link.getDstModule()) && refCToE.getId().equals(link.getId())) {
				isLinkCToE = true;
			}
			if (moduleD.getUid().equals(link.getSrcModule()) || moduleD.getUid().equals(link.getDstModule())) {
				isEdgeWithD = true;
			}
		}

		assertTrue(isLinkAToC);
		assertTrue(isLinkBToC);
		assertTrue(isLinkCToE);

		assertFalse(isLinkCToD);
		assertFalse(isEdgeWithD);
	}

	/**
	 * Method to test that filtering graph creates an ARTIFICIAL link
	 * when multiple nodes are removed from in between.
	 * In this scenario, we have 4 modules with only OUTGOING edges 
	 * from the root module. Upon filtering, it should remove the nodes
	 * from in between and create an artificial link between the root node
	 * and the last node.
	 * 
	 * Graph before filtering:
	 * 
	 * (Module A) ----> (Module B) ----> (Module C) ----> (Module D)
	 * 
	 * Graph after filtering and removing Modules B and C:
	 * 
	 * (Module A) ----ARTIFICIAL----> (Module D)
	 */
	@Test
	public void testGraphFilterMustCreateArtificialLinkAllOutgoing() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.CSD, Type.PROGRAM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.NATURAL, Type.PROGRAM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.NATURAL, Type.PROGRAM);
		final ModulePojo moduleD = create(FOUR, "Module D", Technology.COBOL, Type.PROGRAM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);
		moduleList.add(moduleD);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.CALLS);
		final ModuleRelationshipPojo refBToC = createReference(moduleB, moduleC, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refCToD = createReference(moduleC, moduleD, RelationshipType.CALLS);

		linkList.add(refAToB);
		linkList.add(refBToC);
		linkList.add(refCToD);
		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(getFilterPredicate(Arrays.asList(NodeType.of(Technology.NATURAL, Type.PROGRAM))), getFilterPredicate(Collections.emptyList()));

		assertEquals(2, graph.getModules().size());
		assertEquals(1, graph.getReferences().size());


		assertEquals(RelationshipType.ARTIFICIAL, graph.getReferences().get(0).getRelationship());

		final var filteredLink = graph.getReferences().get(0);

		assertEquals(RelationshipType.ARTIFICIAL, filteredLink.getRelationship());
		assertEquals(DUMMY_UUID, filteredLink.getId());
	}

	/**
	 * Method to test that filtering graph creates an ARTIFICIAL link
	 * when multiple nodes are removed from in between.
	 * In this scenario, we have 4 modules with only INCOMING edges 
	 * from the root module. Upon filtering, it should remove the nodes
	 * from in between and create an artificial link between the root node
	 * and the last node.
	 * 
	 * Graph before filtering:
	 * 
	 * (Module A) <---- (Module B) <---- (Module C) <---- (Module D)
	 * 
	 * Graph after filtering and removing Modules B and C:
	 * 
	 * (Module A) <----ARTIFICIAL---- (Module D)
	 */
	@Test
	public void testGraphFilterMustCreateArtificialLinkAllIncoming() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.CSD, Type.PROGRAM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.NATURAL, Type.PROGRAM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.NATURAL, Type.PROGRAM);
		final ModulePojo moduleD = create(FOUR, "Module D", Technology.COBOL, Type.PROGRAM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);
		moduleList.add(moduleD);

		final ModuleRelationshipPojo refBToA = createReference(moduleB, moduleA, RelationshipType.CALLS);
		final ModuleRelationshipPojo refCToB = createReference(moduleC, moduleB, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refDToC = createReference(moduleD, moduleC, RelationshipType.CALLS);

		linkList.add(refBToA);
		linkList.add(refCToB);
		linkList.add(refDToC);
		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(getFilterPredicate(Arrays.asList(NodeType.of(Technology.NATURAL, Type.PROGRAM))), getFilterPredicate(Collections.emptyList()));

		assertEquals(2, graph.getModules().size());
		assertEquals(1, graph.getReferences().size());

		final var filteredLink = graph.getReferences().get(0);

		assertEquals(RelationshipType.ARTIFICIAL, filteredLink.getRelationship());
		assertEquals(DUMMY_UUID, filteredLink.getId());
	}

	/**
	 * Method to test that filtering graph does not create an ARTIFICIAL link
	 * when multiple nodes are removed from in between.
	 * In this scenario, we have 4 modules.
	 * Upon filtering, it should remove the nodes and return only
	 * the root module.
	 * 
	 * Graph before filtering:
	 * 
	 * (Module A) ----> (Module B) <---- (Module C) <---- (Module D)
	 * 
	 * Graph after filtering and removing Modules B and C:
	 * 
	 * (Module A)
	 */
	@Test
	public void testGraphFilterMustNotCreateArtificialLink() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.CSD, Type.PROGRAM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.NATURAL, Type.PROGRAM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.NATURAL, Type.PROGRAM);
		final ModulePojo moduleD = create(FOUR, "Module D", Technology.COBOL, Type.PROGRAM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);
		moduleList.add(moduleD);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.CALLS);
		final ModuleRelationshipPojo refCToB = createReference(moduleC, moduleB, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refDToC = createReference(moduleD, moduleC, RelationshipType.CALLS);

		linkList.add(refAToB);
		linkList.add(refCToB);
		linkList.add(refDToC);
		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(getFilterPredicate(Arrays.asList(NodeType.of(Technology.NATURAL, Type.PROGRAM))), getFilterPredicate(Collections.emptyList()));

		assertEquals(1, graph.getModules().size());
		assertEquals(0, graph.getReferences().size());

		assertEquals(ONE, graph.getModules().get(0).getId());
	}
	
	/**
	 * Method to test that providing empty predicates for filtering
	 * does not alter {@link DependencyGraph} if
	 * the {@link DependencyGraph#filterGraph(Predicate, Predicate)} is called.
	 *
	 * Graph before filtering:
	 * 
	 * (Module A)           (Module D) <--------- (Module C)
	 *      |                   |
	 *      '----> (Module B) <-'
	 * 
	 * Graph after calling filter method:
	 * 
	 * (Module A)           (Module D) <--------- (Module C)
	 *      |                   |
	 * 		'----> (Module B) <-'
	 */
	@Test
	public void testGraphNoFiltering() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.JCL, Type.JOB);
		final ModulePojo moduleD = create(FOUR, "Module D", Technology.JCL, Type.EXEC_PGM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);
		moduleList.add(moduleD);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refDToB = createReference(moduleD, moduleB, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refCToD = createReference(moduleC, moduleD, RelationshipType.CALLS);

		linkList.add(refAToB);
		linkList.add(refDToB);
		linkList.add(refCToD);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(EMPTY_NODE_TYPE_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(4, graph.getModules().size());
		assertEquals(3, graph.getReferences().size());

		boolean isLinkAToB = false;
		boolean isLinkDToB = false;
		boolean isLinkCToD = false;

		for (final ModuleRelationshipPojo link: graph.getReferences()) {
			if (moduleA.getUid().equals(link.getSrcModule()) && moduleB.getUid().equals(link.getDstModule()) && refAToB.getId().equals(link.getId())) {
				isLinkAToB = true;
			}
			if (moduleD.getUid().equals(link.getSrcModule()) && moduleB.getUid().equals(link.getDstModule()) && refDToB.getId().equals(link.getId())) {
				isLinkDToB = true;
			}
			if (moduleC.getUid().equals(link.getSrcModule()) && moduleD.getUid().equals(link.getDstModule()) && refCToD.getId().equals(link.getId())) {
				isLinkCToD = true;
			}
		}

		assertTrue(isLinkAToB);
		assertTrue(isLinkDToB);
		assertTrue(isLinkCToD);
	}

	/**
	 * Method to test that self-referencing edges are retained in {@link DependencyGraph}.
	 * {@link DependencyGraph#filterGraph(Predicate, Predicate)} should not remove
	 * self-referencing edges when called with predicates that do not affect the edge or the module.
	 * 
	 * Graph before filtering:
	 *                           ___________
	 *                          |           |
	 * (Module A) --------> (Module B) <----'
	 * 
	 * Graph after calling filter method:
	 *                           ___________
	 *                          |           |
	 * (Module A) --------> (Module B) <----'
	 */
	@Test
	public void testGraphFilterSelfReferentialNodes() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.EXEC_PGM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		
		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refBToB = createReference(moduleB, moduleB, RelationshipType.REFERENCES);

		linkList.add(refAToB);
		linkList.add(refBToB);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(EMPTY_NODE_TYPE_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(2, graph.getModules().size());
		assertEquals(2, graph.getReferences().size());

		boolean isLinkAToB = false;
		boolean isSelfRefLink = false;

		for (final ModuleRelationshipPojo link: graph.getReferences()) {
			if (moduleA.getUid().equals(link.getSrcModule()) && moduleB.getUid().equals(link.getDstModule()) && refAToB.getId().equals(link.getId())) {
				isLinkAToB = true;
			}
			if (moduleB.getUid().equals(link.getSrcModule()) && moduleB.getUid().equals(link.getDstModule()) && refBToB.getId().equals(link.getId())) {
				isSelfRefLink = true;
			}
		}

		assertTrue(isLinkAToB);
		assertTrue(isSelfRefLink);
	}

	/**
	 * Method to test that self-referencing edges are removed from {@link DependencyGraph}.
	 * {@link DependencyGraph#filterGraph(Predicate, Predicate)} should remove
	 * self-referencing edges when called with predicates that affect the edge or the module.
	 * It should not throw any exceptions.
	 * 
	 * Graph before filtering:
	 *                           ___________
	 *                          |           |
	 * (Module A) --------> (Module B) <----'
	 * 
	 * Graph after calling filter method:
	 * 
	 * (Module A)
	 */
	@Test
	public void testGraphFilterSelfReferentialNodesRemoveNode() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.COBOL, Type.PROGRAM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.COBOL, Type.PROGRAM);

		moduleList.add(moduleA);
		moduleList.add(moduleB);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refBToB = createReference(moduleB, moduleB, RelationshipType.REFERENCES);

		linkList.add(refAToB);
		linkList.add(refBToB);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(COBOL_PROGRAM_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(1, graph.getModules().size());
		assertEquals(0, graph.getReferences().size());

		assertEquals(ONE, graph.getModules().get(0).getId());
	}

	/**
	 * Method to test that, when all module types are being filtered,
	 * the {@link DependencyGraph} should contain only the Root Module.
	 * 
	 * Graph before filtering:
	 * 
	 * (Module A) --------> (Module B)
	 * 
	 * Graph after filtering all module types:
	 * 
	 * (Module A)
	 *
	 */
	@Test
	public void testGraphFilterShouldRemoveAll() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.COBOL, Type.PROGRAM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.RESOURCE, Type.VIEW);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);

		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refBToC = createReference(moduleB, moduleC, RelationshipType.ACCESSES);

		linkList.add(refAToB);
		linkList.add(refBToC);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(ALL_NODE_TYPE_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(1, graph.getModules().size());
		assertEquals(0, graph.getReferences().size());
	}
	
	/**
	 * Method to test that if nodes are removed and a self-loop is formed within those nodes,
	 * the self-loop should be removed as well.
	 * 
	 * Graph before filtering:
	 * 
	 * (Module A)                ___________
	 *      |            _______|__         |
	 *      '---------->|          |<-------'
	 *  __________      | Module B |________
	 * |          |---->|__________|        |
	 * | Module D |                         |
	 * |__________|<------ (Module C)<------'
	 * 
	 * Graph after filtering and removing all Module types:
	 * 
	 * (Module A)
	 */
	@Test
	public void testGraphFilterDistantSelfReference() {
		final List<ModulePojo> moduleList = new ArrayList<>();
		final List<ModuleRelationshipPojo> linkList = new ArrayList<>();

		final ModulePojo moduleA = create(ONE, "Module A", Technology.COBOL, Type.PROGRAM);
		final ModulePojo moduleB = create(TWO, "Module B", Technology.JCL, Type.EXEC_PGM);
		final ModulePojo moduleC = create(THREE, "Module C", Technology.RESOURCE, Type.VIEW);

		moduleList.add(moduleA);
		moduleList.add(moduleB);
		moduleList.add(moduleC);
		
		final ModuleRelationshipPojo refAToB = createReference(moduleA, moduleB, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refBToC = createReference(moduleB, moduleC, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refBToB = createReference(moduleB, moduleB, RelationshipType.ACCESSES);

		final ModulePojo moduleD = create(FOUR, "Module D", Technology.RESOURCE, Type.VIEW);
		final ModuleRelationshipPojo refCToD = createReference(moduleC, moduleD, RelationshipType.ACCESSES);
		final ModuleRelationshipPojo refDToB = createReference(moduleD, moduleB, RelationshipType.ACCESSES);

		linkList.add(refAToB);
		linkList.add(refBToC);
		linkList.add(refBToB);
		linkList.add(refCToD);
		linkList.add(refDToB);

		final DependencyGraph graph = new DependencyGraph(moduleList, linkList);
		graph.filterGraph(ALL_NODE_TYPE_PREDICATE, EMPTY_RELATIONSHIP_PREDICATE);

		assertEquals(1, graph.getModules().size());
		assertEquals(0, graph.getReferences().size());
	}

	private static <T> Predicate<T> getFilterPredicate(final List<T> filterParameters) {
		return new Predicate<T>() {
			@Override
			public boolean test(@Nullable final T t) {
				return filterParameters.contains(t);
			}
		};
	}

	private static ModulePojo create(final Long nid, final String name, final Technology technology, final Type type) {
		return new ModulePojo(
				UUID.randomUUID(),
				nid,
				CustomPropertiesMap.empty(),
				PROJECT, null, null,
				name,
				"path",
				technology,
				type,
				Storage.from(technology, type),
				Origin.CUSTOM,
				Creator.DISCOVERY,
				Identification.IDENTIFIED,
				null,
				"description",
				null,
				null,
				"link Hash",
				null,
				null,
				false,
				null,
				null,
				null,
				null,
				0,
				0,
				0,
				false,
				null, null, null,
				null, null);
	}

	private static ModuleRelationshipPojo createReference(final ModulePojo src, final ModulePojo dst, final RelationshipType type) {
		return new ModuleRelationshipPojo(
				UUID.randomUUID(),
				src.getUid(),
				null,
				dst.getUid(),
				null,
				type,
				null,
				null,
				null,
				null,
				Collections.emptyList(),
				null, null, null);
	}
}
