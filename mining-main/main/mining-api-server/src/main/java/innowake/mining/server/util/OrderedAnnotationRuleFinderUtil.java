/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.util;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang.ArrayUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;
import innowake.mining.shared.model.controlflow.ControlFlowNode;
import innowake.mining.shared.model.controlflow.ControlFlowEntity;

/**
 * Utility class for finding the Ordered Annotation Rule.
 */
public class OrderedAnnotationRuleFinderUtil {

	private final Logger logger = LoggerFactory.getLogger(OrderedAnnotationRuleFinderUtil.class);

	private static final String JUMP_STATEMENT = "JumpStatement";
	private static final String PROPERTY_TARGET = "target";
	private static final String BRANCH_STATEMENT = "BranchStatement";
	private static final String CFG_COLLAPSIBLE = "CfgCollapsibleNode";
	private static final String INVOCABLE_STATEMENT = "Invocable";
	private static final String INVOCATION_NAME = "invocation name";
	private static final String EXIT_LOOP_LABEL = "EXIT LOOP";

	private final Map<UUID, ControlFlowNode> nodeMap = new LinkedHashMap<>();
	private final Map<UUID, AnnotationPojo> astAndAnnotationMap = new LinkedHashMap<>();
	private final Map<UUID, List<UUID>> groupAstNodes = new LinkedHashMap<>();
	private final Map<UUID, UUID> groupNodesLastNode = new LinkedHashMap<>();
	private final Map<UUID, UUID> loopExitNodes = new LinkedHashMap<>();
	private final Set<UUID> groupNodesCalculationDone = new LinkedHashSet<>();
	private final Map<UUID, UUID> invocableReturnPlace = new LinkedHashMap<>();
	private final Map<UUID, List<UUID>> adjacencyList = new LinkedHashMap<>();

	private final ControlFlowGraph graph;
	private final List<AnnotationPojo> annotationList;

	public OrderedAnnotationRuleFinderUtil(final ControlFlowGraph graph, final List<AnnotationPojo> annotationList) {
		this.graph = graph;
		this.annotationList = annotationList;
	}
	
	public Map<Integer, AnnotationPojo> findAnnotationRulesOrder() {
		UUID startNode = null;
		UUID endNode = null;
		for (final var node : graph.nodes) {
			nodeMap.put(node.id, node);
			if (ControlFlowEntity.TERMINAL == node.entity) {
				if (AstModuleRelationshipType.ENTRY.name().equals(node.type)) {
					startNode = node.id;
				} else {
					endNode = node.id;
				}
			}
		}
		if (startNode != null && endNode != null) {
			mapAstAndAnnotation();
			buildAdjacencyList();
			mapGroupAsts();
			sortGroupNodes(groupAstNodes);
			findLastNodeOfGroupNodes();
			findInvocableReturnNode();
			final Map<UUID, Map<int[], List<AnnotationPojo>>> groupOrderedAnnotations = new LinkedHashMap<>();
			findOrderedAnnotationInGroups(groupOrderedAnnotations);
			final Map<int[], List<AnnotationPojo>> orderedAnnotations = new LinkedHashMap<>();
			findAllPathsUsingDepthFirstSearch(startNode, endNode, new HashSet<>(), new LinkedList<>(), orderedAnnotations, 0, groupOrderedAnnotations);
			return sortOrderAnnotationMap(orderedAnnotations);
		} else {
			throw new IllegalStateException("Start and End node of Control FLow graph is not present");
		}
	}

	private void findAllPathsUsingDepthFirstSearch(final UUID currentNode, final UUID endNode, final Set<UUID> visited,
			final LinkedList<UUID> path, final Map<int[], List<AnnotationPojo>> orderedAnnotations, final int level,
			final Map<UUID, Map<int[], List<AnnotationPojo>>> groupOrderedAnnotations) {
		if (currentNode.equals(endNode)) {
			path.add(currentNode);
			path.removeLast();
			return;
		}
		if (groupAstNodes.containsKey(currentNode) || invocableReturnPlace.containsKey(currentNode)) {
			final UUID invocableReturnNode = markingGroupNodes(currentNode, visited, orderedAnnotations, groupOrderedAnnotations, level);
			if (invocableReturnNode != null) {
				findAllPathsUsingDepthFirstSearch(invocableReturnNode, endNode, visited, path, orderedAnnotations, level + 1, groupOrderedAnnotations);
				return;
			}
		}
		/* Check if the current node is already visited */
		if (visited.contains(currentNode)) {
				/* The current node is part of a cycle, mark it as visited to avoid infinite recursion */
				return;
		}
		/* The current node is not part of a cycle, mark it as visited and continue traversal */
		visited.add(currentNode);

		markAnnotation(currentNode, orderedAnnotations, new int[] { level });
		/* Add the current node to the path */
		path.add(currentNode);

		final List<UUID> neighbours = adjacencyList.getOrDefault(currentNode, new ArrayList<>());
		for (final UUID neighbourId : neighbours) {
			if (path.contains(neighbourId)) {
				markAnnotation(neighbourId, orderedAnnotations, new int[] { level });
				visited.add(neighbourId);
				continue;
			}
			findAllPathsUsingDepthFirstSearch(neighbourId, endNode, visited, path, orderedAnnotations, level + 1, groupOrderedAnnotations);
		}
		path.remove(path.size() - 1);
	}

	private void findOrderedAnnotationInGroups(final Map<UUID, Map<int[], List<AnnotationPojo>>> orderedAnnotations) {
		for(final Entry<UUID, List<UUID>> entry: groupAstNodes.entrySet()) {
			final UUID key = entry.getKey();
			final List<UUID> values = entry.getValue();
			final int level = 1;
			if (! groupNodesCalculationDone.contains(key)) {
				final Map<int[], List<AnnotationPojo>> orderedAnnotation = new LinkedHashMap<>();
				markAnnotation(key, orderedAnnotation, new int [] { level });
				findAllPathsOfGroupNodesUsingDepthFirstSearch(key, values, new LinkedHashSet<>(), new LinkedList<>(), orderedAnnotation, level + 1,
						orderedAnnotations);
				orderedAnnotations.put(key, orderedAnnotation);
				groupNodesCalculationDone.add(key);
			}
		}
	}

	private void findAllPathsOfGroupNodesUsingDepthFirstSearch(final UUID currentNode, final List<UUID> groupNodes, final Set<UUID> visited,
			final LinkedList<UUID> path, final Map<int[], List<AnnotationPojo>> markedAnnotations, final int level,
			final Map<UUID, Map<int[], List<AnnotationPojo>>> groupOrderedAnnotations) {
		if ( ! groupNodes.contains(currentNode)) {
			path.add(currentNode);
			path.removeLast();
			return;
		}

		if (visited.contains(currentNode)) {
			return;
		}
		visited.add(currentNode);
		path.add(currentNode);

		final List<UUID> neighbours = adjacencyList.getOrDefault(currentNode, new ArrayList<>());
		int neighbourNumber = 1;
		for (final UUID neighbourId : neighbours) {
			if (groupNodes.contains(neighbourId)) {
				if (checkForJumpStatement(neighbourId)) {
					int numberOfJumps = 0;
					if (visited.contains(neighbourId)) {
						return;
					} 
					UUID tempNode = neighbourId;
					if(tempNode != null) {
						while(checkForJumpStatement(tempNode)) {
							final UUID invocableReturnNode = 
									calculatePathForInvocation(tempNode, markedAnnotations, level + numberOfJumps, groupOrderedAnnotations, neighbourNumber);
							visited.add(tempNode);
							if (invocableReturnNode != null && 
									groupNodes.contains(invocableReturnNode) && invocableReturnNode != neighbourId) {
								numberOfJumps++;
								markAnnotation(invocableReturnNode, markedAnnotations, new int[] { level + numberOfJumps,  neighbourNumber });
								tempNode = invocableReturnNode;
								numberOfJumps ++;
							} else if(invocableReturnNode == null) {
								return;
							} else {
								break;
							}
						}
						if(tempNode != neighbourId) {
							findAllPathsOfGroupNodesUsingDepthFirstSearch(tempNode, groupNodes, visited, path, markedAnnotations,
									level + ++numberOfJumps, groupOrderedAnnotations);
							path.remove(path.size() - 1);
							return;
						}
					}
				}

				if (path.contains(neighbourId)) {
					visited.add(neighbourId);
					continue;
				}
				if (! visited.contains(neighbourId)) {
					markAnnotation(neighbourId, markedAnnotations, new int[] { level, neighbourNumber });
				}
				findAllPathsOfGroupNodesUsingDepthFirstSearch(neighbourId, groupNodes, visited, path, markedAnnotations, level + 1,
						groupOrderedAnnotations);

			}
			
			neighbourNumber++;
		}
		path.remove(path.size() - 1);
	}

	private @Nullable UUID markingGroupNodes(final UUID neighbourNode, final Set<UUID> visited,
			final Map<int[], List<AnnotationPojo>> orderedAnnotations, final Map<UUID, Map<int[], List<AnnotationPojo>>> groupOrderedAnnotations,
			final int level) {
		if (checkForJumpStatement(neighbourNode)) {
			final UUID invocationNode = findInnovocableNode(neighbourNode);
			if (invocationNode != null && groupNodesCalculationDone.contains(invocationNode)) {
					markAnnotation(neighbourNode, orderedAnnotations, new int[] { level });
					calculateMarkedAnnotationOfEnclosedGroup(invocationNode, groupOrderedAnnotations, orderedAnnotations, new int[] { level });
			}
		} else if (checkForBranchStatement(neighbourNode) && groupNodesCalculationDone.contains(neighbourNode)) {
			calculateMarkedAnnotationOfEnclosedGroup(neighbourNode, groupOrderedAnnotations, orderedAnnotations, new int[] { level });
			final List<UUID> childs = groupAstNodes.get(neighbourNode);
			for (final UUID child : childs) {
				visited.add(child);
			}
		}
		return invocableReturnPlace.get(neighbourNode);
	}

	private @Nullable UUID calculatePathForInvocation(final UUID neighbourNode, final Map<int[], List<AnnotationPojo>> internalMarked, final int level,
			final Map<UUID, Map<int[], List<AnnotationPojo>>> groupOrderedAnnotations, final int neighbourNumber) {
			markAnnotation(neighbourNode, internalMarked, new int[] { level });
			final UUID invocationNode = findInnovocableNode(neighbourNode);
			if (invocationNode != null) {
				if (groupNodesCalculationDone.contains(invocationNode)) {
					calculateMarkedAnnotationOfEnclosedGroup(invocationNode, groupOrderedAnnotations, internalMarked, new int[] { level, neighbourNumber });
				} else {
					final Map<int[], List<AnnotationPojo>> markedAnnotation = new LinkedHashMap<>();
					findAllPathsOfGroupNodesUsingDepthFirstSearch(invocationNode, groupAstNodes.get(invocationNode), new LinkedHashSet<>(), new LinkedList<>(),
							markedAnnotation, 0, groupOrderedAnnotations);
					groupOrderedAnnotations.put(invocationNode, markedAnnotation);
					groupNodesCalculationDone.add(invocationNode);
					calculateMarkedAnnotationOfEnclosedGroup(invocationNode, groupOrderedAnnotations, internalMarked, new int[] { level, neighbourNumber });
				}
			}
			return invocableReturnPlace.get(neighbourNode);
	}

	private @Nullable UUID findInnovocableNode(final UUID nodeId) {
		final var node = nodeMap.get(nodeId);
		final List<UUID> neighbourIds = adjacencyList.get(nodeId);
		for (final UUID neighbourId : neighbourIds) {
			final var neighbourNode = nodeMap.get(neighbourId);
			if (neighbourNode != null
					&& neighbourNode.properties.keySet().contains(INVOCATION_NAME) && node.properties.get(PROPERTY_TARGET) != null
					&& node.properties.get(PROPERTY_TARGET).equals(neighbourNode.properties.get(INVOCATION_NAME))) {
				return neighbourNode.id;
			}
		}
		return ! neighbourIds.isEmpty() ? neighbourIds.get(0) : null;
	}

	private void mapGroupAsts() {
		for (final var node : graph.nodes) {
			if (node.superTypes.contains(CFG_COLLAPSIBLE) && node.superTypes.contains(INVOCABLE_STATEMENT)
					|| (node.superTypes.contains(BRANCH_STATEMENT))) {
				final List<UUID> childs = new ArrayList<>();
				getAllChilds(node, node, new HashSet<>(), childs);
				childs.add(node.id);
				groupAstNodes.put(node.id, childs);
			}
		}
	}

	private void findLastNodeOfGroupNodes() {
		for (final Entry<UUID, List<UUID>> entry : groupAstNodes.entrySet()) {
			final List<UUID> groupNodes = groupAstNodes.get(entry.getKey());
			if (groupNodes != null && !groupNodes.isEmpty()) {
				final UUID lastAst = findLastNode(groupNodes);
				if (lastAst != null && nodeMap.get(lastAst) != null) {
					groupNodesLastNode.put(entry.getKey(), lastAst);
				}
			}
		}
	}

	private void findInvocableReturnNode() {
		for (final var node : graph.nodes) {
			if (checkForBranchStatement(node.id)) {
				final UUID nextAstNodeInvocableWillReturn = nextAstNodeBranchAstNodeWillReturn(node.id);
				if (nextAstNodeInvocableWillReturn != null) {
					invocableReturnPlace.put(node.id, nextAstNodeInvocableWillReturn);
				}
			} else if (checkForJumpStatement(node.id)) {
				final var nextAstNodeInvocableWillReturn = nextAstNodeInvocableWillReturn(node);
				if (nextAstNodeInvocableWillReturn != null) {
					invocableReturnPlace.put(node.id, nextAstNodeInvocableWillReturn.id);
				}
			}
		}
	}

	private @Nullable ControlFlowNode nextAstNodeInvocableWillReturn(final ControlFlowNode node) {
		ControlFlowNode closestNode = null;
		final List<UUID> innovakableNodes = adjacencyList.get(node.id);
		if (innovakableNodes != null) {
			for (final UUID currentNodeId : innovakableNodes) {
				final var currentNode = nodeMap.get(currentNodeId);
				if (currentNode != null && ! currentNode.properties.isEmpty() && currentNode.properties.get(INVOCATION_NAME) != null
						&& currentNode.properties.get(INVOCATION_NAME).equals(node.properties.get(PROPERTY_TARGET))) {
					UUID lastNode = groupNodesLastNode.get(currentNodeId);
					if (lastNode != null) {
						while (checkForJumpStatement(lastNode) && adjacencyList.get(lastNode) != null
								&& ! adjacencyList.get(lastNode).isEmpty()) {
							final List<UUID> edgesFromLastNode = adjacencyList.get(lastNode);
							final UUID invocable = edgesFromLastNode.get(0);
							lastNode = groupNodesLastNode.get(invocable);
							if (lastNode == null) {
								break;
							}
						}
					}
					if (lastNode != null) {
						final List<UUID> pathsFromlastNode = adjacencyList.get(lastNode);
						if (pathsFromlastNode != null && ! pathsFromlastNode.isEmpty()) {
							for (final UUID eachNodeFromLastNodeId : pathsFromlastNode) {
								if (checkForInvocableStatement(eachNodeFromLastNodeId)) {
									UUID findNextPossibleClosetNode = eachNodeFromLastNodeId;
									while (checkForInvocableStatement(findNextPossibleClosetNode)) {
										final UUID lastNodeOfInvocable = groupNodesLastNode.get(findNextPossibleClosetNode);
										if (lastNodeOfInvocable != null) {
											findNextPossibleClosetNode = lastNodeOfInvocable;
										} else {
											break;
										}
									}
									if(findNextPossibleClosetNode != null) {
										final List<UUID> pathsFromNextPossibleClosetNodeIds = adjacencyList.get(findNextPossibleClosetNode);
										if(pathsFromNextPossibleClosetNodeIds != null && ! pathsFromNextPossibleClosetNodeIds.isEmpty()) {
											for (final UUID pathFromNextPossibleClosetNodeId : pathsFromNextPossibleClosetNodeIds) {
												final var pathFromNextPossibleClosetNode = nodeMap.get(pathFromNextPossibleClosetNodeId);
												if (pathFromNextPossibleClosetNode != null) {
													final var possibleClosestNode = compareoffsetOfNodes(pathFromNextPossibleClosetNode, node, closestNode);
													if(possibleClosestNode != null) {
														closestNode = possibleClosestNode;
													}
												}
											}
										}
									}

								}
								if(eachNodeFromLastNodeId != null) {
									final var possibleClosestNode = compareoffsetOfNodes(nodeMap.get(eachNodeFromLastNodeId), node, closestNode);
									if(possibleClosestNode != null) {
										closestNode = possibleClosestNode;
									}
								}
							}
						}
					}
				}
			}
		}
		return closestNode;
	}
	
	private @Nullable ControlFlowNode compareoffsetOfNodes(final ControlFlowNode eachNodeFromLastNode,
			final ControlFlowNode node, @Nullable ControlFlowNode closestNode) {
		final Integer currentOffset = eachNodeFromLastNode.offset;
		final Integer nodeOffset = node.offset;
		if (currentOffset != null && nodeOffset != null && currentOffset > nodeOffset
				&& (closestNode == null || currentOffset < (closestNode.offset != null ? closestNode.offset : Integer.MAX_VALUE))) {
			closestNode = eachNodeFromLastNode;
		}
		return closestNode;
	}

	private @Nullable UUID nextAstNodeBranchAstNodeWillReturn(final UUID node) {
		UUID closestNode = null;
		final Set<UUID> visitedNodes = new HashSet<>();
		final Queue<UUID> traversalQueue = new LinkedList<>();
		traversalQueue.add(node);
		final List<UUID> groupNodes = groupAstNodes.get(node);
		while (! traversalQueue.isEmpty()) {
			final UUID currentNode = traversalQueue.poll();
			if (groupNodes.contains(currentNode)) {
				for (final UUID neighborNode : adjacencyList.getOrDefault(currentNode, new ArrayList<>())) {
					if( ! groupNodes.contains(neighborNode) && loopExitNodes.containsKey(currentNode) &&
							loopExitNodes.get(currentNode).equals(neighborNode)) {
						if( ! checkForInvocableStatement(neighborNode)) {
							return neighborNode;
						} else {
							UUID findNextPossibleClosetNode = neighborNode;
							while (checkForInvocableStatement(findNextPossibleClosetNode)) {
								final UUID lastNodeOfInvocable = groupNodesLastNode.get(findNextPossibleClosetNode);
								if (lastNodeOfInvocable != null) {
									findNextPossibleClosetNode = lastNodeOfInvocable;
								} else {
									break;
								}
							}
							final List<UUID> pathsFromNextPossibleClosetNodeIds = adjacencyList.get(findNextPossibleClosetNode);
							for (final UUID pathFromNextPossibleClosetNodeId : pathsFromNextPossibleClosetNodeIds) {
								final var pathFromNextPossibleClosetNode = nodeMap.get(pathFromNextPossibleClosetNodeId);
								if (pathFromNextPossibleClosetNode != null) {
									final var possibleClosestNode = compareoffsetOfNodes(pathFromNextPossibleClosetNode,
											nodeMap.get(node), nodeMap.get(closestNode));
									if (possibleClosestNode != null) {
										closestNode = possibleClosestNode.id;
									}
								}
							}
							return closestNode;
						}
					} else if (checkForJumpStatement(neighborNode)) {
						final var nextAstNodeInvocableWillReturn = nextAstNodeInvocableWillReturn(nodeMap.get(neighborNode));
						if (nextAstNodeInvocableWillReturn != null) {
							if ( ! groupNodes.contains(nextAstNodeInvocableWillReturn.id)) {
								return nextAstNodeInvocableWillReturn.id;
							} else {
								if ( ! visitedNodes.contains(neighborNode)) {
									visitedNodes.add(nextAstNodeInvocableWillReturn.id);
									traversalQueue.add(nextAstNodeInvocableWillReturn.id);
								}
							}
						}
					} else if ( ! checkForInvocableStatement(neighborNode) && ! visitedNodes.contains(neighborNode)) {
						visitedNodes.add(neighborNode);
						traversalQueue.add(neighborNode);
					}

				}
			} else {
				return currentNode;
			}
		}
		return null;
	}

	private @Nullable UUID findLastNode(final List<UUID> groupNodes) {
		UUID lastNode = groupNodes.get(0);
		final var lastAstNode = nodeMap.get(lastNode);
		for (final UUID nextNodeId : groupNodes) {
			final var nextNode = nodeMap.get(nextNodeId);
			final Integer lastAstNodeOffset = lastAstNode.offset;
			final Integer nextAstNodeOffset = nextNode.offset;
			if (lastAstNodeOffset == null || (nextAstNodeOffset != null && lastAstNodeOffset < nextAstNodeOffset)) {
				lastNode = nextNodeId;
			}
		}
		final Set<UUID> visited = new HashSet<>();
		final Deque<UUID> stack = new ArrayDeque<>();
		UUID validChild = null;
		UUID possibleValidChilds = null;
		stack.push(lastNode);
		while ( ! stack.isEmpty()) {
			final UUID currentNode = stack.pop();
			visited.add(currentNode);
			final List<UUID> children = adjacencyList.getOrDefault(currentNode, new ArrayList<>());
			boolean hasValidChild = false;
			for (final UUID child : children) {
				if (groupNodes.contains(child)) {
					hasValidChild = true;
					if ( ! visited.contains(child)) {
						stack.push(child);
					}
				} else {
					if (possibleValidChilds != null) {
						final var currentAstNode = nodeMap.get(currentNode);
						final var possibleChildAstNode = nodeMap.get(possibleValidChilds);
						final Integer currentOffset = currentAstNode.offset;
						final Integer possibleChildNodeOffset = possibleChildAstNode.offset;
						if (currentOffset != null && possibleChildNodeOffset != null && currentOffset > possibleChildNodeOffset) {
							possibleValidChilds = currentNode;
						}
					} else {
						possibleValidChilds = currentNode;
					}
				}
			}
			if ( ! hasValidChild) {
				validChild = currentNode;
			}
		}
		return validChild != null ? validChild : possibleValidChilds;
	}
	
	private List<ControlFlowNode> getChilds(final ControlFlowNode parent) {
		final List<ControlFlowNode> children = nodeMap.values().stream().filter(node -> parent.id.equals(node.parent)).collect(Collectors.toList());
		/* When creating the CFG we add annotation with type CfgCollapsibleNode and switch the parent id of the containing ast nodes to it */
		children.addAll(children.stream().filter(node -> node.entity == ControlFlowEntity.ANNOTATION).flatMap(node -> getChilds(node).stream()).toList());
		return children;
	}
	
	private void getAllChilds(final ControlFlowNode startNode, final ControlFlowNode currentNode, final Set<UUID> visited, final List<UUID> childs) {
		if (visited.contains(currentNode.id)) {
			return;
		}
		visited.add(currentNode.id);
		final List<ControlFlowNode> children = getChilds(currentNode);
		if (! children.isEmpty()) {
			for (final var childId : children) {
				if (nodeMap.containsKey(childId.id)) {
					childs.add(childId.id);
					getAllChilds(startNode, childId, visited, childs);
				}
			}
		}
	}

	private void buildAdjacencyList() {
		for (final var edge : graph.edges) {
			final UUID fromNode = edge.fromId;
			final UUID toNode = edge.toId;
			if (edge.label != null && EXIT_LOOP_LABEL.equals(edge.label)) {
				loopExitNodes.put(fromNode, toNode);
			}
			final List<UUID> neighbors = adjacencyList.computeIfAbsent(fromNode, k -> new ArrayList<>());
			if (!neighbors.contains(toNode)) {
				int insertionIndex = 0;
				final var toNodeObj = nodeMap.get(toNode);
				try {
					while (insertionIndex < neighbors.size() 
							&& Objects.requireNonNull(nodeMap.get(neighbors.get(insertionIndex)).offset) < Objects.requireNonNull(toNodeObj.offset)) {
						insertionIndex++;
					}
				} catch (final Exception e) {
					logger.error("Error occured at time of calculating AdjacencyList of OrderAnnotationRule: {}", e.getMessage());
				}
				neighbors.add(insertionIndex, toNode);
			}
		}
	}

	private void markAnnotation(final UUID currentNode, final Map<int[], List<AnnotationPojo>> orderedAnnotations, final int[] key) {
		final AnnotationPojo annotation = astAndAnnotationMap.get(currentNode);
		if (annotation != null && annotation.getSourceAttachment().isPresent()) {
			orderedAnnotations.putIfAbsent(key, new ArrayList<>());
			orderedAnnotations.get(key).add(annotation);
		}
	}

	private void calculateMarkedAnnotationOfEnclosedGroup(final UUID invocationNode,
			final Map<UUID, Map<int[], List<AnnotationPojo>>> groupOrderedAnnotations,
			final Map<int[], List<AnnotationPojo>> internalOrderedAnnotations,
			final int[] level) {
		final Map<int[], List<AnnotationPojo>> markedAnnotations = groupOrderedAnnotations.get(invocationNode);
		if(markedAnnotations != null) {
			for (final Entry<int[], List<AnnotationPojo>> entry : markedAnnotations.entrySet()) {
				internalOrderedAnnotations.put(ArrayUtils.addAll(level, entry.getKey()), entry.getValue());
			}
		}
	}

	private void  sortGroupNodes(final Map<UUID, List<UUID>> groupAstNodes) {
		final List<Map.Entry<UUID, List<UUID>>> entryList = new ArrayList<>(groupAstNodes.entrySet());
		final Comparator<Map.Entry<UUID, List<UUID>>> customComparator = (entry1, entry2) -> {
			final List<UUID> list1 = entry1.getValue();
			final List<UUID> list2 = entry2.getValue();
			final boolean matchInList1 = list1.stream().anyMatch(
					str -> nodeMap.get(str).superTypes.contains(JUMP_STATEMENT) && nodeMap.get(str).properties.keySet().contains(PROPERTY_TARGET));
			final boolean matchInList2 = list2.stream().anyMatch(
					str -> nodeMap.get(str).superTypes.contains(JUMP_STATEMENT) && nodeMap.get(str).properties.keySet().contains(PROPERTY_TARGET));
			if (matchInList1 && ! matchInList2) {
				return 1;
			} else if ( ! matchInList1 && matchInList2) {
				return -1;
			} else {
				return 0;
			}
		};
		entryList.sort(customComparator);
		groupAstNodes.clear();
		for (final Map.Entry<UUID, List<UUID>> entry : entryList) {
			groupAstNodes.put(entry.getKey(), entry.getValue());
		}
	}

	private void mapAstAndAnnotation() {
		for (final AnnotationPojo annotation : annotationList) {
			annotation.getLocation().ifPresent(location -> {
				final var closetAstNode = findAstNodeForModuleLocation(graph.nodes, location);
				if (closetAstNode != null) {
					astAndAnnotationMap.put(closetAstNode.id, annotation);
				}
			});
		}
	}

	@Nullable
	private ControlFlowNode findAstNodeForModuleLocation(final List<ControlFlowNode> listOfNodes, final ModuleLocation location) {
		ControlFlowNode closestNode = null;
		int minStartDistance = Integer.MAX_VALUE;
		final int endDistance = location.getOffset() + location.getLength();
		for (final var node : listOfNodes) {
			final Integer offset = node.offset;
			if (offset != null && node.entity != ControlFlowEntity.ANNOTATION) {
				if (offset.intValue() == location.getOffset().intValue()) {
					return node;
				} else if (offset.intValue() > location.getOffset().intValue() && offset.intValue() <= endDistance) {
					if (closestNode == null || Objects.requireNonNull(closestNode.offset).intValue() > offset.intValue()) {
						closestNode = node;
					}
					final int startDistance = Math.abs(offset - location.getOffset());
					if (startDistance <= minStartDistance) {
						closestNode = node;
						minStartDistance = startDistance;
					}
				}
			}
		}
		return closestNode;
	}

	private Map<Integer, AnnotationPojo> sortOrderAnnotationMap(final Map<int[], List<AnnotationPojo>> orderedAnnotations) {
		final List<int[]> keys = new ArrayList<>(orderedAnnotations.keySet());
		Collections.sort(keys, new CustomComparator());
		final Map<Integer, AnnotationPojo> sortedMap = new LinkedHashMap<>();
		int order = 1;
		for (final int[] key : keys) {
			final List<AnnotationPojo> orderAnnotations = orderedAnnotations.get(key);
			for (final AnnotationPojo orderAnnotation : orderAnnotations) {
				sortedMap.put(order++, orderAnnotation);
			}
		}
		return sortedMap;
	}

	class CustomComparator implements Comparator<int[]> {
		@Override
		public int compare(@Nullable final int[] key1, @Nullable final int[] key2) {
			if (key1 != null && key2 != null) {
				final int length = Math.min(key1.length, key2.length);
				for (int index = 0; index < length; index++) {
					final int num1 = key1[index];
					final int num2 = key2[index];
					if (num1 != num2) {
						return Integer.compare(num1, num2);
					}
				}
				return Integer.compare(key1.length, key2.length);
			}
			return 0;
		}
	}

	private boolean checkForBranchStatement(final UUID nodeId) {
		final var node = nodeMap.get(nodeId);
		return node != null && node.superTypes.contains(BRANCH_STATEMENT);
	}

	private boolean checkForJumpStatement(final UUID nodeId) {
		final var node = nodeMap.get(nodeId);
		return node != null && node.superTypes.contains(JUMP_STATEMENT) && node.properties.keySet().contains(PROPERTY_TARGET);
	}

	private boolean checkForInvocableStatement(final UUID nodeId) {
		final var node = nodeMap.get(nodeId);
		return node != null && node.superTypes.contains(INVOCABLE_STATEMENT);
	}
}
