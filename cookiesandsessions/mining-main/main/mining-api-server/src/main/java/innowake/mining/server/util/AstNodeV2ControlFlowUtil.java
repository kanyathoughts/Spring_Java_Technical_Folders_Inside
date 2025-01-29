/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.util;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.model.AstNodeLocation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Utility class to traversal through AstNodePojo using Flow Control to find information.
 */
@Service
public class AstNodeV2ControlFlowUtil implements Serializable {

	@Autowired
	private transient AstService astService;

	private static final Logger LOG = LoggerFactory.getLogger(AstNodeV2ControlFlowUtil.class);

	private static final String BRANCH_STATEMENT = "BranchStatement";
	private static final String FILE_ACCESS = "FileAccessStatement";
	private static final String EXIT_LOOP = "EXIT LOOP";
	private static final String GOBACK = "GOBACK";
	private static final String STOP_RUN = "STOP RUN";
	private static final String EVAL = "EVALUATE";
	private static final String AT_END = "AT END";
	private static final String NOT_AT_END = "NOT AT END";

	public List<Tuple2<AstNodePojo, String>> findConditions(final EntityId moduleId, final ModuleLocation location) {
		/* finding AstNode for the moduleLocation */
		long startTime =  System.currentTimeMillis();
		AstNodePojo startNode;
		final int offset = location.getOffset().intValue();
		final int length = location.getLength().intValue();
		final List<AstNodePojo> moduleLocationNodes = astService.find(q -> q.ofModule(moduleId)
				.withRetracedOffset(Comperator.GREATER_OR_EQUAL, offset)
				.withRetracedEndOffset(Comperator.LESSER_OR_EQUAL, offset + length));
		long endTime = System.currentTimeMillis();
		LOG.info("Fetched the AstNode for moduleId - {} and moduleLocation(offset - {}, length - {}) in {} sec.",
				moduleId, offset, length, (endTime - startTime)/1000);
		startNode = findAstNodeForModuleLocation(moduleLocationNodes, location);
		AstNodePojo endNode = null;
		if (startNode == null) {
			final List<AstNodePojo> moduleAstNodes = astService.find(q -> q.ofModule(moduleId));
			final Tuple2<AstNodePojo, AstNodePojo> startAndEndNode = findStartAndEndAstNodeForModuleLocation(moduleAstNodes, location);
			startNode = startAndEndNode.a;
			endNode = startAndEndNode.b;
		} else {
			/* finding Root AstNode to transverse down to top for finding all conditions details */
			startTime = System.currentTimeMillis();
			final List<AstNodePojo> rootNodes = astService.find(q -> q.ofModule(moduleId).ofParent(null));
			if(! rootNodes.isEmpty()){
				endNode = rootNodes.get(0);
			}
			endTime = System.currentTimeMillis();
			LOG.info("Fetched the Root AstNode for moduleId {} in {} sec.", moduleId, (endTime - startTime)/1000);
		}

		/* Now Transverse the CGF from bottom to top and mark all the branch statements */
		final List<Tuple2<AstNodePojo, String>> branchStatements = new ArrayList<>();
		LOG.info("Transversal to find Branch statement is started for moduleId - {} and moduleLocation(offset - {}, length - {}).",
				moduleId, offset, length);
		startTime = System.currentTimeMillis();
		if (endNode != null) {
			/* Use UUIDs: AstNodePojo does not implement equals() or hashCode() */
			final Set<UUID> marked = new HashSet<>();
			final Map<UUID, Boolean> visited = new HashMap<>();
			final List<UUID> currentPath = new ArrayList<>();
			final AstNodeLocation astNodeLocation = startNode.getLocation();
			LOG.info("Start Branch node - {}", startNode.getLabel());
			traversal(startNode, endNode, visited, currentPath, marked, branchStatements, new ArrayList<>(),
					new ModuleLocation(astNodeLocation.getRetracedOffset().orElseThrow(), astNodeLocation.getRetracedLength().orElseThrow()), startNode);
		}
		endTime = System.currentTimeMillis();
		LOG.info("Transversal to find Branch statement completed for moduleId - {} and moduleLocation(offset - {}, length - {}) in {} sec",
				moduleId, offset, length, (endTime - startTime)/1000);
		Collections.reverse(branchStatements);
		return (branchStatements);
	}

	private Tuple2<AstNodePojo, AstNodePojo> findStartAndEndAstNodeForModuleLocation(final List<AstNodePojo> moduleAstNodes, final ModuleLocation location) {
		AstNodePojo endNode = null;
		AstNodePojo closestNode = null;
		AstNodePojo startNode = null;
		int minStartDistance = Integer.MAX_VALUE;
		int minEndDistance = Integer.MAX_VALUE;
		for (final AstNodePojo node : moduleAstNodes) {

			final List<AstRelationshipPojo> inFlowsControls = astService.findRelationships(q -> q.withType(AstRelationshipType.FLOW).ofDestination(node.getId()));

			if (node.getParent().isEmpty()) {
				endNode = node;
			}
			final AstNodeLocation advancedModuleLocationV2 = node.getLocation();
			if (! inFlowsControls.isEmpty() && advancedModuleLocationV2 != null) {
				final Optional<Integer> offset = advancedModuleLocationV2.getRetracedOffset();
				final Optional<Integer> length = advancedModuleLocationV2.getRetracedLength();
				if (offset.isPresent() && length.isPresent()) {
					if (offset.get().intValue() == location.getOffset().intValue() &&
							(offset.get() + length.get()) == (location.getOffset() + location.getLength())) {
						startNode = node;
					} else {
						final int startDistance = Math.abs(offset.get() - location.getOffset());
						final int endDistance = Math.abs((offset.get() + length.get()) - (location.getOffset() + location.getLength()));
						if (startDistance < minStartDistance || (startDistance == minStartDistance && endDistance < minEndDistance)) {
							closestNode = node;
							minStartDistance = startDistance;
							minEndDistance = endDistance;
						}
					}
				}
			}
		}
		return new Tuple2<>(Objects.requireNonNull(startNode != null ? startNode : closestNode), Objects.requireNonNull(endNode));
	}

	private @Nullable AstNodePojo findAstNodeForModuleLocation(final List<AstNodePojo> listOfAstNodes, final ModuleLocation location) {
		AstNodePojo closestNode = null;
		int minStartDistance = Integer.MAX_VALUE;
		int minEndDistance = Integer.MAX_VALUE;
		for (final AstNodePojo node : listOfAstNodes) {
			final List<AstRelationshipPojo> inFlowsControls = astService.findRelationships(q -> q.withType(AstRelationshipType.FLOW).ofDestination(node.getId()));
			final AstNodeLocation advancedModuleLocationV2 = node.getLocation();
			if (! inFlowsControls.isEmpty() && advancedModuleLocationV2 != null) {
				final Optional<Integer> offset = advancedModuleLocationV2.getRetracedOffset();
				final Optional<Integer> length = advancedModuleLocationV2.getRetracedLength();
				if (offset.isPresent() && length.isPresent()) {
					if (offset.get().intValue() == location.getOffset().intValue() &&
							(offset.get() + length.get()) == (location.getOffset() + location.getLength())) {
						return node;
					} else {
						final int startDistance = Math.abs(offset.get() - location.getOffset());
						final int endDistance = Math.abs((offset.get() + length.get()) - (location.getOffset() + location.getLength()));
						if (startDistance < minStartDistance || (startDistance == minStartDistance && endDistance < minEndDistance)) {
							closestNode = node;
							minStartDistance = startDistance;
							minEndDistance = endDistance;
						}
					}
				}
			}
		}
		return closestNode;
	}
	
	private void traversal(final AstNodePojo currentNode, final AstNodePojo endNode, final Map<UUID, Boolean> visited, final List<UUID> path,
						   final Set<UUID> marked, final List<Tuple2<AstNodePojo, String>> branchStatements, final List<String> pathLabel, final ModuleLocation location,
						   final AstNodePojo startNode) {
		/* check whether current node is same as endNode or not */
		if (currentNode.getId().equals(endNode.getId())) {
			return;
		}
		/* Check if the current node is already visited */
		if (isNodeAlreadyVisited(currentNode, visited)) {
			return;
		}

		/* Add the current node to the path */
		path.add(currentNode.getId());

		/* Now travels through neighbourNode until you reach endNode */
		traversalOfNeighbourNodes(currentNode, endNode, visited, path, marked, branchStatements, pathLabel, location, startNode);

		/* Remove the current node from the path */
		path.remove(path.size() - 1);
	}

	private void traversalOfNeighbourNodes(final AstNodePojo currentNode, final AstNodePojo endNode,
			final Map<UUID, Boolean> visited, final List<UUID> path, final Set<UUID> marked,
			final List<Tuple2<AstNodePojo, String>> branchStatements, final List<String> pathLabel,
			final ModuleLocation location, final AstNodePojo startNode) {
		final List<AstRelationshipPojo> neighbours = astService.findRelationships(q -> q.withType(AstRelationshipType.FLOW).ofDestination(currentNode.getId()));
		for (final AstRelationshipPojo neighborRelationship : neighbours) {
			final AstNodePojo neighbourNode = neighborRelationship.getSrcNode();
			if (path.contains(neighbourNode.getId()) || Boolean.TRUE.equals(visited.get(neighbourNode.getId()))) {
				visited.put(neighbourNode.getId(), true);
				continue;
			}
			neighborRelationship.getLabel().ifPresent(pathLabel::add);
			markBranchStatement(marked, branchStatements, currentNode, pathLabel, location, startNode);
			traversal(neighbourNode, endNode, visited, path, marked, branchStatements, pathLabel, location, startNode);
		}

		final List<AstModuleRelationshipPojo> entries = astService.findModuleRelationships(q -> q.withType(AstModuleRelationshipType.ENTRY).ofNode(currentNode.getId()));
		if ( ! entries.isEmpty()) {
			markBranchStatement(marked, branchStatements, currentNode, pathLabel, location, startNode);
		}
	}

	private boolean isNodeAlreadyVisited(final AstNodePojo currentNode, final Map<UUID, Boolean> visited) {
		if ( visited.containsKey(currentNode.getId())) {
			if ( Boolean.TRUE.equals(visited.get(currentNode.getId()))) {
				/* The current node is part of a cycle, mark it as visited to avoid infinite recursion */
				return true;
			} else {
				/* The current node is not part of a cycle, mark it as visited and continue traversal */
				visited.put(currentNode.getId(), true);
			}
		} else {
			visited.put(currentNode.getId(), false);
		}
		return false;
	}

	void markBranchStatement(final Set<UUID> marked, final List<Tuple2<AstNodePojo, String>> branchStatements, final AstNodePojo currentNode,
			final List<String> pathLabel, final ModuleLocation location, final AstNodePojo startNode) {
		try {
			if ( ! marked.contains(currentNode.getId()) && ! currentNode.getSuperTypes().isEmpty() && currentNode.getSuperTypes().contains(BRANCH_STATEMENT)
					&& ! currentNode.getSuperTypes().contains(FILE_ACCESS)) {
				marked.add(currentNode.getId());
				LOG.info("Current Branch node - {}", currentNode.getLabel());
				final AstNodeLocation advancedModuleLocation = assertNotNull(currentNode.getLocation());
				final int startOffset = advancedModuleLocation.getRetracedOffset().orElseThrow();
				boolean isFalseBranch = false;
				boolean hasGtBranchOffset = false;
				/* we don't return the starting node as branch statement */
				if (location.getOffset() == startOffset) {
					isFalseBranch = true;
				}
				/* we exclude nodes with label EVALUATE since they are not true or false branch */
				boolean added = false;
				if (location.getOffset() > startOffset && ! currentNode.getLabel().contains(EVAL)) {
					/* check to add the immediate if node of the else block */
					if (branchStatements.isEmpty()) {
						final List<AstRelationshipPojo> outNeighbours = currentNode
								.getOutgoingRelations().stream().filter(r -> r.getType().equals(AstRelationshipType.FLOW)).toList();
						if (outNeighbours != null) {
							for (final var neighbour : outNeighbours) {
								final AstNodePojo neighbourNode = neighbour.getDstNode();
								if ("FALSE".equals(neighbour.getLabel().orElse(null)) &&
										neighbourNode.getLabel().contains("ELSE " + startNode.getLabel())) {
									branchStatements.add(new Tuple2<>(currentNode, pathLabel.get(0)));
									added = true;
								}
							}
						}
					}
					isFalseBranch = checkForFalseBranch(branchStatements, currentNode, startNode);
				}
				/* we exclude nodes whose location is greater than identified branch statements as they would not be part of the flow */
				if ( ! branchStatements.isEmpty()) {
					for (final Tuple2<AstNodePojo, String> branchStatement : branchStatements) {
						final int branchOffset = branchStatement.a.getLocation().getRetracedOffset().orElseThrow();
						if (startOffset > branchOffset) {
							hasGtBranchOffset = true;
							break;
						}
					}
				}

				if (! added && location.getOffset() > startOffset && ! isFalseBranch && ! hasGtBranchOffset) {
					branchStatements.add(new Tuple2<>(currentNode, pathLabel.get(0)));
				}
				pathLabel.clear();
			}
		} catch (final Exception ex) {
			LOG.error("An exception occurred for astNode:{} error:{}", currentNode.getId(), ex);
		}
	}

	private boolean checkForFalseBranch (final List<Tuple2<AstNodePojo, String>> branchStatements, final AstNodePojo currentNode, final AstNodePojo startNode) {
		boolean isFalseBranch = false;
		AstNodePojo node = currentNode;
		var outNeighbours = node.getOutgoingRelations().stream().filter(r -> r.getType().equals(AstRelationshipType.FLOW)).toList();
		while ( ! isFalseBranch && ! outNeighbours.isEmpty() && ! outNeighbours.get(0).getDstNode().getLabel().equals(STOP_RUN)
				&& ! outNeighbours.get(0).getDstNode().getLabel().equals(GOBACK) && ! outNeighbours.get(0).getDstNode().getLabel().contains(EVAL)
				&& node.getLabel() != null && ! node.getLabel().contains(AT_END) && ! node.getLabel().contains(NOT_AT_END)) {
			final var lastNode = node;
			for (final var neighbourId : outNeighbours) {
				final AstNodePojo neighbourNode = neighbourId.getDstNode();
				/* Check if the false node leads to the identifed branch nodes or the starting node then we ignore the node from the branch list */
				if (neighbourId.getLabel().isEmpty() || "FALSE".equals(neighbourId.getLabel().orElse(null))
						|| EXIT_LOOP.equals(neighbourId.getLabel().orElse(null))) {
					if ( ! branchStatements.isEmpty()) {
						/* Check if the current node leads to the already identified branch statement then we ignore the node from branch list */
						for (final Tuple2<AstNodePojo, String> branchStatement : branchStatements) {
							if (branchStatement.a.getId().equals(neighbourNode.getId())) {
								isFalseBranch = true;
								break;
							}
						}
					}
					/* Check if the current node is same as startNode then we ignore the node from branch list */
					if (startNode.getId().equals(neighbourNode.getId())) {
						isFalseBranch = true;
						break;
					}
					if ( ! isFalseBranch) {
						node = neighbourNode;
						outNeighbours = node.getOutgoingRelations().stream().filter(r -> r.getType().equals(AstRelationshipType.FLOW)).toList();
					}
					break;
				}
			}
			/* avoid infinite loop */
			if (lastNode == node) {
				final var n = node;
				LOG.debug(() -> "Check for false branch statement: Infinite loop detected for node: " + n);
				break;
			}
		}
		return isFalseBranch;
	}
}
