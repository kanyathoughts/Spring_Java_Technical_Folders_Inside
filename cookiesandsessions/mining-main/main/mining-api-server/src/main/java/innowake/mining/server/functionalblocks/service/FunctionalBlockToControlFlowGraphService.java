/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.shared.entities.functionalblocks.ExcludedBranch;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.controlflow.ControlFlowEntity;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;

import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.controlflow.ControlFlowEdge;
import innowake.mining.shared.model.controlflow.ControlFlowNode;

/**
 * Constructs a filtered {@link ControlFlowGraph} from the code locations referenced by a functional block.
 * that would be visited when traversing from an entry point to the retained statements. The Excluded type Annotation nodes
 * will be coming together as a separate node in the CFG with name Excluded Code.
 */
@Service
public class FunctionalBlockToControlFlowGraphService {

	private static final Set<String> END_NODE_TYPES = Stream.of(AstModuleRelationshipType.values()).map(Enum :: name).collect(Collectors.toSet());
	private static final String DEFAULT_BRANCH = "DefaultBranch";
	private static final String BRANCH = "Branch";

	/**
	 * Label for artificial "HaltPoint" node that represents code areas annotated as "EXCLUDED".
	 */
	private static final String EXCLUDED_CODE_LABEL = "Excluded Code";

	private final AstService astService;
	private final FunctionalBlockService functionalBlockService;
	private final ModuleService moduleService;
	private final AnnotationToFunctionalBlockService annotationToFunctionalBlockService;

	public FunctionalBlockToControlFlowGraphService(final AstService astService, final ModuleService moduleService,
			final FunctionalBlockService functionalBlockService, final AnnotationToFunctionalBlockService annotationToFunctionalBlockService) {
		this.astService = astService;
		this.functionalBlockService = functionalBlockService;
		this.moduleService = moduleService;
		this.annotationToFunctionalBlockService = annotationToFunctionalBlockService;
	}

	/**
	 * Converts the child blocks and links found on a FunctionalBlockPojo into a ControlFlowGraph object. Each node in the graph will represent
	 * one child block and a control flow edge will be created if there is a link between the child blocks. The Excluded type Annotation nodes
	 * will be coming together as a separate node in the CFG with name Excluded Code.
	 * <p>
	 * Currently, this will produce useful results (only) when each child block represents a code location within the same module.
	 * @param block the parent block
	 * @return ControlFlowGraph representing the child blocks
	 */
	public ControlFlowGraph toControlFlowGraph(final FunctionalBlockPojo block) {
		final List<FunctionalBlockPojo> children = functionalBlockService.find(
				q -> q.withTypes(List.of(FunctionalBlockType.FUNCTIONAL_CONDITION, FunctionalBlockType.FUNCTIONAL_STATEMENT))
						.withParent(p -> p.byUid(block.getUid())));

		final Map<String, ControlFlowNode> groupNodes = new HashMap<>();
		final Map<UUID, ControlFlowNode> nodes = new HashMap<>(children.size());
		final List<UUID> nodeRecordIds = new ArrayList<>(children.size());
		final List<ControlFlowNode> statementAndConditionNodes = new ArrayList<>();

		createNodesFromStatementAndConditions(block, children, groupNodes, nodes, statementAndConditionNodes, nodeRecordIds);

		List<FunctionalBlockPojo> functionalUnits = functionalBlockService.find(
				q -> q.withTypes(List.of(FunctionalBlockType.FUNCTIONAL_UNIT)).withParent(p -> p.byUid(block.getUid())));

		final Map<UUID, AnnotationPojo> annotationForFunctionalUnits = annotationToFunctionalBlockService.getAnnotationForFunctionalUnits(block.getProject(),
				functionalUnits.stream().map(FunctionalBlockPojo :: getUid).toList());

		createNodesFromFunctionalUnits(block, annotationForFunctionalUnits, statementAndConditionNodes, nodes, nodeRecordIds);

		final List<ControlFlowEdge> edges = new ArrayList<>();
		createEdges(block, nodes, edges);

		final ControlFlowGraph cfg = new ControlFlowGraph();
		cfg.nodes.addAll(nodes.values());
		cfg.edges.addAll(edges);
		cfg.annotations.addAll(
				nodes.values().stream().filter(node -> ControlFlowEntity.ANNOTATION.equals(node.entity)).map(node -> annotationForFunctionalUnits.get(node.id))
						.filter(Objects :: nonNull).toList());

		addEntryPoint(children, block.getUid(), cfg, nodeRecordIds);
		addHaltPoint(children, block.getUid(), cfg, nodeRecordIds);

		return cfg;
	}

	/**
	 * Computes a {@code ControlFlowGraph} by selecting all child blocks of type {@link FunctionalBlockType#FUNCTIONAL_UNIT} (recursively).
	 * The resulting graph will only contain the statements represented by those FUNCTIONAL_UNITs
	 * and any conditions that are on a path from the entry point of the module to one of the statements.
	 *
	 * @param block the parent block containing the FUNCTIONAL_UNITs
	 * @return ControlFlowGraph object representing FUNCTIONAL_UNITs and associated conditions
	 */
	public ControlFlowGraph toControlFlowGraphOfFunctionalUnits(final FunctionalBlockPojo block) {
		final List<FunctionalBlockPojo> functionalUnits = functionalBlockService.findChildrenDeep(block.getUid(), - 1,
				q -> q.withType(FunctionalBlockType.FUNCTIONAL_UNIT));
		final List<ModulePart> resolvedModuleParts = functionalUnits.stream().flatMap(b -> b.getModuleParts().stream()).toList();
		final List<String> moduleLinkHashes = resolvedModuleParts.stream().map(ModulePart :: getModuleLinkHash).distinct().toList();

		final ControlFlowGraph combinedCfg = new ControlFlowGraph();
		for (final String moduleLinkHash : moduleLinkHashes) {
			final List<ModulePart> partsForModule = resolvedModuleParts.stream().filter(part -> part.getModuleLinkHash().equals(moduleLinkHash)).toList();

			final Long moduleId = moduleService.findAnyModule(q -> q.ofProject(block.getProject()).withLinkHash(moduleLinkHash))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module with link hash: " + moduleLinkHash)).getId();

			final ControlFlowGraph cfg = astService.getControlFlow(EntityId.of(moduleId), null);

			if ( partsForModule.stream().anyMatch(part -> part.getLocation().isEmpty()) ) {
				/* the functional block references the entire module so there's no filtering that can be done on the CFG */
				combinedCfg.nodes.addAll(cfg.nodes);
				combinedCfg.edges.addAll(cfg.edges);
				continue;
			}

			final ControlFlowGraph filteredCfg = filterControlFlowGraph(cfg,
					partsForModule.stream().map(ModulePart :: getLocation).filter(Optional :: isPresent).map(Optional :: get).toList(),
					Collections.emptyList(),
					Collections.emptyList());

			combinedCfg.nodes.addAll(filteredCfg.nodes);
			combinedCfg.edges.addAll(filteredCfg.edges);
		}

		return combinedCfg;
	}

	/**
	 * Filters a ControlFlowGraph, retaining only statements that are referenced by the given {@code referencedLocations} and any conditions
	 * that are on a path from the entry point to any referenced statement.
	 *
	 * @param cfg the control flow graph to filter
	 * @param referencedLocations the referenced locations that shall be retained
	 * @param excludedLocations the locations that shall be excluded
	 * @param excludedBranches the branches that shall be excluded
	 *
	 * @return the filtered control flow graph
	 */
	public ControlFlowGraph filterControlFlowGraph(final ControlFlowGraph cfg, final List<ModuleLocation> referencedLocations,
			final List<ModuleLocation> excludedLocations, final List<ExcludedBranch> excludedBranches) {
		/* collect all statements that are referenced and all entry and exit points as "targets" */
		final Set<ControlFlowNode> targets = collectTargets(cfg, referencedLocations);
		return getFilteredControlFlowGraph(cfg, targets, excludedLocations, excludedBranches);
	}

	private void createNodesFromStatementAndConditions(final FunctionalBlockPojo block, final List<FunctionalBlockPojo> children,
			final Map<String, ControlFlowNode> groupNodes, final Map<UUID, ControlFlowNode> nodes, final List<ControlFlowNode> statementAndConditionNodes,
			final List<UUID> nodeRecordIds) {
		for (final FunctionalBlockPojo child : children) {
			final Optional<ModuleLocation> location = child.getModuleParts().stream().findAny().flatMap(ModulePart :: getLocation);
			if ( location.isEmpty() ) {
				return;
			}

			final var node = new ControlFlowNode(child.getUid(), block.getUid(), ControlFlowEntity.AST_NODE);
			node.label = child.getName();
			node.offset = location.get().getOffset();
			node.length = location.get().getLength();

			createGroups(block, child, groupNodes, nodes, node);
			if ( FunctionalBlockUtil.hasType(child, FunctionalBlockType.FUNCTIONAL_STATEMENT) ) {
				node.superTypes = Set.of(AstNodeUtils.STATEMENT);
				statementAndConditionNodes.add(node);
			} else {
				node.superTypes = Set.of(AstNodeUtils.BRANCH_STATEMENT);
				statementAndConditionNodes.add(node);
			}
			nodes.put(child.getUid(), node);
			nodeRecordIds.add(child.getUid());
		}
	}

	private void createGroups(final FunctionalBlockPojo block, final FunctionalBlockPojo child, final Map<String, ControlFlowNode> groupNodes,
			final Map<UUID, ControlFlowNode> nodes, final ControlFlowNode node) {
		final Optional<String> subgroup = FunctionalBlockUtil.getStringFlag(child.getFlags(), FunctionalBlockFlag.FUNCTIONAL_GROUP);
		if ( subgroup.isPresent() && ! subgroup.get().equals(block.getUid().toString()) ) {
			/* this CFG node is not directly inside the block, it represents code that is in a subgroup -> wrap in a collapsible parent node */
			final ControlFlowNode groupNode = groupNodes.computeIfAbsent(subgroup.get(), g -> {
				final Optional<FunctionalBlockPojo> subgroupBlock = functionalBlockService.find(UUID.fromString(g));
				if ( subgroupBlock.isEmpty() ) {
					return null;
				}

				final ControlFlowNode newGroup = new ControlFlowNode(UUID.randomUUID(), block.getUid(), ControlFlowEntity.AST_NODE);
				newGroup.label = subgroupBlock.get().getName();
				newGroup.type = "ANNOTATION";
				newGroup.superTypes = Set.of(AstNodeUtils.CFG_COLLAPSIBLE_NODE);

				nodes.put(newGroup.id, newGroup);
				return newGroup;
			});

			if ( groupNode != null ) {
				node.parent = groupNode.id;
			}
		}
	}

	private void createNodesFromFunctionalUnits(final FunctionalBlockPojo block, final Map<UUID, AnnotationPojo> annotationForFunctionalUnits,
			final List<ControlFlowNode> statementAndConditionNodes, final Map<UUID, ControlFlowNode> nodes, final List<UUID> nodeRecordIds) {

		for (final Map.Entry<UUID, AnnotationPojo> annotationKeySet : annotationForFunctionalUnits.entrySet()) {
			createAnnotationNode(block, statementAndConditionNodes, nodes, nodeRecordIds, annotationKeySet);
		}
	}

	private void createAnnotationNode(final FunctionalBlockPojo block, final List<ControlFlowNode> statementAndConditionNodes,
			final Map<UUID, ControlFlowNode> nodes, final List<UUID> nodeRecordIds, final Map.Entry<UUID, AnnotationPojo> annotationKeySet) {
		final Optional<ModuleLocation> location = annotationKeySet.getValue().getLocation();
		if ( ! annotationKeySet.getValue().getType().equals(AnnotationType.FUNCTIONAL) ) {
			if ( location.isEmpty() ) {
				return;
			}
			final var node = new ControlFlowNode(annotationKeySet.getKey(), block.getUid(), ControlFlowEntity.ANNOTATION);
			node.label = annotationKeySet.getValue().getName();
			node.offset = location.get().getOffset();
			node.length = location.get().getLength();
			node.type = "ANNOTATION";
			node.superTypes = Set.of(AstNodeUtils.CFG_COLLAPSIBLE_NODE);
			for (final ControlFlowNode next : statementAndConditionNodes) {
				if ( next.offset >= node.offset && ( next.offset + next.length ) <= ( node.offset + node.length ) ) {
					if ( next.parent != null ) {
						node.parent = next.parent;
					}
					next.parent = node.id;
					nodes.put(next.id, next);
				}
			}
			nodes.put(node.id, node);
			nodeRecordIds.add(node.id);
		}
	}

	private void createEdges(final FunctionalBlockPojo block, final Map<UUID, ControlFlowNode> nodes, final List<ControlFlowEdge> edges) {
		final List<FunctionalBlockLink> links = functionalBlockService.getLinks(block.getUid());
		for (final FunctionalBlockLink link : links) {
			final UUID from = link.getChildA();
			final UUID to = link.getChildB();
			if ( nodes.containsKey(from) && nodes.containsKey(to) ) {
				final var edge = new ControlFlowEdge(from, to);
				edge.label = StringUtils.trimToNull(link.getConditionLabel());
				edges.add(edge);
			}
		}
	}

	private void addEntryPoint(final List<FunctionalBlockPojo> blocks, final UUID module, final ControlFlowGraph cfg, final List<UUID> nodeRecordIds) {
		final List<FunctionalBlockPojo> entryPointBlocks =
				blocks.stream().filter(block -> block.getFlags().containsKey(FunctionalBlockFlag.ENTRY_POINT.name()))
				.toList();

		if ( entryPointBlocks.isEmpty() ) {
			return;
		}

		final ControlFlowNode entryPoint = new ControlFlowNode(UUID.randomUUID(), module, ControlFlowEntity.TERMINAL);
		entryPoint.type = AstModuleRelationshipType.ENTRY.name();
		cfg.nodes.add(entryPoint);

		for (final FunctionalBlockPojo block : entryPointBlocks) {
			if ( entryPoint.id != null && block.getUid() != null && nodeRecordIds.contains(block.getUid()) ) {
				cfg.edges.add(new ControlFlowEdge(entryPoint.id, block.getUid()));
			}
		}

	}

	private void addHaltPoint(final List<FunctionalBlockPojo> blocks, final UUID module, final ControlFlowGraph cfg, final List<UUID> nodeRecordIds) {
		final List<FunctionalBlockPojo> haltPointBlocks = blocks.stream().filter(block -> block.getFlags().containsKey(FunctionalBlockFlag.HALT_POINT.name()))
				.toList();

		if ( haltPointBlocks.isEmpty() ) {
			return;
		}

		final ControlFlowNode haltPoint = new ControlFlowNode(UUID.randomUUID(), module, ControlFlowEntity.TERMINAL);
		haltPoint.label = EXCLUDED_CODE_LABEL;
		haltPoint.type = AstModuleRelationshipType.HALT.name();
		cfg.nodes.add(haltPoint);

		for (final FunctionalBlockPojo block : haltPointBlocks) {
			if ( nodeRecordIds.contains(block.getUid()) ) {
				final String label = StringUtils.trimToNull(block.getFlags().get(FunctionalBlockFlag.HALT_POINT.name()).toString());
				cfg.edges.add(new ControlFlowEdge(block.getUid(), haltPoint.id, label));
			}
		}
	}

	private ControlFlowGraph getFilteredControlFlowGraph(final ControlFlowGraph cfg, final Set<ControlFlowNode> targets,
			final List<ModuleLocation> excludedLocations, final List<ExcludedBranch> excludedBranches) {
		/* collect all conditions that are on a path from a target to the entry point. create and collect the edges for this path */
		final Pair<Set<ControlFlowNode>, Set<ControlFlowEdge>> conditionsAndEdges = collectConditions(cfg, targets);
		final Set<ControlFlowNode> conditions = conditionsAndEdges.getLeft();
		final Set<ControlFlowEdge> controlFlowEdges = conditionsAndEdges.getRight();
		final Set<ControlFlowNode> branchesInTarget = targets.stream().filter(this :: isBranchAndLoopStatement).collect(Collectors.toSet());

		/* remove branches that are marked as excluded from conditions in the graph */
		filterExcludedBranches(branchesInTarget, conditions, controlFlowEdges, excludedBranches);
		/* remove nodes and the edges to them from the graph if they are inside any of the excluded locations (this may break the graph apart) */
		filterExcludedNodes(targets, conditions, controlFlowEdges, excludedLocations);
		/* excluding condition branches and excluding nodes might have created holes in the graph, so remove the paths that are now unreachable */
		filterDisconnectedConditions(conditions, controlFlowEdges);
		/* remove meaningless conditions where all outgoing branches point to the same target in the filtered graph */
		filterMeaninglessConditions(conditions, controlFlowEdges);

		final ControlFlowGraph filteredCfg = new ControlFlowGraph();
		filteredCfg.nodes.addAll(targets);
		filteredCfg.nodes.addAll(conditions);
		filteredCfg.edges.addAll(controlFlowEdges);
		return filteredCfg;
	}

	private Set<ControlFlowNode> collectTargets(final ControlFlowGraph cfg, final List<ModuleLocation> resolvedModuleParts) {
		final Set<ControlFlowNode> targets = new HashSet<>();
		for (final ControlFlowNode controlFlowNode : cfg.nodes) {
			if ( ( matchesReferencedLocation(controlFlowNode, resolvedModuleParts) || isEndNode(controlFlowNode) ) && ! ControlFlowEntity.ANNOTATION.equals(
					controlFlowNode.entity) && ! isBranchAndLoopStatement(controlFlowNode) ) {
				targets.add(controlFlowNode);
			}
		}

		/* remove targets from the list if their parent ControlFlowNode is already a target */
		return targets.stream().filter(target -> targets.stream().noneMatch(p -> Objects.equals(target.parent, p.id))).collect(Collectors.toSet());
	}

	private Pair<Set<ControlFlowNode>, Set<ControlFlowEdge>> collectConditions(final ControlFlowGraph cfg, final Set<ControlFlowNode> targets) {
		final Set<ControlFlowNode> conditions = new HashSet<>();
		final Set<ControlFlowEdge> edges = new HashSet<>();

		for (final ControlFlowNode target : targets) {
			final Set<ControlFlowNode> visited = new HashSet<>();
			collectConditionsRecursively(cfg, target, targets, conditions, edges, visited);
		}
		return Pair.of(conditions, edges);
	}

	private void collectConditionsRecursively(final ControlFlowGraph cfg, final ControlFlowNode current, final Set<ControlFlowNode> targets,
			final Set<ControlFlowNode> conditions, final Set<ControlFlowEdge> collectedEdges, final Set<ControlFlowNode> visited) {

		if ( ! visited.add(current) ) {
			return;
		}

		for (final Pair<ControlFlowNode, ControlFlowEdge> nextNode : findNextConditionOrEndNode(cfg, current, targets, visited)) {
			if ( ! targets.contains(nextNode.getLeft()) ) {
				conditions.add(nextNode.getLeft());
			}
			if ( nextNode.getLeft().id != null && current.id != null ) {
				collectedEdges.add(new ControlFlowEdge(nextNode.getLeft().id, current.id, StringUtils.trimToEmpty(nextNode.getRight().label)));
			}
			collectConditionsRecursively(cfg, nextNode.getLeft(), targets, conditions, collectedEdges, visited);
		}
	}

	private List<Pair<ControlFlowNode, ControlFlowEdge>> findNextConditionOrEndNode(final ControlFlowGraph cfg, final ControlFlowNode target,
			final Set<ControlFlowNode> targets, final Set<ControlFlowNode> visited) {
		final List<Pair<ControlFlowNode, ControlFlowEdge>> ret = new ArrayList<>();
		final List<ControlFlowEdge> edges = cfg.edges.stream().filter(edge -> Objects.equals(target.id, edge.toId)).toList();

		for (final ControlFlowEdge edge : edges) {
			final List<ControlFlowNode> nodes = cfg.nodes.stream().filter(node -> Objects.equals(node.id, edge.fromId)).toList();
			for (final ControlFlowNode node : nodes) {
				if ( targets.contains(node) || isCondition(node) || isEndNode(node) ) {
					ret.add(Pair.of(node, edge));
				} else if ( visited.add(node) ) {
					ret.addAll(findNextConditionOrEndNode(cfg, node, targets, visited));
				}
			}
		}
		return ret;
	}

	private void filterMeaninglessConditions(final Set<ControlFlowNode> conditions, final Set<ControlFlowEdge> edges) {
		boolean changed = true;
		while (changed) {
			changed = false;
			for (final ControlFlowNode condition : conditions) {
				final List<ControlFlowEdge> outgoingEdges = edges.stream().filter(edge -> edge.fromId.equals(condition.id)).toList();
				final Set<UUID> edgeTargets = outgoingEdges.stream().map(edge -> edge.toId).collect(Collectors.toSet());

				if ( edgeTargets.size() <= 1 ) {
					/* all branches of the condition have the same target (or it has no outgoing edges at all) -> the condition is meaningless */
					changed = true;
					final List<ControlFlowEdge> incomingEdges = edges.stream().filter(edge -> edge.toId.equals(condition.id)).toList();

					/* remove the condition and connect all of its incoming edges directly to the single outgoing node */
					conditions.remove(condition);
					incomingEdges.forEach(edges :: remove);
					outgoingEdges.forEach(edges :: remove);

					/* we already checked that edgeTargets contains exactly 1 entry */
					if ( edgeTargets.size() == 1 ) {
						final UUID edgeTarget = edgeTargets.stream().findAny().orElseThrow();

						for (final ControlFlowEdge incomingEdge : incomingEdges) {
							edges.add(new ControlFlowEdge(incomingEdge.fromId, edgeTarget, StringUtils.trimToEmpty(incomingEdge.label)));
						}
					}
					/* repeat the iteration from the start since we updated the collection of conditions */
					break;
				}
			}
		}
	}

	private void filterExcludedBranches(final Set<ControlFlowNode> branchInTargets, final Set<ControlFlowNode> branchInStatements,
			final Set<ControlFlowEdge> edges, final List<ExcludedBranch> excludedBranches) {
		Set<ControlFlowNode> branchStatements = new HashSet<>();
		branchStatements.addAll(branchInTargets);
		branchStatements.addAll(branchInStatements);
		for (final ControlFlowNode branchStmt : branchStatements) {
			final Set<String> excludedBranchesForStatement = getExcludedBranchesForStatement(branchStmt, excludedBranches);
			if ( ! excludedBranchesForStatement.isEmpty() ) {
				final List<ControlFlowEdge> excludedEdges = edges.stream().filter(edge -> edge.fromId.equals(branchStmt.id))
						.filter(edge -> excludedBranchesForStatement.stream().anyMatch(excluded -> excluded.equals(edge.label))).toList();
				excludedEdges.forEach(edges :: remove);
			}
		}
	}

	private Set<String> getExcludedBranchesForStatement(final ControlFlowNode branchStmt, final List<ExcludedBranch> excludedBranches) {
		final Integer offset = branchStmt.offset;
		final Integer length = branchStmt.length;
		if ( offset == null || length == null ) {
			return Collections.emptySet();
		}

		final ModuleLocation branchStatementLocation = new ModuleLocation(offset, length);
		return excludedBranches.stream().filter(excluded -> excluded.getLocation().overlapsWith(branchStatementLocation)).map(ExcludedBranch :: getBranch)
				.collect(Collectors.toSet());
	}

	private void filterDisconnectedConditions(final Set<ControlFlowNode> conditions, final Set<ControlFlowEdge> edges) {
		boolean changed = true;
		while (changed) {
			changed = false;
			for (final ControlFlowNode condition : conditions) {
				final boolean hasIncoming = edges.stream().anyMatch(edge -> edge.toId.equals(condition.id));
				if ( ! hasIncoming ) {
					changed = true;
					conditions.remove(condition);
					final List<ControlFlowEdge> outgoingEdges = edges.stream().filter(edge -> edge.fromId.equals(condition.id)).toList();
					outgoingEdges.forEach(edges :: remove);
					break;
				}
			}
		}
	}

	private void filterExcludedNodes(final Set<ControlFlowNode> targets, final Set<ControlFlowNode> conditions, final Set<ControlFlowEdge> edges,
			final Collection<ModuleLocation> excludedLocations) {

		final List<ControlFlowNode> excludedNodes = Stream.concat(targets.stream(), conditions.stream()).filter(target -> {
			final Integer offset = target.offset;
			final Integer length = target.length;
			if ( offset == null || length == null ) {
				/* artificial node, like entry point - these are never excluded */
				return false;
			}

			final ModuleLocation targetLocation = new ModuleLocation(offset, length);
			return excludedLocations.stream().anyMatch(targetLocation :: isWithin);
		}).toList();

		final List<ControlFlowEdge> removedEdges = edges.stream().filter(edge -> excludedNodes.stream()
				.anyMatch(excludedNode -> Objects.equals(edge.fromId, excludedNode.id) || Objects.equals(edge.toId, excludedNode.id))).toList();

		final List<ControlFlowEdge> edgesFromNonExcludedToExcluded = edges.stream()
				.filter(edge -> excludedNodes.stream().anyMatch(node -> Objects.equals(node.id, edge.toId)) && excludedNodes.stream()
						.noneMatch(node -> Objects.equals(node.id, edge.fromId))).toList();

		excludedNodes.forEach(targets :: remove);
		excludedNodes.forEach(conditions :: remove);
		removedEdges.forEach(edges :: remove);

		if ( ! edgesFromNonExcludedToExcluded.isEmpty() ) {
			/* introduce artificial "HaltPoint" that represents the excluded code area */
			final ControlFlowNode haltPoint = new ControlFlowNode(UUID.randomUUID(), targets.stream().findAny().orElseThrow().module,
					ControlFlowEntity.TERMINAL);
			haltPoint.type = AstModuleRelationshipType.HALT.name();
			haltPoint.label = EXCLUDED_CODE_LABEL;
			targets.add(haltPoint);
			/* replace any former edges from non-excluded nodes to excluded nodes with an edge to the halt point */
			for (final ControlFlowEdge edge : edgesFromNonExcludedToExcluded) {
				edges.add(new ControlFlowEdge(edge.fromId, haltPoint.id, edge.label));
			}
		}
	}

	private boolean matchesReferencedLocation(final ControlFlowNode controlFlowNode, final List<ModuleLocation> referencedLocations) {
		final Integer offset = controlFlowNode.offset;
		final Integer length = controlFlowNode.length;
		if ( offset == null || length == null ) {
			return false;
		}
		return referencedLocations.stream().anyMatch(loc -> loc.overlapsWith(new ModuleLocation(offset, length)));
	}

	private boolean isCondition(final ControlFlowNode controlFlowNode) {
		final Set<String> superTypes = controlFlowNode.superTypes;
		return superTypes.contains(AstNodeUtils.BRANCH_STATEMENT) || superTypes.contains(AstNodeUtils.LOOP_STATEMENT);
	}

	private static boolean isEndNode(final ControlFlowNode controlFlowNode) {
		return controlFlowNode.entity == ControlFlowEntity.TERMINAL && END_NODE_TYPES.contains(controlFlowNode.type);
	}

	private boolean isBranchAndLoopStatement(final ControlFlowNode controlFlowNode) {
		final Set<String> superTypes = controlFlowNode.superTypes;
		return superTypes.contains(AstNodeUtils.BRANCH_STATEMENT) || superTypes.contains(AstNodeUtils.LOOP_STATEMENT) || superTypes.contains(DEFAULT_BRANCH)
				|| superTypes.contains(BRANCH);
	}
}
