/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import static innowake.mining.data.core.SchemaConstants.SYSTEM_USER;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.shared.entities.functionalblocks.ExcludedBranch;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.server.functionalblocks.generation.datalineagefunctionalblock.DataLineageFunctionalBlockGeneration;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockWithChildrenAndLinks;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.controlflow.ControlFlowEdge;
import innowake.mining.shared.model.controlflow.ControlFlowEntity;
import innowake.mining.shared.model.controlflow.ControlFlowGraph;
import innowake.mining.shared.model.controlflow.ControlFlowNode;

/**
 * Service for generating Control Flow Graphs by creating statement and links for a Functional Block with Type Functional Group.
 * <p>
 * The service generates conditions and links for statements and annotations and adds them as children to the provided functional
 * block. The service first retrieves the control flow graph for the module and filters it for the provided statement locations or annotations. It then
 * retrieves the annotations for the statements and maps them to functional blocks. The service then generates Statement and condition blocks for the module and
 * adds them as children to the provided functional block. Finally, the service generates links between all the blocks and returns the functional block with
 * children and links.
 * </p>
 */
@Service
public class FunctionalBlockControlFlowGraphGenerationService {

	private static final Logger LOG = LoggerFactory.getLogger(FunctionalBlockControlFlowGraphGenerationService.class);

	@Autowired
	private AnnotationService annotationDao;
	@Autowired
	private AstService astService;
	@Autowired
	private AnnotationToFunctionalBlockService annotationToFunctionalBlockService;
	@Autowired
	private FunctionalBlockToControlFlowGraphService controlFlowGraphService;
	@Autowired
	private FunctionalBlockService functionalBlockService;
	@Autowired
	private ExecutorService executorService;
	@Autowired
	private ObjectMapper objectMapper;

	/**
	 * Generates conditions and links for statements for provided statement location of a module and add as child to functional block.
	 * <p>
	 * The method generates conditions and links for the provided statement locations of a module and adds them as children to the provided
	 * functional block. The method first retrieves the control flow graph for the module and filters it for the provided statement locations.
	 * It then retrieves the annotations for the statements and maps them to functional blocks. The method then generates Statement and condition blocks
	 * for the module and adds them as children to the provided functional block. Finally, the method generates links between the statement blocks
	 * and the Statement and condition blocks and returns the functional block with children and links.
	 * </p>
	 *
	 * @param projectId the project ID
	 * @param moduleId the module ID
	 * @param moduleHashlink module hash
	 * @param functionalBlock the functional block
	 * @param statementLocations the statement locations
	 * @param excludedBranches the excluded branches
	 *
	 * @return FunctionalBlockWithChildrenAndLinks the functional block with children and links
	 */
	public FunctionalBlockWithChildrenAndLinks generateConditionsAndLinksForStatements(final EntityId projectId, final EntityId moduleId,
			final String moduleHashlink, final FunctionalBlockPojoPrototype functionalBlock, final List<ModuleLocation> statementLocations,
			final List<ExcludedBranch> excludedBranches) {
		LOG.info("Generating conditions and links for statements for module {} in project {}", moduleId, projectId);

		LOG.debug("Getting control flow graph for module {} in project {}", moduleId, projectId);
		final ControlFlowGraph cfg = astService.getControlFlow(moduleId, null);
		final List<ModuleLocation> excludedLocations = getExcludedLocations(moduleId);
		LOG.debug("Filtering control flow graph for module {} in project {}", moduleId, projectId);
		final ControlFlowGraph filteredCfg = controlFlowGraphService.filterControlFlowGraph(cfg, statementLocations, excludedLocations, excludedBranches);

		LOG.debug("Getting statement blocks for module {} in project {}", moduleId, projectId);
		final Map<UUID, AnnotationPojo> annotations = getAnnotationsForStatements(moduleId, filteredCfg);
		final List<FunctionalBlockPojo> functionalUnitBlocks = getFunctionalUnitBlocks(projectId, annotations);
		return generateBlocksAndLinksForControlFlowGraph(projectId, moduleId, moduleHashlink, functionalBlock, filteredCfg, functionalUnitBlocks, true);
	}

	/**
	 * Generates conditions and links for annotations for provided annotation list of a module and add as child to functional block.
	 * <p>
	 * The method generates conditions and links for the provided annotations of a module and adds them as children to the provided functional block.
	 * The method first retrieves the control flow graph for the module and filters it for the provided annotations. The method then generates statement and
	 * condition blocks for the module and adds them as children. Finally, the method generates links between the statement blocks and the Statement and
	 * condition blocks and returns the functional block with children and links.
	 *</p>
	 *
	 * @param projectId the project ID
	 * @param moduleId the module ID
	 * @param functionalBlock the functional block
	 * @param functionalUnitBlocks the functional unit blocks
	 * @param excludedBranches the excluded branches
	 * @return FunctionalBlockWithChildrenAndLinks the functional block with children and links
	 */
	public FunctionalBlockWithChildrenAndLinks generateConditionsAndLinksForAnnotations(final EntityId projectId, final EntityId moduleId,
			final FunctionalBlockPojoPrototype functionalBlock, final List<FunctionalBlockPojo> functionalUnitBlocks,
			final List<ExcludedBranch> excludedBranches) {

		final String moduleLinkHash = functionalUnitBlocks.stream().flatMap(fu -> fu.getModuleParts().stream()).map(ModulePart :: getModuleLinkHash).findAny()
				.orElseThrow(() -> new IllegalArgumentException("the provided block contains no Annotations"));

		LOG.info("Generating conditions and links for annotations for module {} in project {}", moduleId, projectId);
		ControlFlowGraph cfg = astService.getControlFlow(moduleId, null);

		if (cfg.nodes.isEmpty() || cfg.edges.isEmpty()) {
			executorService.executeControlFlowCalculation(moduleId);
			cfg = astService.getControlFlow(moduleId, null);
		}

		final List<ModuleLocation> parts = functionalUnitBlocks.stream().flatMap(fu -> fu.getModuleParts().stream()).map(ModulePart :: getLocation)
				.flatMap(Optional :: stream).toList();

		final List<ModuleLocation> excludedLocations = getExcludedLocations(moduleId);
		LOG.debug("Filtering control flow graph for module {} in project {}", moduleId, projectId);
		final ControlFlowGraph filteredCfg = controlFlowGraphService.filterControlFlowGraph(cfg, parts, excludedLocations, excludedBranches);

		LOG.debug("Getting Statement and Condition blocks for module {} in project {}", moduleId, projectId);
		return generateBlocksAndLinksForControlFlowGraph(projectId, moduleId, moduleLinkHash, functionalBlock, filteredCfg, functionalUnitBlocks, false);
	}

	private FunctionalBlockWithChildrenAndLinks generateBlocksAndLinksForControlFlowGraph(final EntityId projectId, final EntityId moduleId,
			final String moduleLinkHash, final FunctionalBlockPojoPrototype functionalBlock, final ControlFlowGraph filteredCfg,
			final Collection<FunctionalBlockPojo> functionalUnitBlocks, final boolean isNewBlock) {
		LOG.debug("Getting Statement and Condition blocks for module {} in project {}", moduleId, projectId);
		final Map<UUID, FunctionalBlockPojoPrototype> cfgBlocks = new HashMap<>();
		cfgBlocks.putAll(makeFunctionalStatementBlocks(projectId, filteredCfg, moduleLinkHash));
		cfgBlocks.putAll(makeFunctionalConditionBlocks(projectId, moduleLinkHash, filteredCfg));
		LOG.debug("Making functional block for module {} in project {}", moduleId, projectId);
		makeFunctionalBlock(functionalUnitBlocks, cfgBlocks.values(), functionalBlock, isNewBlock);
		final List<FunctionalBlockLink> links = makeLinks(cfgBlocks, filteredCfg);

		/* uid is not defined when creating a new block - in this case the new block also does not contain subgroups yet, so we can skip this step */
		if ( functionalBlock.uid.isDefined() ) {
			assignGroups(functionalBlock.uid.getNonNull(), functionalUnitBlocks, cfgBlocks.values(), links);
		}

		LOG.info("Generated conditions and links for annotations for module {} in project {}", moduleId, projectId);
		return new FunctionalBlockWithChildrenAndLinks(functionalBlock, new ArrayList<>(cfgBlocks.values()), links);
	}

	private Map<UUID, AnnotationPojo> getAnnotationsForStatements(final EntityId moduleId, final ControlFlowGraph controlFlowGraph) {
		final var statementNodes = controlFlowGraph.nodes.stream().filter(node -> ! isBranchStatement(node)).toList();

		final List<AnnotationPojo> existingAnnotations = annotationDao.find(q -> q.ofModule(moduleId).withType(AnnotationType.FUNCTIONAL));
		final Map<UUID, AnnotationPojo> createdAnnotations = new HashMap<>(statementNodes.size());
		for (final var statement : statementNodes) {
			final Integer offset = statement.offset;
			final Integer length = statement.length;
			if ( offset == null || length == null || statement.label == null ) {
				continue;
			}
			final ModuleLocation statementLocation = new ModuleLocation(offset, length);
			final Optional<AnnotationPojo> existingAnnotation = existingAnnotations.stream()
					.filter(annotation -> ( annotation.getLocation().isPresent() && annotation.getLocation().get().overlapsWith(statementLocation) )).findAny();
			if ( existingAnnotation.isPresent() ) {
				createdAnnotations.put(statement.id, existingAnnotation.get());
			} else {
				final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype().setState(WorkingState.CANDIDATE).setType(AnnotationType.FUNCTIONAL)
						.setName(Objects.requireNonNull(statement.label)).setModule(moduleId).setLocation(statementLocation).setCreatedByUserId(SYSTEM_USER);
				final EntityId annotationId = annotationDao.create(annotation);
				createdAnnotations.put(statement.id, annotationDao.get(annotationId));
			}
		}
		return createdAnnotations;
	}

	private List<FunctionalBlockPojo> getFunctionalUnitBlocks(final EntityId projectId, final Map<UUID, AnnotationPojo> annotations) {
		final Map<Long, UUID> annotationToFunctionalUnit = annotationToFunctionalBlockService.getFunctionalUnitsForAnnotations(projectId,
				annotations.values().stream().map(AnnotationPojo :: getId).map(EntityId :: of).toList());

		return functionalBlockService.get(new ArrayList<>(annotationToFunctionalUnit.values()));
	}

	private List<ModuleLocation> getExcludedLocations(final EntityId moduleId) {
		return annotationDao.find(q -> q.ofModule(moduleId).withType(AnnotationType.EXCLUDE)).stream().map(AnnotationPojo :: getLocation)
				.filter(Optional :: isPresent).map(Optional :: get).toList();
	}

	private Map<UUID, FunctionalBlockPojoPrototype> makeFunctionalStatementBlocks(final EntityId projectId, final ControlFlowGraph controlFlowGraph,
			final String moduleLinkHash) {
		final Map<UUID, FunctionalBlockPojoPrototype> ret = new HashMap<>();
		final var astNodes = controlFlowGraph.nodes.stream().filter(node -> ! isBranchStatement(node) && node.entity != ControlFlowEntity.ANNOTATION)
				.filter(node -> node.offset != null && node.length != null).toList();

		for (final var astNode : astNodes) {
			final Map<String, Object> flags = new HashMap<>();
			flags.put(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_STATEMENT.name()));
			flags.put(FunctionalBlockFlag.READ_ONLY.name(), Boolean.TRUE);
			flags.put(FunctionalBlockFlag.GENERATED_BY.name(), DataLineageFunctionalBlockGeneration.class.getSimpleName());
			flags.put(FunctionalBlockFlag.GENERATED_AT.name(), Instant.now().toEpochMilli());
			if ( isEntryPoint(controlFlowGraph, astNode) ) {
				flags.put(FunctionalBlockFlag.ENTRY_POINT.name(), Boolean.TRUE);
			}
			final Optional<String> haltPointAndLabel = getHaltPointAndLabel(controlFlowGraph, astNode);
			haltPointAndLabel.ifPresent(s -> flags.put(FunctionalBlockFlag.HALT_POINT.name(), s));

			final ModulePart moduleParts = new ModulePart(moduleLinkHash, new ModuleLocation(astNode.offset, astNode.length));
			final FunctionalBlockPojoPrototype block = new FunctionalBlockPojoPrototype().setUid(UUID.randomUUID()).setProject(projectId)
					.setName(astNode.label != null ? astNode.label : "").setDescription("").setFlags(flags).setModuleParts(List.of(moduleParts));

			ret.put(astNode.id, block);
		}
		return ret;
	}

	private Map<UUID, FunctionalBlockPojoPrototype> makeFunctionalConditionBlocks(final EntityId projectId, final String moduleLinkHash,
			final ControlFlowGraph filteredCfg) {
		final var branchStatements = filteredCfg.nodes.stream().filter(this :: isBranchStatement).toList();
		final Map<UUID, FunctionalBlockPojoPrototype> ret = new HashMap<>();
		for (final var branch : branchStatements) {
			final List<ControlFlowNode> conditions = extractConditions(branch);
			final List<ModulePart> moduleParts = conditions.stream().filter(cond -> cond.offset != null && cond.length != null)
					.map(cond -> new ModulePart(moduleLinkHash, new ModuleLocation(Objects.requireNonNull(cond.offset), Objects.requireNonNull(cond.length))))
					.toList();

			final Map<String, Object> flags = new HashMap<>();
			flags.put(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_CONDITION.name()));
			flags.put(FunctionalBlockFlag.READ_ONLY.name(), Boolean.TRUE);
			flags.put(FunctionalBlockFlag.GENERATED_BY.name(), DataLineageFunctionalBlockGeneration.class.getSimpleName());
			flags.put(FunctionalBlockFlag.GENERATED_AT.name(), Instant.now().toEpochMilli());
			if ( isEntryPoint(filteredCfg, branch) ) {
				flags.put(FunctionalBlockFlag.ENTRY_POINT.name(), Boolean.TRUE);
			}
			final Optional<String> haltPointAndLabel = getHaltPointAndLabel(filteredCfg, branch);
			haltPointAndLabel.ifPresent(s -> flags.put(FunctionalBlockFlag.HALT_POINT.name(), s));

			final FunctionalBlockPojoPrototype block = new FunctionalBlockPojoPrototype().setUid(UUID.randomUUID()).setProject(projectId)
					.setName(conditions.stream().map(n -> n.label).collect(Collectors.joining(" "))).setDescription("").setFlags(flags)
					.setModuleParts(moduleParts);

			ret.put(branch.id, block);
		}

		return ret;
	}

	private void makeFunctionalBlock(final Collection<FunctionalBlockPojo> functionalUnitBlocks, final Collection<FunctionalBlockPojoPrototype> cfgBlocks,
			final FunctionalBlockPojoPrototype functionalBlock, final boolean isNewBlock) {
		final List<FunctionalBlockPojo> existingOrNewChildren;
		if ( isNewBlock ) {
			existingOrNewChildren = sortedFunctionalUnitBlocks(functionalUnitBlocks);
		} else {
			/* retain all children that are not FUNCTIONAL_STATEMENT or FUNCTIONAL_CONDITION (as these are deleted and re-created by this computation) */
			final FunctionalBlockPojo existingBlock = functionalBlockService.find(functionalBlock.uid.getNonNull())
					.orElseThrow(() -> new IllegalStateException("block must exist since computation was executed on it"));
			final List<FunctionalBlockPojo> existingChildren = functionalBlockService.get(existingBlock.getChildren());
			existingOrNewChildren = existingChildren.stream()
					.filter(child -> ! FunctionalBlockUtil.hasType(child, FunctionalBlockType.FUNCTIONAL_CONDITION) && ! FunctionalBlockUtil.hasType(child,
							FunctionalBlockType.FUNCTIONAL_STATEMENT)).toList();
		}

		final List<UUID> children = new ArrayList<>(functionalUnitBlocks.size() + cfgBlocks.size());

		for (final FunctionalBlockPojo functionalUnitBlock : existingOrNewChildren) {
			children.add(functionalUnitBlock.getUid());
		}
		for (final FunctionalBlockPojoPrototype conditionBlock : cfgBlocks) {
			children.add(conditionBlock.uid.getNonNull());
		}
		functionalBlock.children.set(children);
	}

	private List<FunctionalBlockLink> makeLinks(final Map<UUID, FunctionalBlockPojoPrototype> blocks, final ControlFlowGraph cfg) {

		final Map<UUID, UUID> astNodeToBlock = new HashMap<>();
		for (final Map.Entry<UUID, FunctionalBlockPojoPrototype> entry : blocks.entrySet()) {
			astNodeToBlock.put(entry.getKey(), entry.getValue().uid.getNonNull());
		}

		final List<FunctionalBlockLink> links = new ArrayList<>();
		for (final ControlFlowEdge edge : cfg.edges) {
			final UUID fromBlock = astNodeToBlock.get(edge.fromId);
			final UUID toBlock = astNodeToBlock.get(edge.toId);

			if ( fromBlock != null && toBlock != null && ! fromBlock.equals(toBlock) ) {
				links.add(new FunctionalBlockLink(null, null, fromBlock, toBlock, edge.label, Collections.emptyMap(), null));
			}
		}

		return links;
	}

	private boolean isEntryPoint(final ControlFlowGraph cfg, final ControlFlowNode node) {
		final List<ControlFlowEdge> incomingEdges = cfg.edges.stream().filter(edge -> Objects.equals(edge.toId, node.id)).toList();

		final List<ControlFlowNode> incomingNodes = cfg.nodes.stream().filter(n -> incomingEdges.stream().anyMatch(edge -> Objects.equals(edge.fromId, n.id)))
				.toList();

		return incomingNodes.stream().anyMatch(n -> ControlFlowEntity.TERMINAL == n.entity && AstModuleRelationshipType.ENTRY.toString().equals(n.type));
	}

	private boolean isBranchStatement(final ControlFlowNode astNode) {
		final Set<String> superTypes = astNode.superTypes;
		return superTypes.contains(AstNodeUtils.BRANCH_STATEMENT) || superTypes.contains(AstNodeUtils.LOOP_STATEMENT);
	}

	private Optional<String> getHaltPointAndLabel(final ControlFlowGraph cfg, final ControlFlowNode node) {
		final List<ControlFlowEdge> outgoingEdges = cfg.edges.stream().filter(edge -> Objects.equals(edge.fromId, node.id)).toList();

		final List<ControlFlowNode> outgoingNodes = cfg.nodes.stream().filter(n -> outgoingEdges.stream().anyMatch(edge -> Objects.equals(edge.toId, n.id)))
				.toList();

		return outgoingNodes.stream().filter(n -> AstModuleRelationshipType.HALT.name().equals(n.type)).findAny()
				.flatMap(n -> outgoingEdges.stream().filter(e -> Objects.equals(e.toId, n.id)).findAny()).map(e -> StringUtils.trimToEmpty(e.label));
	}

	private static List<FunctionalBlockPojo> sortedFunctionalUnitBlocks(final Collection<FunctionalBlockPojo> functionalUnitBlocks) {
		return functionalUnitBlocks.stream().sorted(Comparator.comparing(
				block -> block.getModuleParts().stream().findFirst().flatMap(ModulePart :: getLocation).map(ModuleLocation :: getOffset).orElse(0))).toList();
	}

	/**
	 * Attempts to extract the actual conditions from a branch statement. Returns a list of the contained conditions or a list containing only
	 * the original branch statement itself, if the extraction is unsuccessful.
	 * @param astNode a branch statement inside the control flow graph
	 * @return list of conditions contained in the branch statement or a list containing only the branch statement itself
	 */
	private List<ControlFlowNode> extractConditions(final ControlFlowNode astNode) {
		final var prop = astNode.properties.get(AstNodeUtils.PROPERTY_CONDITION_PATHS);

		if ( prop == null ) {
			return List.of(astNode);
		}

		final int[][] conditionPaths;
		try {
			conditionPaths = objectMapper.readValue((String) prop, int[][].class);
		} catch (final JsonProcessingException e) {
			return List.of(astNode);
		}

		final List<ControlFlowNode> conditions = new ArrayList<>(conditionPaths.length);
		for (final int[] conditionPath : conditionPaths) {
			ControlFlowNode current = astNode;
			for (final int child : conditionPath) {
				final var children = astService.get(current.id).getChildIds();
				if ( children.size() <= child ) {
					return List.of(astNode);
				}

				final var childNode = astService.get(children.get(child));
				current = new ControlFlowNode(childNode.getId(), childNode.getModuleUid(), ControlFlowEntity.AST_NODE);
				current.type = childNode.getType();
				current.label = childNode.getLabel();
				current.offset = childNode.getLocation().getRetracedOffset().orElse(null);
				current.length = childNode.getLocation().getRetracedLength().orElse(null);
				current.parent = childNode.getParentId().orElse(null);
				current.superTypes = childNode.getSuperTypes();
				current.properties.putAll(childNode.getProperties());
			}
			conditions.add(current);
		}
		return conditions;
	}

	/**
	 * Assign the {@code cfgBlocks} to subgroups depending on the actual FUNCTIONAL_GROUP the corresponding FUNCTIONAL_UNIT is in.
	 * <p>
	 * If the FUNCTIONAL_UNIT is directly contained in the block denoted by {@code functionalBlockId} then no subgroup is assigned. Otherwise, the id
	 * of the parent of the FUNCTIONAL_UNIT is assigned to each corresponding FUNCTIONAL_STATEMENT as subgroup by setting the
	 * {@link FunctionalBlockFlag#FUNCTIONAL_GROUP} flag.
	 * <p>
	 * FUNCTIONAL_CONDITION blocks will not usually match any FUNCTIONAL_UNITS - therefore all preceding conditions of a FUNCTIONAL_STATEMENT
	 * are assigned to the same subgroup as the FUNCTIONAL_STATEMENT, until reaching the next FUNCTIONAL_STATEMENT that is in a different subgroup.
	 *
	 * @param functionalUnitBlocks the functional unit blocks representing the annotations from which the cfg was built
	 * @param cfgBlocks the blocks representing the nodes of the cfg
	 * @param links the links that link the cfg blocks to form a graph
	 */
	private void assignGroups(final UUID functionalBlockId, final Collection<FunctionalBlockPojo> functionalUnitBlocks,
			final Collection<FunctionalBlockPojoPrototype> cfgBlocks, final Collection<FunctionalBlockLink> links) {

		/* assign subgroup to cfgBlock if its location overlaps with a FUNCTIONAL_UNIT block that is in a subgroup */
		final Map<UUID, UUID> subgroupMap = mapFunctionalUnitToSubgroup(functionalBlockId, functionalUnitBlocks);
		for (final FunctionalBlockPojoPrototype cfgBlock : cfgBlocks) {
			final Optional<FunctionalBlockPojo> functionalUnit = functionalUnitBlocks.stream().filter(fu -> fu.getModuleParts().stream().anyMatch(
							fuPart -> fuPart.getLocation().map(fuLocation -> cfgBlock.moduleParts.getNonNull().stream()
									.anyMatch(cfgPart -> cfgPart.getLocation().map(cfgLocation -> cfgLocation.overlapsWith(fuLocation)).orElse(false))).orElse(false)))
					.findAny();

			if ( functionalUnit.isPresent() && subgroupMap.containsKey(functionalUnit.get().getUid()) ) {
				final Map<String, Object> flags = cfgBlock.flags.getNonNull();
				final Optional<String> existingGroup = FunctionalBlockUtil.getStringFlag(flags, FunctionalBlockFlag.FUNCTIONAL_GROUP);
				if ( existingGroup.isEmpty() ) {
					flags.put(FunctionalBlockFlag.FUNCTIONAL_GROUP.name(), subgroupMap.get(functionalUnit.get().getUid()));
				}
			}
		}

		/* "propagate" subgroups along the links until reaching a block that is already in a different subgroup */
		for (final FunctionalBlockPojoPrototype cfgBlock : cfgBlocks) {
			final Optional<String> subgroup = FunctionalBlockUtil.getStringFlag(cfgBlock.flags.getNonNull(), FunctionalBlockFlag.FUNCTIONAL_GROUP);
			if ( subgroup.isEmpty() ) {
				/* this cfgBlock is not in a subgroup -> nothing to propagate */
				continue;
			}
			propagate(cfgBlocks, cfgBlock, subgroup.get(), links);
		}
	}

	private void propagate(final Collection<FunctionalBlockPojoPrototype> cfgBlocks, final FunctionalBlockPojoPrototype currentBlock, final String subgroup,
			final Collection<FunctionalBlockLink> links) {
		final List<FunctionalBlockLink> incomingLinks = links.stream().filter(l -> l.getChildB().equals(currentBlock.uid.get())).toList();
		final List<FunctionalBlockPojoPrototype> incomingBlocks = cfgBlocks.stream()
				.filter(block -> incomingLinks.stream().anyMatch(link -> link.getChildA().equals(block.uid.get()))).toList();

		for (final FunctionalBlockPojoPrototype incomingBlock : incomingBlocks) {
			final Map<String, Object> flags = incomingBlock.flags.getNonNull();
			final Optional<String> existingGroup = FunctionalBlockUtil.getStringFlag(flags, FunctionalBlockFlag.FUNCTIONAL_GROUP);
			if ( existingGroup.isEmpty() ) {
				flags.put(FunctionalBlockFlag.FUNCTIONAL_GROUP.name(), subgroup);
				propagate(cfgBlocks, incomingBlock, subgroup, links);
			}
		}
	}

	private Map<UUID, UUID> mapFunctionalUnitToSubgroup(final UUID rootBlockId, final Collection<FunctionalBlockPojo> functionalUnitBlocks) {
		final List<FunctionalBlockPojo> subgroups = functionalBlockService.findChildrenDeep(rootBlockId, - 1,
				q -> q.withType(FunctionalBlockType.FUNCTIONAL_GROUP));

		final Map<UUID, UUID> ret = new HashMap<>();
		for (final FunctionalBlockPojo functionalUnitBlock : functionalUnitBlocks) {
			if ( ! functionalUnitBlock.getParents().contains(rootBlockId) ) {
				Optional<FunctionalBlockPojo> subgroup = subgroups.stream().filter(sb -> functionalUnitBlock.getParents().contains(sb.getUid()))
						.findAny();
				Optional<FunctionalBlockPojo> previousSubGroup = Optional.empty();
				while( subgroup.isPresent()) {
					FunctionalBlockPojo finalSubgroup = subgroup.get();
					previousSubGroup = subgroup;
					subgroup = subgroups.stream().filter(sb -> finalSubgroup.getParents().contains(sb.getUid())).findAny();
				}
				previousSubGroup.ifPresentOrElse(
						psg -> ret.put(functionalUnitBlock.getUid(), psg.getUid()),
						() -> ret.put(functionalUnitBlock.getUid(), rootBlockId)
				);
			} else {
				ret.put(functionalUnitBlock.getUid(), rootBlockId);
			}
		}
		return ret;
	}
}
