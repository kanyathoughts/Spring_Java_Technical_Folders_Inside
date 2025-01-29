/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.generation.reachabilityanalysis;

import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.extensions.export.callchain.CallChainExporterJob;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.extensions.export.callchain.Parameters;
import innowake.mining.extensions.export.callchain.model.CallChain;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGeneration;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult.Operation;
import innowake.mining.server.functionalblocks.generation.ModuleBlockGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.time.Instant;
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
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
public class ReachabilityTopDownGeneration implements FunctionalBlockGeneration<EntityId, List<FunctionalBlockLink>> {

	public static final String RA_TOP_DOWN_GENERATION_ID = "ReachabilityTopDownGeneration";

	private FunctionalBlockGenerationService functionalBlockGenerationService;
	private final FunctionalBlockService functionalBlockService;
	private final CallChainService callChainService;

	public ReachabilityTopDownGeneration(final FunctionalBlockService functionalBlockService,
										 final CallChainService callChainService) {
		this.functionalBlockService = functionalBlockService;
		this.callChainService = callChainService;
	}

	@Override
	public void setFunctionalBlockGenerationService(final FunctionalBlockGenerationService service) {
		this.functionalBlockGenerationService = service;
	}

	@Override
	public Collection<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> generate(final FunctionalBlockGenerationContext context,
																	  final EntityId topModuleId) {
		if (topModuleId == null) {
			return Collections.emptyList();
		}

		final Optional<CallChainGraph> optionalCallChainGraph = calculateCallChainGraph(context, topModuleId);
		if (optionalCallChainGraph.isEmpty()) {
			// since we don't have a callchain to gather information from, a functional block for the ra can't be created.
			return Collections.emptyList();
		}

		final GraphExtractionResult graphExtractionResult = extractRequiredInformationFromGraph(context, optionalCallChainGraph.get());

		return generateFunctionalBlocks(context, graphExtractionResult);
	}


	private List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> generateFunctionalBlocks(final FunctionalBlockGenerationContext context,
																				 final GraphExtractionResult graphExtractionResult) {
		final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> blocks = new ArrayList<>();
		final List<UUID> children = new ArrayList<>();

		final Map<String, UUID> moduleBlocks = getModuleBlocks(context, graphExtractionResult.allModules);

		final Optional<UUID> reachabilityBlockUidOptional =
				handleExistingReachabilityBlock(graphExtractionResult.startModule, blocks, context);
		UUID reachabilityBlockUid = UUID.randomUUID();
		FunctionalBlockGenerationResult.Operation operation = Operation.CREATE;
		if (reachabilityBlockUidOptional.isPresent()) {
			reachabilityBlockUid = reachabilityBlockUidOptional.get();
			operation = Operation.UPDATE;
		}

	    generateUpperBoundBlock(context.getProjectId(), graphExtractionResult.startModule, moduleBlocks, children, blocks);

		final Map<ModuleLightweightPojo, Collection<ModuleLightweightPojo>> endModuleToAccessModules = graphExtractionResult.endModules.stream()
				.collect(Collectors.toMap(Function.identity(), module -> graphExtractionResult.callChainGraph.getTargetMap().get(module)));

		final Map<ModuleLightweightPojo, FunctionalBlockPojoPrototype> accessModuleToAccessModuleBlock = generateAccessModuleBlocks(
				context.getProjectId(),  moduleBlocks, children,endModuleToAccessModules, blocks);

		final Map<ModuleLightweightPojo, FunctionalBlockPojoPrototype> endModuleToLowerBoundBlock = generateLowerBoundBlocks(context.getProjectId(),
				graphExtractionResult, moduleBlocks, children, blocks);

		generateCallChainBlock(context.getProjectId(), graphExtractionResult, moduleBlocks, children, blocks);

		final FunctionalBlockPojoPrototype reachabilityBlock = generateReachabilityBlock(context.getProjectId(), graphExtractionResult.startModule,
				children, blocks, operation, reachabilityBlockUid);

		generateLinkBetweenLowerBoundAndAccessModules(endModuleToAccessModules, graphExtractionResult,
				accessModuleToAccessModuleBlock, endModuleToLowerBoundBlock, reachabilityBlock,  blocks);


		return blocks;
	}

	private void  generateLinkBetweenLowerBoundAndAccessModules(final Map<ModuleLightweightPojo, Collection<ModuleLightweightPojo>> endModuleToAccessModules,
															   final GraphExtractionResult graphExtractionResult,
			                                                   final Map<ModuleLightweightPojo, FunctionalBlockPojoPrototype> accessModuleToAccessModuleBlock,
			                                                   final Map<ModuleLightweightPojo, FunctionalBlockPojoPrototype> endModuleToLowerBoundBlock,
			                                                   final FunctionalBlockPojoPrototype reachabilityBlock,
			                                                   final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> blocksToGenerate) {

		final List<FunctionalBlockLink> accessModuleLinks = new ArrayList<>();
		for (final Map.Entry<ModuleLightweightPojo, Collection<ModuleLightweightPojo>> endModuleToAccessModuleEntry : endModuleToAccessModules.entrySet()) {
			final ModuleLightweightPojo lowerBoundModule = endModuleToAccessModuleEntry.getKey();
			final FunctionalBlockPojoPrototype lowerBoundBlock = endModuleToLowerBoundBlock.get(lowerBoundModule);
			for (final var accessModule : endModuleToAccessModuleEntry.getValue()) {
				final var accessTypes = graphExtractionResult.callChainGraph.getEdgeMap().get(accessModule).parallelStream()
						.filter(edge -> edge.getTarget().equals(lowerBoundModule))
						.map(this::extractAccessTypes)
						.flatMap(Collection::parallelStream)
						.collect(Collectors.toSet());
				final var accessModuleBlock = accessModuleToAccessModuleBlock.get(accessModule);
				accessModuleLinks.add(new FunctionalBlockLink(UUID.randomUUID(), reachabilityBlock.uid.get(),
						accessModuleBlock.uid.getNonNull(), lowerBoundBlock.uid.getNonNull(), null,
						Map.of(FunctionalBlockLinkFlag.TYPE.name(), FunctionalBlockLinkType.RA_ACCESS.name(),
								FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name(), accessTypes), null));
			}
		}
		blocksToGenerate.add(new FunctionalBlockGenerationResult<>(Operation.UPDATE, reachabilityBlock, accessModuleLinks));

	}

	/**
	 * Calculates the call chain graph starting from the module walking downwards.
	 *
	 * @param context The context to get the project id from.
	 * @param startModuleId The resource module / upper bound to start the call chain from.
	 * @return The generated call chain if it successful and there is a call chain.
	 */
	private Optional<CallChainGraph> calculateCallChainGraph(final FunctionalBlockGenerationContext context,
															 final EntityId startModuleId) {
		final Parameters callChainParameters = createCallChainParameters(context, List.of(startModuleId));
		return callChainService.createCallChainGraphs(context.getProgressMonitor(), callChainParameters)
                .filter(list -> !list.isEmpty()) // return empty if the list is empty or the operation was cancelled
				.map(list -> list.get(0)); // return empty if the Optional is empty, otherwise there should be only 1 entry because we started only with 1 module at the bottom
	}

	/**
	 * Creates the lower bound functional block for the whole reachability block.
	 * The lower bounds are all the resources the start module access in some way (read, write, ...).
	 *
	 * @param projectId The current projectId.
	 * @param graphExtractionResult result from the extraction to get all the required information.
	 * @param moduleBlocks The mapping from the module hash links to the generated from module functional block.
	 * @param reachabilityBlockChildren List of the reachability block children to append the lower bound block to.
	 * @param blocksToGenerate List of the functional block that have to be generated.
	 */
	private Map<ModuleLightweightPojo, FunctionalBlockPojoPrototype> generateLowerBoundBlocks(final EntityId projectId,
										  final GraphExtractionResult graphExtractionResult,
										  final Map<String, UUID> moduleBlocks,
										  final List<UUID> reachabilityBlockChildren,
										  final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> blocksToGenerate) {
		final Map<ModuleLightweightPojo, FunctionalBlockPojoPrototype> endModuleToLowerBoundBlock = graphExtractionResult.endModules.stream()
				.collect(Collectors.toMap(Function.identity(), module ->
					createBlock(projectId, moduleBlocks.get(module.getLinkHash()), module, FunctionalBlockType.RA_LOWER_BOUND)));

		endModuleToLowerBoundBlock.values().forEach(block -> {
			reachabilityBlockChildren.add(block.uid.getNonNull());
			blocksToGenerate.add(new FunctionalBlockGenerationResult<>(Operation.CREATE, block));
		});

		setAccessTypeForLowerBounds(graphExtractionResult.callChainGraph, endModuleToLowerBoundBlock);
		return endModuleToLowerBoundBlock;
	}

	/**
	 * Set the RA_ACCESS_TYPE flag on the lower bound blocks
	 *
	 * @param callChainGraph The call chain graph to the access modules
	 * @param endModuleToLowerBoundBlock The end modules to set the access for
	 */
	private void setAccessTypeForLowerBounds(final CallChainGraph callChainGraph,
											 final Map<ModuleLightweightPojo, FunctionalBlockPojoPrototype> endModuleToLowerBoundBlock) {
		for (final Map.Entry<ModuleLightweightPojo, FunctionalBlockPojoPrototype> entry : endModuleToLowerBoundBlock.entrySet()) {
			final ModuleLightweightPojo endModule = entry.getKey();
			final FunctionalBlockPojoPrototype lowerBoundBlock = entry.getValue();
			final Set<String> accessTypes = new HashSet<>();
			for (final ModuleLightweightPojo accessModule : callChainGraph.getTargetMap().get(endModule)) {
				callChainGraph.getEdgeMap().get(accessModule).stream()
						.filter(edge -> edge.getTarget().equals(endModule))
						.map(this::extractAccessTypes)
						.flatMap(Set::stream)
						.forEach(accessTypes::add);
			}
			lowerBoundBlock.flags.getNonNull().put(FunctionalBlockFlag.RA_ACCESS_TYPE.name(), new ArrayList<>(accessTypes));
		}
	}

	/**
	 * Creates the overall reachability block, for which all the other functional blocks this one serves as a parent to group them all together.
	 *
	 * @param projectId the project id
	 * @param startModule The lower bound for which this block is to be generated for
	 * @param children All the children of the block that is to be generated
	 * @param blocksToGenerate A list of all the blocks that are to be generated to append this one to.
	 */
	private static FunctionalBlockPojoPrototype generateReachabilityBlock(final EntityId projectId,
												  final ModuleLightweightPojo startModule,
												  final List<UUID> children,
												  final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> blocksToGenerate,
												  final FunctionalBlockGenerationResult.Operation operation,
												  final UUID uuid) {
		final FunctionalBlockPojoPrototype reachabilityBlock = new FunctionalBlockPojoPrototype();
		reachabilityBlock.setUid(uuid);
		reachabilityBlock.setProject(projectId);
		reachabilityBlock.setChildren(children);
		reachabilityBlock.setFlags(Map.of(
				FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY, FunctionalBlockType.RA_TOP_DOWN),
				FunctionalBlockFlag.GENERATED_BY.name(), RA_TOP_DOWN_GENERATION_ID,
				FunctionalBlockFlag.GENERATED_AT.name(), Instant.now().toEpochMilli(),
				FunctionalBlockFlag.OUTDATED.name(), false /* in case we are updating an existing block, reset its OUTDATED flag to false */,
				FunctionalBlockFlag.DELETED.name(), false /* in case we are updating an existing block, reset its DELETED flag to false */
		));
		reachabilityBlock.setName(startModule.getName());
		reachabilityBlock.setDescription("");
		blocksToGenerate.add(new FunctionalBlockGenerationResult<>(operation, reachabilityBlock));
		return reachabilityBlock;
	}


	/**
	 * Generates the call Chain block, which contains all the module functional blocks in between the both ends of the call chain.
	 *
	 * @param projectId The current projectId.
	 * @param graphExtractionResult result from the extraction to get all the required information.
	 * @param moduleBlocks The mapping from the module hash links to the generated from module functional block.
	 * @param reachabilityBlockChildren List of the reachability block children to append the lower bound block to.
	 * @param blocksToGenerate List of the functional block that have to be generated.
	 */
	@SuppressWarnings("unchecked")
	private static void generateCallChainBlock(final EntityId projectId,
											   final GraphExtractionResult graphExtractionResult,
											   final Map<String, UUID> moduleBlocks,
											   final List<UUID> reachabilityBlockChildren,
											   final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> blocksToGenerate) {
		final FunctionalBlockPojoPrototype callChainBlock = new FunctionalBlockPojoPrototype();
		callChainBlock.setUid(UUID.randomUUID());
		callChainBlock.setProject(projectId);
		callChainBlock.setChildren(new ArrayList<>(moduleBlocks.values()));
		callChainBlock.setFlags(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.CALL_CHAIN)));
		callChainBlock.setName(graphExtractionResult.startModule.getName() + " " + FunctionalBlockType.CALL_CHAIN);
		callChainBlock.setDescription("");
		reachabilityBlockChildren.add(callChainBlock.uid.getNonNull());

		final List<FunctionalBlockLink> callChainBlockLinks = new ArrayList<>();
		for (final Map.Entry<ModuleLightweightPojo, ModuleLightweightPojo> edgeEntry : graphExtractionResult.callChainGraph.getTargetMap().entries()) {
			final UUID sourceModuleId = moduleBlocks.get(edgeEntry.getValue().getLinkHash());
			final UUID targetModuleId = moduleBlocks.get(edgeEntry.getKey().getLinkHash());
			final Set<RelationshipType> callChainEdgeRelationshipType = new HashSet<>();
			final Set<Map<String, Object>> callChainEdgeAccessAttributes = new HashSet<>();
			graphExtractionResult.callChainGraph.getEdgeMap().get(edgeEntry.getValue())
					.stream()
					.filter(edge -> Objects.requireNonNull(edge.getTarget().getLinkHash()).equals(edgeEntry.getKey().getLinkHash()))
					.forEach(edge -> {
						final RelationshipType relationship = edge.getRelationship();
						callChainEdgeRelationshipType.add(relationship);
						if (relationship == RelationshipType.ACCESSES) {
							callChainEdgeAccessAttributes.add(edge.getRelationshipAttributes());
						}
					});
			final Map<String, Object> flags = new HashMap<>();
			flags.put(FunctionalBlockLinkFlag.TYPE.name(), List.of(FunctionalBlockLinkType.CALL_CHAIN, FunctionalBlockLinkType.DIRECTED));
			if ( ! callChainEdgeRelationshipType.isEmpty()) {
				flags.put(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name(), callChainEdgeRelationshipType);
				if ( ! callChainEdgeAccessAttributes.isEmpty()) {
					final Map<String, Object> attributes = new HashMap<>();
					callChainEdgeAccessAttributes.stream().map(Map::entrySet).flatMap(Collection::stream)
							.forEach(entry -> {
								if (entry.getValue() instanceof String) {
									((Collection<Object>) attributes.computeIfAbsent(entry.getKey(), k -> new HashSet<>())).add(entry.getValue());
								} else if (entry.getValue() instanceof Collection) {
									((Collection<Object>) attributes.computeIfAbsent(entry.getKey(), k -> new HashSet<>())).addAll((Collection<Object>) entry.getValue());
								}
							});
					flags.put(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_PROPERTIES.name(), attributes);
				}
			}
			callChainBlockLinks.add(new FunctionalBlockLink(UUID.randomUUID(),
					callChainBlock.uid.getNonNull(), sourceModuleId, targetModuleId, null, flags, null));
		}
		blocksToGenerate.add(new FunctionalBlockGenerationResult<>(Operation.CREATE, callChainBlock, callChainBlockLinks));
	}

	/**
	 * Creates the access functional block for the whole reachability block.
	 * The access modules are the modules which access the resource (lower bound) directly in the CallChain.
	 *
	 * @param projectId The current projectId.
	 * @param moduleBlocks The mapping from the module hash links to the generated from module functional block.
	 * @param reachabilityBlockChildren List of the reachability block children to append the lower bound block to.
	 * @param blocksToGenerate List of the functional block that have to be generated.
	 */
	private  Map<ModuleLightweightPojo, FunctionalBlockPojoPrototype>  generateAccessModuleBlocks(final EntityId projectId,
											final Map<String, UUID> moduleBlocks,
											final List<UUID> reachabilityBlockChildren,
											final Map<ModuleLightweightPojo, Collection<ModuleLightweightPojo>> endModuleToAccessModules,
											final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> blocksToGenerate) {

		final Map<ModuleLightweightPojo, FunctionalBlockPojoPrototype> accessModuleToAccessModuleBlock = endModuleToAccessModules.values().stream()
				.flatMap(Collection::stream)
				.distinct()
				.collect(Collectors.toMap(Function.identity(), accessModule ->
					createBlock(projectId, moduleBlocks.get(accessModule.getLinkHash()), accessModule, FunctionalBlockType.RA_ACCESS_MODULE)));

		accessModuleToAccessModuleBlock.values().forEach(block -> {
			reachabilityBlockChildren.add(block.uid.getNonNull());
			blocksToGenerate.add(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.CREATE, block));
		});
		return accessModuleToAccessModuleBlock;
	}

	/**
	 * Creates the upper bound functional blocks for the whole reachability block.
	 * The upper bound module is the start module of this generation.
	 *
	 * @param projectId The current projectId.
	 * @param startModule The  start modules which the upper bound block should be created for.
	 * @param moduleBlocks The mapping from the module hash links to the generated from module functional block.
	 * @param reachabilityBlockChildren List of the reachability block children to append the lower bound block to.
	 * @param blocksToGenerate List of the functional block that have to be generated.
	 */
	private void generateUpperBoundBlock(final EntityId projectId,
										 final ModuleLightweightPojo startModule,
										 final Map<String, UUID> moduleBlocks,
										 final List<UUID> reachabilityBlockChildren,
										 final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> blocksToGenerate) {
		final FunctionalBlockPojoPrototype upperBoundBlock = createBlock(projectId, moduleBlocks.get(startModule.getLinkHash()), startModule, FunctionalBlockType.RA_UPPER_BOUND);
		reachabilityBlockChildren.add(upperBoundBlock.uid.getNonNull());
		blocksToGenerate.add(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.CREATE, upperBoundBlock, null));
	}

	/**
	 * Get all the generated from modules functional blocks for the modules.
	 * For all the modules which doesn't yet have a functional block created, a functional block will be created.
	 *
	 * @param context The context which is required to generate the missing functional blocks.
	 * @param allModules All the modules to get the functional block from.
	 * @return A Map from the module link hash to the functional block uuid.
	 */
	private Map<String, UUID> getModuleBlocks(final FunctionalBlockGenerationContext context,
											  final Map<String, ModuleLightweightPojo> allModules) {
		final Map<String, UUID> modulesToFb = new HashMap<>(functionalBlockService.findGeneratedFromModules(allModules.keySet(), context.getProjectId()));
		final Set<String> missingModules = new HashSet<>(allModules.keySet());
		missingModules.removeAll(modulesToFb.keySet());
		missingModules.forEach(moduleLinkHash ->
				functionalBlockGenerationService.generate(ModuleBlockGeneration.class, context, EntityId.of(allModules.get(moduleLinkHash).getId())));
		modulesToFb.putAll(functionalBlockService.findGeneratedFromModules(missingModules, context.getProjectId()));
		return modulesToFb;
	}

	/**
	 * Convenience function to create a reachability block.
	 *
	 * @param projectId The project id
	 * @param moduleBlockId The uuid of the module generated functional block
	 * @param module The module itself
	 * @param type The type of the functional block.
	 * @return The functional block prototype
	 */
	private FunctionalBlockPojoPrototype createBlock(final EntityId projectId,
													 final UUID moduleBlockId,
													 final ModuleLightweightPojo module,
													 final FunctionalBlockType type) {
		final FunctionalBlockPojoPrototype block = new FunctionalBlockPojoPrototype();
		block.setUid(UUID.randomUUID());
		block.setProject(projectId);
		block.setChildren(List.of(moduleBlockId));
		block.setFlags(new HashMap<>(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(type))));
		block.setName(module.getName() + " " + type);
		block.setDescription("");
		return block;
	}

	/**
	 * Creates the parameters to be used to walk the call chain downwards until the endpoints (resources which are accessed).
	 *
	 * @param context The context to get the project id from.
	 * @param startModuleIds The start modules the call chain should start from.
	 * @return The parameters
	 */
	private Parameters createCallChainParameters(final FunctionalBlockGenerationContext context, final List<EntityId> startModuleIds) {
		final var parameterBuilder = new Parameters.Builder();

		parameterBuilder.setProjectId(context.getProjectId());
		parameterBuilder.setStartModuleIds(startModuleIds);
		parameterBuilder.setStartModuleTypes(Collections.emptySet());
		parameterBuilder.setEndModuleIds(Collections.emptyList());
		parameterBuilder.setEndModuleTypes(context.getReachabilityAnalysisConfig().getLowerBoundModuleTypes().parallelStream()
				.map(ModuleType::getType).collect(Collectors.toSet()));
		parameterBuilder.setDirections(List.of(CallChain.CallChainDirection.OUT));
		parameterBuilder.setCallTypes(CallChainExporterJob.ALL_RELATIONSHIP_TYPES.stream().map(RelationshipType::from).collect(Collectors.toSet()));
		parameterBuilder.setFilteredModuleNames(Collections.emptySet());
		parameterBuilder.setFilteredModuleTypes(Collections.emptySet());
		parameterBuilder.setIgnoredTaxonomies(Collections.emptyList());

		return parameterBuilder.build();
	}

	/**
	 * Get the access type(s) of the edge to be stored in the block later.
	 *
	 * @param edge The edge to extract the access type(s) from.
	 * @return A set of the access types
	 */
	private Set<String> extractAccessTypes(final CallChainGraph.CallChainEdge edge) {
		final Map<String, Object> attributes = edge.getRelationshipAttributes();
		final Set<String> ret = new HashSet<>();

		/* in lack of a good enum constant, we will use String for now :-( */
		if (isReadAccess(attributes)) {
			ret.add("READ");
		}
		if (isWriteAccess(attributes)) {
			ret.add("WRITE");
		}

		return ret;
	}

	/**
	 * Check if the attributes contains a read access.
	 *
	 * @param attributes The attributes to check.
	 * @return true if it there is a read access, false otherwise
	 */
	/* copied from CallChainService - should be moved to an appropriate location (utility or base class) */

	private static boolean isReadAccess(final Map<String, Object> attributes) {
		final String fileAccessTypes = String.valueOf(attributes.get(ModelAttributeKey.FILE_ACCESS_TYPE.toString()));
		final String dbAccessTypes = String.valueOf(attributes.get(ModelAttributeKey.DB_ACCESS_TYPE.toString()));

		return StringUtils.contains(fileAccessTypes, ModelAttributeValue.FileAccess.READ.toString())
				|| StringUtils.contains(dbAccessTypes, DatabaseAccessType.READ.toString());
	}

	/**
	 * Check if the attributes contains a write access.
	 *
	 * @param attributes The attributes to check.
	 * @return true if it there is a write access, false otherwise
	 */
	private static boolean isWriteAccess(final Map<String, Object> attributes) {
		final String fileAccessTypes = String.valueOf(attributes.get(ModelAttributeKey.FILE_ACCESS_TYPE.toString()));
		final String dbAccessTypes = String.valueOf(attributes.get(ModelAttributeKey.DB_ACCESS_TYPE.toString()));

		return StringUtils.contains(fileAccessTypes, ModelAttributeValue.FileAccess.WRITE.toString())
				|| StringUtils.contains(dbAccessTypes, DatabaseAccessType.STORE.toString())
				|| StringUtils.contains(dbAccessTypes, DatabaseAccessType.UPDATE.toString())
				|| StringUtils.contains(dbAccessTypes, DatabaseAccessType.DELETE.toString());
	}


	/**
	 * Collects all the required information from the given graph into a single result class.
	 *
	 * @param callChainGraph The first run of the call chain graph to extract the information form
	 * @return The extracted information
	 */
	private static GraphExtractionResult extractRequiredInformationFromGraph(final FunctionalBlockGenerationContext context,
			final CallChainGraph callChainGraph) {
		final Set<ModuleType> lowerBoundModuleTypes = new HashSet<>(context.getReachabilityAnalysisConfig().getLowerBoundModuleTypes());
		final Map<String, ModuleLightweightPojo> allModules = new HashMap<>(callChainGraph.getSize());
		final ModuleLightweightPojo startModule = callChainGraph.getRoot();
		final Set<ModuleLightweightPojo> endModules = new HashSet<>(callChainGraph.getEndModules(
				module -> lowerBoundModuleTypes.contains(ModuleType.fromTechnologyAndType(module.getTechnology(), module.getType()))));
		allModules.put(startModule.getLinkHash(), startModule);
		callChainGraph.getTargetMap().values().forEach(module -> allModules.put(module.getLinkHash(), module));
		callChainGraph.getTargetMap().keySet().forEach(module -> allModules.put(module.getLinkHash(), module));
		return new GraphExtractionResult(allModules, startModule, endModules, callChainGraph);
	}

	private Optional<UUID> handleExistingReachabilityBlock(final ModuleLightweightPojo moduleLightweight,
			final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> blocksToGenerate, final FunctionalBlockGenerationContext context) {
		final Optional<FunctionalBlockPojo> topDownOptional = functionalBlockService.find(q -> q
				.ofProject(context.getProjectId())
				.withType(FunctionalBlockType.RA_TOP_DOWN)
				.withChild(c1 -> c1
						.withType(FunctionalBlockType.RA_UPPER_BOUND)
						.withChild(c2 -> c2
								.generatedFromModule(moduleLightweight.getLinkHash()))))
				.stream()
				.findAny();

		if (topDownOptional.isPresent()) {
			removeReachabilityBlockChildren(topDownOptional.get(), blocksToGenerate);
			return Optional.of(topDownOptional.get().getUid());
		}
		return Optional.empty();
	}

	private void removeReachabilityBlockChildren(final FunctionalBlockPojo blockPojo,
			final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> blocksToGenerate) {
		final List<FunctionalBlockPojoPrototype> reachabilityBlockChildren = functionalBlockService.get(blockPojo
				.getChildren()).stream()
				.map(fb -> new FunctionalBlockPojoPrototype().setUid(fb.getUid()))
				.toList();
		reachabilityBlockChildren.forEach(block ->
				blocksToGenerate.add(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.DELETE, block)));
	}

	/**
	 * Convenience class to store the extracted information from a graph.
	 */
	private static class GraphExtractionResult {
		private final Map<String, ModuleLightweightPojo> allModules;
		private final ModuleLightweightPojo startModule;
		private final Set<ModuleLightweightPojo> endModules;
		private final CallChainGraph callChainGraph;

		private GraphExtractionResult(final Map<String, ModuleLightweightPojo> allModules,
									  final ModuleLightweightPojo startModule,
									  final Set<ModuleLightweightPojo> endModules,
									  final CallChainGraph callChainGraph) {
			this.allModules = allModules;
			this.startModule = startModule;
			this.endModules = endModules;
			this.callChainGraph = callChainGraph;
		}
	}
	 @Override
	 public void persistAdditionalData(final FunctionalBlockGenerationContext context, final UUID functionalBlockId, final List<FunctionalBlockLink> links) {
		 functionalBlockService.setLinks(functionalBlockId, links);
	}
}
