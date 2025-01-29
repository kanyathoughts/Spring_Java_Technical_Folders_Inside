/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.generation.reachabilityanalysis;

import innowake.lib.core.api.lang.Nullable;
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
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;

import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Computes the bottom-up for the given entity and stores the result a functional block.
 */
@Component
public class ReachabilityBottomUpGeneration implements FunctionalBlockGeneration<EntityId, Void> {
	/* continue searching recursively for upper bounds when encountering one of these upper bound types */
	private static final Set<Type> RECURSIVE_UPPER_BOUND_TYPES = Set.of(Type.STORED_PROCEDURE);

	private FunctionalBlockGenerationService functionalBlockGenerationService;
	private final FunctionalBlockService functionalBlockService;
	private final CallChainService callChainService;

	public ReachabilityBottomUpGeneration(final FunctionalBlockService functionalBlockService,
										  final CallChainService callChainService) {
		this.functionalBlockService = functionalBlockService;
		this.callChainService = callChainService;
	}

	@Override
	public void setFunctionalBlockGenerationService(final FunctionalBlockGenerationService service) {
		this.functionalBlockGenerationService = service;
	}

	@Override
	public Collection<FunctionalBlockGenerationResult<Void>> generate(final FunctionalBlockGenerationContext context, @Nullable final EntityId startModuleId) {
		if (startModuleId == null) {
			return Collections.emptyList();
		}

		final Optional<CallChainGraph> optionalCallChainGraph = calculateCallChainGraph(context, startModuleId);
		if (optionalCallChainGraph.isEmpty()) {
			// since we don't have a callchain to gather information from, a functional block for the ra can't be created.
			return Collections.emptyList();
		}

		// extract all the information of the graph required for the functional block(s).
		final GraphExtractionResult extractionResult = extractRequiredInformationFromGraph(context, optionalCallChainGraph.get());

		return generateBlocks(context, extractionResult);
	}


	/**
	 * Generates all the blocks from the extraction result.
	 *
	 * @param context The context.
	 * @param graphExtractionResult The result of the call chain analysis.
	 * @return A list of blocks that have to be created.
	 */
	private List<FunctionalBlockGenerationResult<Void>> generateBlocks(final FunctionalBlockGenerationContext context,
																	   final GraphExtractionResult graphExtractionResult) {
		final List<FunctionalBlockGenerationResult<Void>> blocksToGenerate = new ArrayList<>();
		final List<UUID> reachabilityBlockChildren = new ArrayList<>();

		final Map<String, UUID> moduleBlocks = getModuleBlocks(context, graphExtractionResult.allModules);

		final Optional<UUID> reachabilityBlockUidOptional = 
				handleExistingReachabilityBlock(graphExtractionResult.startModule, blocksToGenerate, context);
		UUID reachabilityBlockUid = UUID.randomUUID();
		FunctionalBlockGenerationResult.Operation operation = Operation.CREATE;
		if (reachabilityBlockUidOptional.isPresent()) {
			reachabilityBlockUid = reachabilityBlockUidOptional.get();
			operation = Operation.UPDATE;
		}

		generateLowerBoundBlock(context.getProjectId(), graphExtractionResult.startModule, moduleBlocks, reachabilityBlockChildren, blocksToGenerate);

		generateAccessModuleBlocks(context.getProjectId(), graphExtractionResult, moduleBlocks, reachabilityBlockChildren, blocksToGenerate);

		generateUpperBoundBlocks(context.getProjectId(), graphExtractionResult.endModules, moduleBlocks, reachabilityBlockChildren, blocksToGenerate);

		generateCallChainBlock(context.getProjectId(), graphExtractionResult.startModule, moduleBlocks, reachabilityBlockChildren, blocksToGenerate);

		generateReachabilityBlock(context.getProjectId(),
				graphExtractionResult.startModule, reachabilityBlockChildren, blocksToGenerate, operation, reachabilityBlockUid);

		return blocksToGenerate;
	}


	/**
	 * Creates the lower bound functional block for the whole reachability block.
	 * The lower bound is (in this case) the module the generation is started from.
	 *
	 * @param projectId The current projectId.
	 * @param startModule The module for which the lower bound is created.
	 * @param moduleBlocks The mapping from the module hash links to the generated from module functional block.
	 * @param reachabilityBlockChildren List of the reachability block children to append the lower bound block to.
	 * @param blocksToGenerate List of the functional block that have to be generated.
	 */
	private void generateLowerBoundBlock(final EntityId projectId,
										 final ModuleLightweightPojo startModule,
										 final Map<String, UUID> moduleBlocks,
										 final List<UUID> reachabilityBlockChildren,
										 final List<FunctionalBlockGenerationResult<Void>> blocksToGenerate) {
		final FunctionalBlockPojoPrototype lowerBoundBlock = createBlock(projectId, moduleBlocks.get(startModule.getLinkHash()), startModule, FunctionalBlockType.RA_LOWER_BOUND);
		reachabilityBlockChildren.add(lowerBoundBlock.uid.getNonNull());
		blocksToGenerate.add(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.CREATE, lowerBoundBlock));
	}

	/**
	 * Creates the access functional block for the whole reachability block.
	 * The access modules are the modules which access the resource (lower bound) directly in the CallChain.
	 *
	 * @param projectId The current projectId.
	 * @param graphExtractionResult The result to extract all the required information from.
	 * @param moduleBlocks The mapping from the module hash links to the generated from module functional block.
	 * @param reachabilityBlockChildren List of the reachability block children to append the lower bound block to.
	 * @param blocksToGenerate List of the functional block that have to be generated.
	 */
	private void generateAccessModuleBlocks(final EntityId projectId,
											final GraphExtractionResult graphExtractionResult,
											final Map<String, UUID> moduleBlocks,
											final List<UUID> reachabilityBlockChildren,
											final List<FunctionalBlockGenerationResult<Void>> blocksToGenerate) {
		final List<ModuleLightweightPojo> accessModules = graphExtractionResult.callChainGraph.getEdgeMap()
				.get(graphExtractionResult.startModule)
				.stream()
				.map(CallChainGraph.CallChainEdge::getTarget)
				.toList();
		final List<FunctionalBlockPojoPrototype> accessModuleBlocks = accessModules.stream()
				.map(module -> createBlock(projectId, moduleBlocks.get(module.getLinkHash()), module, FunctionalBlockType.RA_ACCESS_MODULE))
				.toList();
		accessModuleBlocks.forEach(block -> reachabilityBlockChildren.add(block.uid.getNonNull()));
		accessModuleBlocks.forEach(block -> blocksToGenerate.add(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.CREATE, block,
				null)));
	}


	/**
	 * Creates the upper bound functional blocks for the whole reachability block.
	 * The upper bound modules are the modules which are at the endpoints of the call chains, coming from the lower block.
	 * Basically, those are the jobs, service requests, ... that access the lower bound in the end in some way (read, write, ...).
	 *
	 * @param projectId The current projectId.
	 * @param endModules The end modules which the upper bound blocks should be created for.
	 * @param moduleBlocks The mapping from the module hash links to the generated from module functional block.
	 * @param reachabilityBlockChildren List of the reachability block children to append the lower bound block to.
	 * @param blocksToGenerate List of the functional block that have to be generated.
	 */
	private void generateUpperBoundBlocks(final EntityId projectId,
										  final Set<ModuleLightweightPojo> endModules,
										  final Map<String, UUID> moduleBlocks,
										  final List<UUID> reachabilityBlockChildren,
										  final List<FunctionalBlockGenerationResult<Void>> blocksToGenerate) {
		final List<FunctionalBlockPojoPrototype> upperBoundBlocks = endModules.stream()
				.map(module -> createBlock(projectId, moduleBlocks.get(module.getLinkHash()), module, FunctionalBlockType.RA_UPPER_BOUND))
				.toList();
		upperBoundBlocks.forEach(block -> reachabilityBlockChildren.add(block.uid.getNonNull()));
		upperBoundBlocks.forEach(block -> blocksToGenerate.add(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.CREATE, block,
				null)));
	}

	/**
	 * Creates the overall reachability block, for which all the other functional blocks this one serves as a parent to group them all together.
	 *
	 * @param projectId the project Id
	 * @param startModule The lower bound for which this block is to be generated for
	 * @param children All the children of the block that is to be generated
	 * @param blocksToGenerate A list of all the blocks that are to be generated to append this one to.
	 */
	private static void generateReachabilityBlock(final EntityId projectId,
												  final ModuleLightweightPojo startModule,
												  final List<UUID> children,
												  final List<FunctionalBlockGenerationResult<Void>> blocksToGenerate,
												  final FunctionalBlockGenerationResult.Operation operation,
												  final UUID blockUid) {
		final FunctionalBlockPojoPrototype reachabilityBlock = new FunctionalBlockPojoPrototype();
		reachabilityBlock.setUid(blockUid);
		reachabilityBlock.setProject(projectId);
		reachabilityBlock.setChildren(children);
		reachabilityBlock.setFlags(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY, FunctionalBlockType.RA_BOTTOM_UP)));
		reachabilityBlock.setName(startModule.getName());
		reachabilityBlock.setDescription("");
		blocksToGenerate.add(new FunctionalBlockGenerationResult<>(operation, reachabilityBlock));
	}

	/**
	 * Generates the call Chain block, which contains all the module functional blocks in between the both ends of the call chain.
	 *
	 * @param projectId The current projectId.
	 * @param startModule The start module / lower bound for this block is to be generated for.
	 * @param moduleBlocks The mapping from the module hash links to the generated from module functional block.
	 * @param reachabilityBlockChildren List of the reachability block children to append the lower bound block to.
	 * @param blocksToGenerate List of the functional block that have to be generated.
	 */
	private static void generateCallChainBlock(final EntityId projectId,
											   final ModuleLightweightPojo startModule,
											   final Map<String, UUID> moduleBlocks,
											   final List<UUID> reachabilityBlockChildren,
											   final List<FunctionalBlockGenerationResult<Void>> blocksToGenerate) {
		final FunctionalBlockPojoPrototype callChainBlock = new FunctionalBlockPojoPrototype();
		callChainBlock.setUid(UUID.randomUUID());
		callChainBlock.setProject(projectId);
		callChainBlock.setChildren(new ArrayList<>(moduleBlocks.values()));
		callChainBlock.setFlags(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.CALL_CHAIN)));
		callChainBlock.setName(startModule.getName() + " " + FunctionalBlockType.CALL_CHAIN);
		callChainBlock.setDescription("");
		reachabilityBlockChildren.add(callChainBlock.uid.getNonNull());
		blocksToGenerate.add(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.CREATE, callChainBlock, null));
	}

	/**
	 * Calculates the (initial) call chain graph starting from the module walking upwards.
	 *
	 * @param context The context which holds some information required for the call chain graph calculation
	 * @param startModuleId The resource module / lower bound to start the call chain from.
	 * @return The generated call chain if it successful and there is a call chain.
	 */
	private Optional<CallChainGraph> calculateCallChainGraph(final FunctionalBlockGenerationContext context, final EntityId startModuleId) {
		final Parameters callChainParameters = createCallChainParameters(context, List.of(startModuleId));
		return callChainService.createCallChainGraphs(context.getProgressMonitor(), callChainParameters)
                .filter(list -> !list.isEmpty()) // return empty if the list is empty or the operation was cancelled
                .map(list -> list.get(0)); // return empty if the Optional is empty, otherwise there should be only 1 entry because we started only with 1 module at the bottom
	}

	/**
	 * The call chain stops when encountering one of the upper bound types. But in case one upper bound module calls another upper bound module -
	 * for example a Job calling a Stored Procedure - then we would only find the first one. Therefore, we continue the call chain with each
	 * upper bound module until we can't find additional ones.
	 *
	 * @param context The context which holds some information required for the call chain graph calculation
	 * @param allModules A map of all the modules which where found in the first invocation of the call chain generation
	 * @param endModules All the end modules which where identified in the first invocation and which should potentially be walked up again.
	 */
	private void collectAdditionalUpperBoundModules(final FunctionalBlockGenerationContext context,
													final Map<String, ModuleLightweightPojo> allModules,
													final Set<ModuleLightweightPojo> endModules) {
		Set<ModuleLightweightPojo> additionalUpperBounds = endModules.stream().filter(m -> RECURSIVE_UPPER_BOUND_TYPES.contains(m.getType())).collect(Collectors.toSet());
		while ( ! additionalUpperBounds.isEmpty()) {
			final Parameters callChainParameters = createCallChainParameters(context,
					additionalUpperBounds.stream().map(ModuleLightweightPojo::identity).collect(Collectors.toList()));
			final Optional<List<CallChainGraph>> callChainGraphs = callChainService.createCallChainGraphs(context.getProgressMonitor(), callChainParameters);
			if (callChainGraphs.isEmpty()) {
				return;
			}
			if (callChainGraphs.get().isEmpty()) {
				return;
			}

			additionalUpperBounds = new HashSet<>();
			for (final CallChainGraph callChainGraph : callChainGraphs.get()) {
				final List<ModuleLightweightPojo> additionalEndModules = callChainGraph.getEndModules(module -> true);
				for (final ModuleLightweightPojo additionalEndModule : additionalEndModules) {
					if ( ! endModules.contains(additionalEndModule)) {
						endModules.add(additionalEndModule);
						callChainGraph.getTargetMap().values().forEach(module -> allModules.put(module.getLinkHash(), module));
						callChainGraph.getTargetMap().keySet().forEach(module -> allModules.put(module.getLinkHash(), module));
						if (RECURSIVE_UPPER_BOUND_TYPES.contains(additionalEndModule.getType())) {
							additionalUpperBounds.add(additionalEndModule);
						}
					}
				}
			}
		}
	}

	/**
	 * Get all the generated from modules functional blocks for the modules.
	 * For all the modules which doesn't yet have a functional block created, a functional block will be created.
	 *
	 * @param context The context which is required to generate the missing functional blocks.
	 * @param allModules All the modules to get the functional block from.
	 * @return A Map from the module link hash to the functional block uuid.
	 */
	private Map<String, UUID> getModuleBlocks(final FunctionalBlockGenerationContext context, final Map<String, ModuleLightweightPojo> allModules) {
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
		block.setFlags(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(type)));
		block.setName(module.getName() + " " + type);
		block.setDescription("");
		return block;
	}

	/**
	 * Creates the parameters to be used to walk the call chain upwards until the endpoints.
	 *
	 * @param context The context for the block generation.
	 * @param startModuleIds The start modules the call chain should start from.
	 * @return The parameters
	 */
	private Parameters createCallChainParameters(final FunctionalBlockGenerationContext context, final List<EntityId> startModuleIds) {
		final var parameterBuilder = new Parameters.Builder();

		parameterBuilder.setProjectId(context.getProjectId());
		parameterBuilder.setStartModuleIds(startModuleIds);
		parameterBuilder.setStartModuleTypes(Collections.emptySet());
		parameterBuilder.setEndModuleIds(Collections.emptyList());
		parameterBuilder.setEndModuleTypes(context.getReachabilityAnalysisConfig().getUpperBoundModuleTypes()
				.parallelStream().map(ModuleType::getType).collect(Collectors.toSet()));
		parameterBuilder.setDirections(List.of(CallChain.CallChainDirection.IN));
		parameterBuilder.setCallTypes(CallChainExporterJob.ALL_RELATIONSHIP_TYPES.stream().map(RelationshipType::from).collect(Collectors.toSet()));
		parameterBuilder.setFilteredModuleNames(Collections.emptySet());
		parameterBuilder.setFilteredModuleTypes(Collections.emptySet());
		parameterBuilder.setIgnoredTaxonomies(Collections.emptyList());

		return parameterBuilder.build();
	}

	/**
	 * Collects all the required information from the given graph.
	 * It will also walk the call chain again for the endpoints which are also called by other endpoints.
	 *
	 * @param context The context for the block generation.
	 * @param callChainGraph The first run of the call chain graph to extract the information form
	 * @return The extracted information
	 */
	private GraphExtractionResult extractRequiredInformationFromGraph(final FunctionalBlockGenerationContext context,
																	  final CallChainGraph callChainGraph) {
		final Set<ModuleType> moduleTypes = new HashSet<>(context.getReachabilityAnalysisConfig().getUpperBoundModuleTypes());
		final Map<String, ModuleLightweightPojo> allModules = new HashMap<>(callChainGraph.getSize());
		final ModuleLightweightPojo startModule = callChainGraph.getRoot();
		final Set<ModuleLightweightPojo> endModules = new HashSet<>(callChainGraph.getEndModules(
				module -> moduleTypes.contains(ModuleType.fromTechnologyAndType(module.getTechnology(), module.getType()))));
		allModules.put(startModule.getLinkHash(), startModule);
		callChainGraph.getTargetMap().values().forEach(module -> allModules.put(module.getLinkHash(), module));
		callChainGraph.getTargetMap().keySet().forEach(module -> allModules.put(module.getLinkHash(), module));

		// the bottom up can have additional upper bound modules, because e.g. a SP (which is an endpoint) can be called by another endpoint.
		collectAdditionalUpperBoundModules(context, allModules, endModules);
		return new GraphExtractionResult(allModules, startModule, endModules, callChainGraph);
	}

	private Optional<UUID> handleExistingReachabilityBlock(final ModuleLightweightPojo moduleLightweight,
			final List<FunctionalBlockGenerationResult<Void>> blocksToGenerate, final FunctionalBlockGenerationContext context) {

		final Optional<FunctionalBlockPojo> bottomUpBlockOptional = functionalBlockService.find(q -> q
				.ofProject(context.getProjectId())
				.withType(FunctionalBlockType.RA_BOTTOM_UP)
				.withChild(c1 -> c1
						.withType(FunctionalBlockType.RA_LOWER_BOUND)
						.withChild(c2 -> c2
								.generatedFromModule(moduleLightweight.getLinkHash()))))
				.stream()
				.findAny();

		if (bottomUpBlockOptional.isPresent()) {
			removeReachabilityBlockChildren(bottomUpBlockOptional.get(), blocksToGenerate);
			return Optional.of(bottomUpBlockOptional.get().getUid());
		}
		return Optional.empty();
	}

	private void removeReachabilityBlockChildren(final FunctionalBlockPojo blockPojo, final List<FunctionalBlockGenerationResult<Void>> blocksToGenerate) {
		final List<FunctionalBlockPojoPrototype> reachabilityBlockChildren = functionalBlockService.get(blockPojo.getChildren())
				.stream()
				.map(fb -> new FunctionalBlockPojoPrototype().setUid(fb.getUid()))
				.toList();
		reachabilityBlockChildren.forEach(block ->
			blocksToGenerate.add(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.DELETE, block)));
	}

	/**
	 * Convenience class to store the (aggregated) extracted information from one or more graphs.
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
}
