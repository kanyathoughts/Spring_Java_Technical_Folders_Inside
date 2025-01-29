/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.generation.datalineagefunctionalblock;

import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.datalineage.query.DataFlowGraphQueryService;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.server.datalineage.query.QueryDirection;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGeneration;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult.Operation;
import innowake.mining.server.functionalblocks.service.FunctionalBlockControlFlowGraphGenerationService;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockWithChildrenAndLinks;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.datalineage.SourceLocation;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Generating a functional block based on data lineage.
 */
@Component
public class DataLineageFunctionalBlockGeneration implements FunctionalBlockGeneration<List<EntityId>, List<FunctionalBlockLink>> {

	private static final Logger LOG = LoggerFactory.getLogger(DataLineageFunctionalBlockGeneration.class);
	private final ModuleService moduleDao;
	private final DataDictionaryService dataDictionaryDao;
	private final ExecutorService executorService;
	private final FunctionalBlockService functionalBlockService;
	private final DataFlowGraphQueryService dataFlowGraphQueryService;
	private final FunctionalBlockControlFlowGraphGenerationService functionalBlockControlFlowGraphGenerationService;
	private static final String EMPTY_BLOCKS_MESSAGE = "Could not identify functional block for %s in %s. No data flow related statements found for %s";
	private static final String GENERATED_FUNCTIONAL_BLOCKS = "Functional block(s) generated %s/%s";

	public DataLineageFunctionalBlockGeneration(final ModuleService moduleDao, final DataDictionaryService dataDictionaryDao,
			final ExecutorService executorService, final FunctionalBlockService functionalBlockService,
			final DataFlowGraphQueryService dataFlowGraphQueryService,
			final FunctionalBlockControlFlowGraphGenerationService functionalBlockControlFlowGraphGenerationService) {
		this.moduleDao = moduleDao;
		this.dataDictionaryDao = dataDictionaryDao;
		this.executorService = executorService;
		this.functionalBlockService = functionalBlockService;
		this.dataFlowGraphQueryService = dataFlowGraphQueryService;
		this.functionalBlockControlFlowGraphGenerationService = functionalBlockControlFlowGraphGenerationService;
	}

	@Override
	public Collection<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> generate(final FunctionalBlockGenerationContext context,
			@Nullable final List<EntityId> ddeUids) {

		if ( ddeUids == null || ddeUids.isEmpty() ) {
			throw new IllegalArgumentException("No Data Dictionaries Ids provided");
		}
		LOG.info("Generating functional blocks using data lineage for data dictionary entries : {} ",
				ddeUids.stream().map(EntityId :: toString).collect(Collectors.joining(", ")));

		final List<DataDictionaryPojo> dataVariables = findVariables(ddeUids);
		if ( dataVariables.isEmpty() ) {
			throw new IllegalStateException("No variables found for the given UUIDs");
		}
		LOG.info("Found variables for generating functional blocks : {}",
				dataVariables.stream().map(DataDictionaryPojo :: getName).collect(Collectors.joining(", ")));

		final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> results = new ArrayList<>();

		final Map<EntityId, List<DataDictionaryPojo>> moduleIdToVariables = groupVariablesByModule(context, dataVariables);

		final List<String> emptyBlocksAdditionalData = new LinkedList<>();
		for (final Map.Entry<EntityId, List<DataDictionaryPojo>> entry : moduleIdToVariables.entrySet()) {
			generateFunctionalBlocksForDataDictionaries(context, entry.getKey(), entry.getValue(), results, emptyBlocksAdditionalData);
		}
		emptyBlocksAdditionalData.add(0,
				String.format(GENERATED_FUNCTIONAL_BLOCKS, moduleIdToVariables.size() - emptyBlocksAdditionalData.size(), moduleIdToVariables.size()));
		context.setAdditionalData(emptyBlocksAdditionalData);
		return results;
	}

	/**
	 * This method generates functional blocks for a given module and a list of variables.
	 * It first computes the Control Flow Graph for the module, then computes the Data Lineage for the variables.
	 * After that, it generates a Functional Block for the variables by creating annotation for statement location
	 * from data lineage graph and conditions from control flow graph.
	 *
	 * @param context The context of the FunctionalBlockGeneration.
	 * @param moduleId The ID of the module for which the functional blocks are to be generated.
	 * @param variables The list of variables for which the functional blocks are to be generated.
	 * @param results The list of results where the generated functional blocks will be added.
	 */
	private void generateFunctionalBlocksForDataDictionaries(final FunctionalBlockGenerationContext context, final EntityId moduleId,
			final List<DataDictionaryPojo> variables, final List<FunctionalBlockGenerationResult<List<FunctionalBlockLink>>> results,
			final List<String> emptyBlocksAdditionalData) {
		final ModulePojo module = moduleDao.getModule(moduleId);
		LOG.debug("Generating functional blocks for module : {}, data dictionaries entries: {}", moduleId,
				variables.stream().map(DataDictionaryPojo :: getName).collect(Collectors.joining(", ")));

		/* Set progress description and compute the Control Flow Graph (CFG) for the module */
		context.getProgressMonitor().setStepDescription("Computing Control Flow Graph for " + module.getName());
		LOG.info("Computing Control Flow Graph for module : {}", moduleId);
		try {
			executorService.executeControlFlowCalculation(moduleId);
			context.getProgressMonitor().setStepDescription("Computed Control Flow Graph for " + module.getName());
			LOG.info("Computed Control Flow Graph for module : {}", moduleId);
		} catch (final Exception e) {
			LOG.error("Error during control flow calculation for module with Id " + moduleId, e);
			return;
		}
		final String variablesNames = variables.stream().map(DataDictionaryPojo :: getName).collect(Collectors.joining(", "));

		/* Set progress description and compute the Data Lineage for the variables */
		context.getProgressMonitor().setStepDescription("Computing Data Lineage for " + variablesNames);
		LOG.info("Computing Data Lineage for variables : {}", variablesNames);
		final DataFlowGraph dataFlowGraph = getDataFlowGraph(context, module, variables);
		final List<ModuleLocation> statementLocations = getStatementLocations(dataFlowGraph);
		if ( dataFlowGraph.getNodes().isEmpty() || statementLocations.isEmpty() ) {
			LOG.debug(() -> String.format(EMPTY_BLOCKS_MESSAGE, variablesNames, module.getName(), variablesNames));
			emptyBlocksAdditionalData.add(String.format(EMPTY_BLOCKS_MESSAGE, variablesNames, module.getName(), variablesNames));
			return;
		}
		context.getProgressMonitor().setStepDescription("Computed Data Lineage for " + variablesNames);
		LOG.info("Computed Data Lineage for variables : {}", variablesNames);

		/* Prepare flags for the functional block and create functional block prototype */
		final Map<String, Object> functionalBlockFlags = new HashMap<>();
		functionalBlockFlags.put(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP.name()));
		functionalBlockFlags.put(FunctionalBlockFlag.GENERATED_BY.name(), DataLineageFunctionalBlockGeneration.class.getSimpleName());
		functionalBlockFlags.put(FunctionalBlockFlag.GENERATED_AT.name(), Instant.now().toEpochMilli());
		functionalBlockFlags.put(FunctionalBlockFlag.HAS_CFG.name(), true);

		context.getProgressMonitor().setStepDescription("Generating Functional Block for " + variablesNames);
		LOG.info("Generating Functional Block for variables : {}", variablesNames);
		final FunctionalBlockPojoPrototype functionalBlock = new FunctionalBlockPojoPrototype().setProject(context.getProjectId())
				.setName("Functional blocks for " + variablesNames + " in " + module.getName())
				.setDescription("Automatic Generated Functional Blocks for Data Variables : " + variablesNames + " in Module: " + module.getName())
				.setFlags(functionalBlockFlags);

		/* Generate functional block with children and links */
		final FunctionalBlockWithChildrenAndLinks functionalBlockWithChildrenAndLinks =
				functionalBlockControlFlowGraphGenerationService.generateConditionsAndLinksForStatements(
				context.getProjectId(), moduleId, module.getLinkHash(), functionalBlock, statementLocations, Collections.emptyList());

		if ( functionalBlockWithChildrenAndLinks.getFunctionalBlock().children.isNullable() ) {
			LOG.debug(() -> String.format("No children found for functional block for %s in %s", variablesNames, module.getName()));
			emptyBlocksAdditionalData.add(
					String.format("Could not identify functional block for %s in %s. because functional block has no annotations to " + "get assigned.",
							variablesNames, module.getName()));
			return;
		}
		results.addAll(functionalBlockWithChildrenAndLinks.getChildren().stream()
				.map(block -> new FunctionalBlockGenerationResult<List<FunctionalBlockLink>>(Operation.CREATE, block)).toList());
		results.add(new FunctionalBlockGenerationResult<>(Operation.CREATE, functionalBlockWithChildrenAndLinks.getFunctionalBlock(),
				functionalBlockWithChildrenAndLinks.getFunctionalBlockLinks()));
		context.getProgressMonitor().setStepDescription("Generated Functional Block for " + variablesNames);
		LOG.info("Generated Functional Block for variables : {}", variablesNames);
	}

	/**
	 * This method finds variables based on a list of UUIDs.
	 * It queries the dataDictionaryDao with the UUIDs and returns the found variables.
	 *
	 * @param uuids The list of UUIDs for which the variables are to be found.
	 * @return A list of found variables.
	 */
	private List<DataDictionaryPojo> findVariables(final List<EntityId> uuids) {
		return dataDictionaryDao.find(q -> q.byIds(uuids));
	}

	/**
	 * This method groups variables by their module.
	 * It iterates through the variables and checks if they are defined in a copybook.
	 * If a variable is defined in a copybook, it resolves all including modules and adds the variable to the list of variables for each including module.
	 * If a variable is not defined in a copybook, it adds the variable to the list of variables for its module.
	 *
	 * @param context The context of the FunctionalBlockGeneration.
	 * @param variables The collection of variables to be grouped by their module.
	 * @return A map where the keys are module IDs and the values are lists of variables for each module.
	 */
	private Map<EntityId, List<DataDictionaryPojo>> groupVariablesByModule(final FunctionalBlockGenerationContext context,
			final Collection<DataDictionaryPojo> variables) {
		final Map<EntityId, List<DataDictionaryPojo>> ret = new HashMap<>();
		for (final DataDictionaryPojo variable : variables) {
			if ( variable.getDefinedLocation().map(DefinedLocation.COPYBOOK :: equals).orElse(false) ) {
				/* for variables defined in a copybook we need to resolve all including modules */
				final List<EntityId> includingModules = moduleDao.findModuleIds(
						q -> q.ofProject(context.getProjectId()).withDestinationRelationshipsTo(variable.getModule(), RelationshipType.INCLUDES));
				for (final EntityId includingModule : includingModules) {
					ret.computeIfAbsent(includingModule, k -> new ArrayList<>()).add(variable);
				}
			} else {
				ret.computeIfAbsent(variable.getModule(), k -> new ArrayList<>()).add(variable);
			}
		}
		return ret;
	}

	/**
	 * This method is used to build a DataFlowGraph for a given module and a list of start variables.
	 * It sets the parameters for the DataFlowGraph query and then calls the buildDataFlowGraph method of the dataFlowGraphQueryService.
	 *
	 * @param context The context of the FunctionalBlockGeneration.
	 * @param module The module for on the DataFlowGraph is to be built.
	 * @param startVariables The list of start variables for which the DataFlowGraph will be generated.
	 * @return The built DataFlowGraph.
	 * @throws IllegalStateException If no location is found for a business variable.
	 */
	private DataFlowGraph getDataFlowGraph(final FunctionalBlockGenerationContext context, final ModulePojo module,
			final List<DataDictionaryPojo> startVariables) {

		final Parameters.Builder parametersBuilder = new Parameters.Builder().setProjectId(context.getProjectId()).setQueryDirection(QueryDirection.BOTH)
				.setDetailLevel(DetailLevel.STATEMENT).setMaxModuleDistance(1);

		for (final DataDictionaryPojo startVariable : startVariables) {
			final Optional<ModuleLocation> location = startVariable.getLocation();
			if ( location.isEmpty() ) {
				throw new IllegalStateException("No location found for business variable " + startVariable.getName());
			}
			if ( startVariable.getDefinedLocation().map(DefinedLocation.COPYBOOK :: equals).orElse(false) ) {
				parametersBuilder.addStartField(startVariable.getModule(), location.get().getOffset(), module.identity());
			} else {
				parametersBuilder.addStartField(startVariable.getModule(), location.get().getOffset());
			}
		}

		return dataFlowGraphQueryService.buildDataFlowGraph(context.getProgressMonitor(), parametersBuilder.build());
	}

	/**
	 * This method is used to get the locations of the statements in a DataFlowGraph.
	 * It iterates through the nodes in the DataFlowGraph, filters to include only nodes of type STATEMENT,
	 * maps each node to its source location if it exists, otherwise uses its location, filters out any null locations,
	 * and collects the results into a list.
	 *
	 * @param dataFlowGraph The DataFlowGraph from which the statement locations are to be retrieved.
	 * @return A list of ModuleLocations of the statements in the DataFlowGraph.
	 */
	private List<ModuleLocation> getStatementLocations(final DataFlowGraph dataFlowGraph) {
		return dataFlowGraph.getNodes().stream().filter(node -> node.getType() == DataFlowGraphNode.Type.STATEMENT)
				.map(n -> Optional.ofNullable(n.getSourceLocation()).map(SourceLocation :: getModuleLocation).orElse(n.getLocation()))
				.filter(Objects :: nonNull).toList();
	}

	@Override
	public void persistAdditionalData(final FunctionalBlockGenerationContext context, final UUID functionalBlockId, final List<FunctionalBlockLink> data) {
		functionalBlockService.setLinks(functionalBlockId, data);
	}
}
