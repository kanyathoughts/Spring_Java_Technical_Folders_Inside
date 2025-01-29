/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.query;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.profile.DefaultProfiler;
import innowake.lib.job.api.OperationCanceledException;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.context.DataLineageContextProvider;
import innowake.mining.server.datalineage.core.DataLineageCoreService;
import innowake.mining.shared.FutureUtil;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.DataFlowService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.DataFlowError;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.SourceLocation;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;
import innowake.mining.shared.model.datalineage.graph.DataInterfaceNode;
import innowake.mining.shared.model.datalineage.graph.FieldNode;
import innowake.mining.shared.model.datalineage.graph.ModuleNode;
import innowake.mining.shared.model.datalineage.graph.StatementNode;
import innowake.ndt.fieldtracing.model.TracedModule;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang.StringUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType.READ_ACCESS;
import static innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType.RELATED_FIELD;
import static innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType.WRITE_ACCESS;
import static innowake.mining.shared.hashing.CityHash.cityHash64;

/**
 * Executes one Data Flow Graph Query. Once instance of this class can only be used to execute one query and should be disposed afterwards.
 * <p>
 * The query execution consists of five stages:
 * </p>
 * <p>
 * <b>Prepare Start Nodes:</b> The query is initially provided with {@linkplain Parameters#getStartModuleIds() a list of module ids}
 * and/or {@linkplain Parameters#getStartFields() pairs of module id and field offset}. This initial stage is responsible
 * for creating the corresponding DataFlowNodes to start the query with. On each start module we call
 * {@link DataLineageCoreService#traceModule(DataLineageContext, EntityId)}. Then, if only a module id was provided we use all generated DataFlowNodes
 * from that module as start nodes. In case a field offset was provided, we only use the DataFlowNode of the corresponding field as start node.
 * At the end of this stage we have a set of {@linkplain DataFlowNodePojo DataFlowNodePojos} that serve as start nodes of the query.
 * </p>
 * <p>
 * <b>Traverse:</b> In this stage we traverse the graph beginning with each DataFlowNode in the set of start nodes and following the links in each node's
 * {@code READ_ACCESSes}, {@code WRITE_ACCESSes} and {@code RELATED_FIELDs} lists.
 * The traversal respects the configured {@linkplain Parameters#getQueryDirection() direction}
 * and {@linkplain Parameters#getMaxModuleDistance() maximum distance}.
 * </p>
 * <p>
 * <b>Build Graph:</b> In this stage we iterate over all DataFlowNodes that were visited in the "Traverse" stage. Each node is converted into a
 * {@link DataFlowGraphNode}, then we establish the correct links between the graph nodes based on the links of the DataFlowNodes. Read and write accesses
 * of DataFlowNodes are directly translated into the corresponding incoming / outgoing links in the data flow graph. The "related fields" are more tricky.
 * Currently the resulting data flow graph does not display the "related field" relationship. Instead, read/write accesses to fields are "replicated" for
 * each related field. Let's say the statement S has a write access to field A. In the data flow graph, there will be an outgoing edge from S to A
 * and also from S <i>to each related field</i> of A. This rule is <i>not</i> applied to proxy fields.
 * See {@code #processRelatedFields(DataFlowNode, DataFlowNode, boolean) processRelatedFields()} for details.
 * </p>
 * <p>
 * <b>Adjust Detail Level:</b> The graph is initially always rendered at the highest detail level "STATEMENT". If a different detail level was requested
 * this stage adjusts the graph by removing nodes that are not to be shown at the requested detail level. The edges of a removed node are distributed
 * accordingly to the adjacent nodes.
 * </p>
 * <b>Create Parent Nodes:</b> This final stage creates artificial "Module" nodes that group all other nodes (Fields and Statements) in the data flow graph.
 */
public class DataFlowGraphQuery {

	private static final Logger LOG = LoggerFactory.getLogger(DataFlowGraphQuery.class);

	/* store at most this many parsed programs in the cache */
	private static final int FIELD_TRACER_MAX_CACHE_SIZE = 100;

	private static final String STATUS_MESSAGE_START_NODES = "Preparing start nodes (%d/%d)";
	private static final String STATUS_MESSAGE_TRAVERSE = "Computing data lineage from %d start nodes (%d nodes processed)";
	private static final String STATUS_MESSAGE_BUILD_GRAPH = "Building data flow graph (%d/%d) nodes";
	private static final String STATUS_MESSAGE_DETAIL_LEVEL = "Adjusting graph to selected detail level";
	private static final String STATUS_MESSAGE_FINALIZE_GRAPH = "Finalizing data flow graph";

	private static final String START_NODE_MODULE_LOG_MESSAGE = "StartNodes (moduleId=%s): %s";
	private static final String START_NODE_FIELD_LOG_MESSAGE = "StartNodes (moduleId=%s, offset=%d): %s";
	private static final String TRAVERSE_LOG_MESSAGE = "Traverse %s (%s): %s";
	private static final String TRAVERSE_LINK_LOG_MESSAGE = "Traverse %s (%s): %s -> %s (%s)";
	private static final String BUILD_GRAPH_LOG_MESSAGE = "BuildGraph %s (%s): %s";
	private static final String BUILD_GRAPH_LINK_OUTGOING_LOG_MESSAGE = "BuildGraph %s (%s): link -> %s (%s)";
	private static final String BUILD_GRAPH_LINK_INCOMING_LOG_MESSAGE = "BuildGraph %s (%s): link <- %s (%s)";
	private static final String BUILD_GRAPH_RELATED_LOG_MESSAGE = "BuildGraph %s (%s): related: %s (%s)";
	private static final String BUILD_GRAPH_PROXY_LOG_MESSAGE = "BuildGraph %s (%s): proxy: %s (%s) <-> %s (%s)";

	private static final Set<String> COMPUTATION_AND_TRANSFORMATION_STATEMENTS = Set.of("MOVE", "COMPUTE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "STRING",
			"UNSTRING", "FUNCTION REFERENCE");

	private final ExecutorService executor;
	private final DataFlowService dataFlowService;
	private final DataLineageContextProvider contextProvider;
	private final DataLineageCoreService coreService;
	private final ModuleService moduleService;
	private final AstService astService;

	private final ProgressMonitor progressMonitor;
	private final Parameters parameters;

	private final Cache<TracedModule<ModuleLightweightPojo>, Object> fieldTracerCache = CacheBuilder.newBuilder()
			.maximumSize(FIELD_TRACER_MAX_CACHE_SIZE)
			.build();

	private final Cache<Long, List<TracedModule<ModuleLightweightPojo>>> incomingDependenciesCache = CacheBuilder.newBuilder()
			.maximumSize(FIELD_TRACER_MAX_CACHE_SIZE)
			.build();

	private final Cache<Long, List<TracedModule<ModuleLightweightPojo>>> outgoingDependenciesCache = CacheBuilder.newBuilder()
			.maximumSize(FIELD_TRACER_MAX_CACHE_SIZE)
			.build();

	/* queue of futures of async operations */
	private final BlockingQueue<Future<?>> futures = new LinkedBlockingQueue<>();

	private final Set<UUID> startNodeUids = new HashSet<>();
	/* contains uuids of nodes already visited during traversing */
	private final Set<UUID> traversedNodes = new HashSet<>();
	/* map of data flow node uuid to "module distance", containing the number of modules between the node and the start field during upstream traversal */
	private final Map<UUID, Integer> upstreamTraversalDistances = new ConcurrentHashMap<>();
	/* map of data flow node uuid to "module distance", containing the number of modules between the node and the start field during downstream traversal */
	private final Map<UUID, Integer> downstreamTraversalDistances = new ConcurrentHashMap<>();
	/* contains the produced DataFlowGraphNodes, using the uuid of the corresponding DataFlowNode as key */
	private final Map<UUID, DataFlowGraphNode> graphNodes = new ConcurrentHashMap<>();
	/* same as graphNodes but using the id of the DataFlowGraphNode as key */
	private final Map<String, DataFlowGraphNode> graphNodesById = new ConcurrentHashMap<>();

	private volatile QueryDirection currentDirection = QueryDirection.BOTH;

	/**
	 * Construct new DataFlowGraphQuery
	 *
	 * @param executor executor service for async operations
	 * @param dataFlowService service for accessing data flow entities
	 * @param contextProvider provider {@link DataLineageContext} instances
	 * @param coreService service for tracing the data flow in a module
	 * @param moduleService for providing additional module details in the result
	 * @param astService service for accessing AST entities
	 * @param progressMonitor a progress monitor to report progress and cancel the query
	 * @param parameters the query parameters
	 */
	public DataFlowGraphQuery(final ExecutorService executor,
							  final DataFlowService dataFlowService,
							  final DataLineageContextProvider contextProvider,
							  final DataLineageCoreService coreService,
							  final ModuleService moduleService,
							  final AstService astService,
							  final ProgressMonitor progressMonitor,
							  final Parameters parameters) {
		this.executor = executor;
		this.dataFlowService = dataFlowService;
		this.contextProvider = contextProvider;
		this.coreService = coreService;
		this.moduleService = moduleService;
		this.astService = astService;
		this.progressMonitor = progressMonitor;
		this.parameters = parameters;
	}

	public DataFlowGraph execute() {
		prepareStartNodes();

		if (parameters.getQueryDirection() == QueryDirection.BOTH) {
			/* in case of QueryDirection.BOTH - do first an UPSTREAM traversal followed by DOWNSTREAM traversal */
			currentDirection = QueryDirection.UPSTREAM;
			traverse();
			currentDirection = QueryDirection.DOWNSTREAM;
			traverse();
			/* reset direction to BOTH, otherwise buildGraph() only builds half the graph */
			currentDirection = QueryDirection.BOTH;
		} else {
			/* only one traversal in the specified direction is necessary */
			currentDirection = parameters.getQueryDirection();
			traverse();
		}

		/* add all nodes visited during traversal to traversedNodes */
		traversedNodes.addAll(upstreamTraversalDistances.keySet());
		traversedNodes.addAll(downstreamTraversalDistances.keySet());

		buildGraph();

		final List<DataFlowGraphNode> adjustedNodes = adjustedLevelNodes();
		final List<ModuleNode> parentNodes = createModuleNodes(adjustedNodes);

		return new DataFlowGraph(ListUtils.union(adjustedNodes, parentNodes));
	}

	private void prepareStartNodes() {
		final int total = parameters.getStartModuleIds().size() + parameters.getStartFields().size();
		int worked = 0;

		progressMonitor.setStepDescription(String.format(STATUS_MESSAGE_START_NODES, worked, total));
		for (final EntityId startModuleId : parameters.getStartModuleIds()) {
			final Optional<ModuleLightweightPojo> module = moduleService.findAnyModuleLightweight(q -> q.byId(startModuleId));
			if (module.isEmpty()) {
				throw new IllegalArgumentException("Start module with id not found: " + startModuleId);
			}

			final List<DataFlowNodePojo> nodesInModule = traceStartModule(module.get());

			LOG.trace(() -> String.format(START_NODE_MODULE_LOG_MESSAGE, startModuleId, "found " + nodesInModule.size() + " start nodes: " + nodesInModule));
			startNodeUids.addAll(nodesInModule.stream().map(DataFlowNodePojo::getId).collect(Collectors.toList()));
			progressMonitor.setStepDescription(String.format(STATUS_MESSAGE_START_NODES, ++worked, total));
		}

		for (final SelectedField startField : parameters.getStartFields()) {
			final Optional<ModuleLightweightPojo> module = moduleService.findAnyModuleLightweight(q -> q.byId(startField.getModule()));
			if ( ! module.isPresent()) {
				throw new IllegalArgumentException("Start module with id not found: " + startField.getModule());
			}

			final List<DataFlowNodePojo> nodesForField = traceStartField(module.get(), startField);

			LOG.trace(() -> String.format(START_NODE_FIELD_LOG_MESSAGE, startField.getModule(), startField.getOffset(), "found " + nodesForField.size() + " start nodes: " + nodesForField));
			startNodeUids.addAll(nodesForField.stream().map(DataFlowNodePojo::getId).collect(Collectors.toList()));
			progressMonitor.setStepDescription(String.format(STATUS_MESSAGE_START_NODES, ++worked, total));
		}
		if (parameters.getContainerType() == ProxyContainerPojo.Type.DATABASE_TABLE && ! parameters.getSelectedTableColumnName().isBlank()) {
			final var proxyContainers = coreService.getProxyContainersOfType(contextProvider.createContext(parameters.getProjectId()),
																			parameters.getStartModuleIds().get(0),
																			ProxyContainerPojo.Type.DATABASE_TABLE);
			if ( ! proxyContainers.isEmpty()) {
				proxyContainers.iterator().next().getFieldNodes().stream()
						.filter(fieldNode -> fieldNode.getName().equals(parameters.getSelectedTableColumnName()))
						.findFirst()
						.ifPresent(node -> startNodeUids.add(node.getId()));

			}
		}
	}

	private List<DataFlowNodePojo> traceStartModule(final ModuleLightweightPojo module) {
		LOG.trace(() -> String.format(START_NODE_MODULE_LOG_MESSAGE, module.getId(), "tracing all in Module " + module.getId()));
		try (final DataLineageContext context = contextProvider.createContext(parameters.getProjectId(), progressMonitor, fieldTracerCache,
				incomingDependenciesCache, outgoingDependenciesCache)) {
			
			if (module.getType() == Type.COPYBOOK) {
				final List<Long> moduleIds = moduleService.getSrcModuleIdsByRelationshipTypeAndDstId(RelationshipType.INCLUDES, module.identity().getUid());
				for (final Long cobolModuleId: moduleIds) {
					coreService.traceModule(context, EntityId.of(cobolModuleId));
				}
				return dataFlowService.find(q -> q.withInclusionCalleeModuleId(module.identity()));
			} else {
				coreService.traceModule(context, EntityId.of(module.getId()));
				return dataFlowService.find(q -> q.ofModule(module.identity()));
			}
		}
	}

	private List<DataFlowNodePojo> traceStartField(final ModuleLightweightPojo module, final SelectedField startField) {
		final int fieldOffset = startField.getOffset();
		final Optional<EntityId> includingModuleId = startField.getIncludingModule();
		final boolean isAssembled = parameters.isAssembled();
		if (includingModuleId.isPresent()) {
			return traceCopybookFieldFromCobolModule(module, fieldOffset, includingModuleId.get());
		} else {
			traceStartModule(module);
			if (module.getType() == Type.COPYBOOK) {
				return traceCopybookField(module, fieldOffset);
			} else if (isAssembled) {
				return traceFieldInAssembledMode(module, fieldOffset);
			} else {
				final Optional<Integer> resolvedOffset = resolveFieldReference(module, fieldOffset);
				if (resolvedOffset.isPresent()) {
					/* resolvedOffset is the _assembled_ offset, so that it doesn't matter whether the field definition is in a copybook or not */
					return dataFlowService.find(q -> q.ofModule(module.identity())
														.withAssembledOffset(Comperator.LESSER_OR_EQUAL, resolvedOffset.get())
														.withAssembledEndOffset(Comperator.GREATER, resolvedOffset.get()));
				} else {
					/* but the provided fieldOffset is the retraced location */
					return dataFlowService.find(q -> q.ofModule(module.identity())
														.withRetracedOffset(Comperator.LESSER_OR_EQUAL, fieldOffset)
														.withRetracedEndOffset(Comperator.GREATER, fieldOffset));
				}
			}
		}
	}

	private List<DataFlowNodePojo> traceCopybookField(final ModuleLightweightPojo module, final Integer fieldOffset) {
		final Optional<Integer> resolvedOffset = resolveFieldReferenceInCopybook(module, fieldOffset);
		if (resolvedOffset.isPresent()) {
			return dataFlowService.find(q -> q.withInclusionCalleeModuleId(module.identity())
												.withAssembledOffset(Comperator.LESSER_OR_EQUAL, resolvedOffset.get())
												.withAssembledEndOffset(Comperator.GREATER, resolvedOffset.get()));
		} else {
			return dataFlowService.find(q -> q.withInclusionCalleeModuleId(module.identity())
												.withRetracedOffset(Comperator.LESSER_OR_EQUAL, fieldOffset)
												.withRetracedEndOffset(Comperator.GREATER, fieldOffset));
		}
	}

	private List<DataFlowNodePojo> traceFieldInAssembledMode(final ModuleLightweightPojo module, final Integer fieldOffset) {
		final Integer offset = resolveFieldReferenceForAssembledMode(module, fieldOffset).orElse(fieldOffset);
		return dataFlowService.find(q -> q.ofModule(module.identity())
											.withAssembledOffset(Comperator.LESSER_OR_EQUAL, offset)
											.withAssembledEndOffset(Comperator.GREATER, offset));
	}

	private List<DataFlowNodePojo> traceCopybookFieldFromCobolModule(final ModuleLightweightPojo module, final Integer fieldOffset,
			final EntityId includingModuleId) {
		final ModulePojo includingModule  = moduleService.findAnyModule(q -> q.ofProject(parameters.getProjectId())
				.byId(includingModuleId))
				.orElseThrow(() -> new IllegalArgumentException("Including module with id " + includingModuleId + " not found."));
		final List<Long> moduleIds = moduleService.getSrcModuleIdsByRelationshipTypeAndDstId(RelationshipType.INCLUDES, module.identity().getUid());
		final Long includedModuleId = includingModule.getId();
		if (moduleIds.contains(includedModuleId)) {
			try (final DataLineageContext context = contextProvider.createContext(parameters.getProjectId(), progressMonitor, fieldTracerCache,
					incomingDependenciesCache, outgoingDependenciesCache)) {
				coreService.traceModule(context, EntityId.of(includedModuleId));
				final EntityId includedModuleEid = EntityId.of(includedModuleId);
				return dataFlowService.find(q -> q.withInclusionCalleeModuleId(module.identity())
													.ofModule(includedModuleEid)
													.withRetracedOffset(Comperator.LESSER_OR_EQUAL, fieldOffset)
													.withRetracedEndOffset(Comperator.GREATER, fieldOffset));
			}
		}
		throw new IllegalArgumentException("Module with id " + module.getId() + " not found.");
	}

	private Optional<Integer> resolveFieldReferenceForAssembledMode(final ModuleLightweightPojo module, final Integer fieldOffset) {
		return astService.find(q -> q.ofModule(module.identity())
										.withAssembledOffset(Comperator.LESSER_OR_EQUAL, fieldOffset)
										.withAssembledEndOffset(Comperator.GREATER, fieldOffset)
										.withSuperTypes(AstNodeUtils.FIELD_REFERENCE))
								.stream().findAny()
								.flatMap(node -> this.resolveFieldReference(node, fieldOffset, module.getName()));
	}

	private Optional<Integer> resolveFieldReference(final ModuleLightweightPojo module, final Integer fieldOffset) {
		return astService.find(q -> q.ofModule(module.identity())
										.withRetracedOffset(Comperator.LESSER_OR_EQUAL, fieldOffset)
										.withRetracedEndOffset(Comperator.GREATER, fieldOffset)
										.withSuperTypes(AstNodeUtils.FIELD_REFERENCE))
								.stream().findAny()
								.flatMap(node -> this.resolveFieldReference(node, fieldOffset, module.getName()));
	}

	private Optional<Integer> resolveFieldReferenceInCopybook(final ModuleLightweightPojo module, final Integer fieldOffset) {
		return astService.findAny(q -> q.withIncludedModule(module.identity())
						.withRetracedOffset(Comperator.LESSER_OR_EQUAL, fieldOffset)
						.withRetracedEndOffset(Comperator.GREATER, fieldOffset)
						.withSuperTypes(AstNodeUtils.FIELD_REFERENCE))
				.flatMap(node -> this.resolveFieldReference(node, fieldOffset, module.getName()));
	}

	private Optional<Integer> resolveFieldReference(final AstNodePojo fieldReference, final Integer fieldOffset, final String moduleName) {
		final Collection<AstRelationshipPojo> outAstBinding = fieldReference.getOutgoingRelations().stream()
																			.filter(r -> r.getType() == AstRelationshipType.BINDING)
																			.collect(Collectors.toList());
		if (outAstBinding.isEmpty()) {
			throw new IllegalArgumentException("No field to trace found in Module " + moduleName + " at offset " + fieldOffset);
		}

		final Optional<AstNodePojo> fieldDefinition = outAstBinding.stream()
				.filter(astBinding -> "definedBy".equals(astBinding.getLabel().orElse(null)))
				.findAny()
				.flatMap(binding -> Optional.ofNullable(binding.getDstNode()));

		return fieldDefinition.flatMap(fd -> fd.getLocation().getAssembledOffset());
	}

	private void traverse() {
		progressMonitor.setStepDescription(String.format(STATUS_MESSAGE_TRAVERSE, startNodeUids.size(), traversedNodes.size()));
		try {
			for (final var startNodeUid : startNodeUids) {
				traverseNode(startNodeUid, 0, parameters.getMaxRecursionDepth(), true);
			}
			FutureUtil.awaitAll(futures);
		} catch (final ExecutionException e) {
			throw new IllegalStateException(e);
		} catch (final InterruptedException e) {
			LOG.error("Interrupted while executing data flow graph query", e);
			Thread.currentThread().interrupt();
			throw new OperationCanceledException();
		} finally {
			futures.clear();
		}
	}

	private void traverseNode(final UUID nodeId, final int currentModuleDistance, final int remainingRecursionDepth, final boolean processRelatedFields) {
		if (currentModuleDistance - parameters.getMaxModuleDistance() == 0) {
			return;
		}
		if (remainingRecursionDepth == 0) {
			return;
		}

		futures.add(executor.submit(() -> {
			if (isAlreadyVisited(currentModuleDistance, nodeId)) {
				return;
			}

			progressMonitor.setStepDescription(String.format(STATUS_MESSAGE_TRAVERSE, startNodeUids.size(), traversedNodes.size()));
			
			final DataFlowNodePojo dfNodeBeforeTrace = dataFlowService.findAny(q -> q.byId(nodeId))
					.orElseThrow(() -> new IllegalStateException("While traversing DataFlowNodePojo: " + nodeId + ": The DataFlowNodePojo does not exist"));
			LOG.trace(() -> String.format(TRAVERSE_LOG_MESSAGE, dfNodeBeforeTrace.getId(), dfNodeBeforeTrace.getName(),
					"begin - already traced: " + dfNodeBeforeTrace.isTraced()));
			final DataFlowNodePojo dfNode = executeTraceNodes(dfNodeBeforeTrace, nodeId);
			
			LOG.trace(() -> String.format(TRAVERSE_LOG_MESSAGE, dfNode.getId(), dfNode.getName(), "node: " + dfNode));
			if ( ! dfNode.isTraced()) {
				final List<DataFlowErrorPojo> errors = dataFlowService.findErrors(q -> q.ofNode(dfNode.getId()));
				if (parameters.isStrictTracing() && errors.isEmpty()) {
					/* fail fast if node is still not marked as traced=true after calling traceNode() */
					throw new IllegalStateException("While traversing DataFlowNode " + nodeId + ": Node still not traced after call to traceNode()");
				}
				if ( ! errors.isEmpty()) {
					LOG.warn(() -> "DataFlowNode could not be traced due to:" + errors);
				}
			}
			traverseLinks(dfNode, currentModuleDistance, remainingRecursionDepth, processRelatedFields);
		}));
	}

	private String getStatementLabel(final DataFlowNodePojo dfNode) {
		if (dfNode.getType() == DataFlowNodePojo.Type.STATEMENT && COMPUTATION_AND_TRANSFORMATION_STATEMENTS.contains(dfNode.getName().toUpperCase())) {
			final Integer offset = dfNode.getLocation()
											.orElseThrow(() -> new IllegalStateException("Location must not be null in data flow node: " + dfNode))
											.getOffset();
			final var astNodes = astService.find(q -> q.ofModule(dfNode.getModuleId()).withAssembledOffset(Comperator.EQUAL, offset));
			if ( ! astNodes.isEmpty()) {
				return astNodes.get(0).getLabel();
			}
		}

		return "";
	}

	private DataFlowNodePojo executeTraceNodes(final DataFlowNodePojo dfNodeBeforeTrace, final UUID nodeId) {
		if ( ! dfNodeBeforeTrace.isTraced()) {
			traceNode(dfNodeBeforeTrace);
			return dataFlowService.findAny(q -> q.byId(nodeId))
					.orElseThrow(() -> new IllegalStateException("While traversing DataFlowNodePojo " + nodeId + ": DataFlowNodePojo does not exist"));
		} else {
			return dfNodeBeforeTrace;
		}
	}
	
	/**
	 * Checks whether a node was already visited at or at less than a certain distance during upstream and downstream
	 * traversal.
	 *
	 * @param currentModuleDistance the current module distance
	 * @param nodeId {@link UUID} of node we want to check if it was previously visited
	 * @return true if the node was visited at or at less than the current distance; false if not.
	 */
	private boolean isAlreadyVisited(final Integer currentModuleDistance, final UUID nodeId) {
		final Map<UUID, Integer> distanceMap;
		if (currentDirection == QueryDirection.UPSTREAM) {
			distanceMap = upstreamTraversalDistances;
		} else {
			distanceMap = downstreamTraversalDistances;
		}
		final Integer previous = distanceMap.putIfAbsent(nodeId, currentModuleDistance);
		if (previous != null) {
			if (previous <= currentModuleDistance) {
				/* node was already visited during traversal */
				return true;
			} else {
				distanceMap.put(nodeId, currentModuleDistance);
			}
		}
		return false;
	}

	private void traceNode(final DataFlowNodePojo dfNode) {
		LOG.trace(() -> String.format(TRAVERSE_LOG_MESSAGE, dfNode.getId(), dfNode.getName(), "tracing"));
		try (final DataLineageContext context = contextProvider.createContext(parameters.getProjectId(), progressMonitor, fieldTracerCache,
				incomingDependenciesCache, outgoingDependenciesCache)) {
			if (dfNode.getType() == DataFlowNodePojo.Type.PROXY_FIELD) {
				final DataFlowId dataFlowId = dfNode.getProxyContainer()
										.orElseThrow(() -> new IllegalStateException("Data flow node must have a proxy container: " + dfNode))
										.getDataFlowId();
				coreService.resolveProxyContainer(context, dfNode.getModuleId(), dataFlowId);
			}
			coreService.traceModule(context, dfNode.getModuleId());
		} catch (final Exception e) {
			LOG.error(() -> "While tracing DataFlowNode " + dfNode.getId() + ": unhandled exception", e);
			dataFlowService.createError(new DataFlowErrorPojoPrototype()
											.setNodeId(dfNode.getId())
											.setSeverity(DataFlowErrorPojo.Severity.ERROR)
											.setText(StringUtils.trimToEmpty(e.getMessage())));
		} finally {
			if (DefaultProfiler.isProfilingEnabled()) {
				ProfilingFactory.getProfilingSession().flushCurrentThread();
			}
		}
		LOG.trace(() -> String.format(TRAVERSE_LOG_MESSAGE, dfNode.getId(), dfNode.getName(), "trace complete"));
	}

	private void traverseLinks(final DataFlowNodePojo dfNode, final int currentModuleDistance, final int remainingRecursionDepth,
			final boolean processRelatedFields) {

		/* For Proxy Fields, the related fields are always processed, even recursively. */
		if (processRelatedFields || dfNode.getType() == DataFlowNodePojo.Type.PROXY_FIELD) {
			final var relatedNodes = dataFlowService.find(q -> q.withRelationshipFrom(dfNode.getId(), RELATED_FIELD));
			for (final DataFlowNodePojo relatedField : relatedNodes) {
				LOG.trace(() -> String.format(TRAVERSE_LINK_LOG_MESSAGE, dfNode.getId(), dfNode.getName(),
						"related", relatedField.getId(), relatedField.getName()));
				/* the remainingRecursionDepth is not decremented for related fields - they are treated as "siblings", belonging to the same recursion depth */
				/* setting "processRelatedFields" false to avoid recursive traversal of related fields ("related field" relationship is not transitive)
				 *  except for Proxy Fields. */
				traverseNode(relatedField.getId(), getNextModuleDistance(dfNode, relatedField, currentModuleDistance),
						remainingRecursionDepth, dfNode.getType() == DataFlowNodePojo.Type.PROXY_FIELD);
			}
		}

		if (shouldFollowReadAccesses(dfNode.getType())) {
			final var readAccessNodes = dataFlowService.find(q -> q.withRelationshipFrom(dfNode.getId(), READ_ACCESS));
			for (final DataFlowNodePojo readAccess : readAccessNodes) {
				LOG.trace(() -> String.format(TRAVERSE_LINK_LOG_MESSAGE, dfNode.getId(), dfNode.getName(),
						"read access", readAccess.getId(), readAccess.getName()));
				traverseNode(readAccess.getId(), getNextModuleDistance(dfNode, readAccess, currentModuleDistance),
						remainingRecursionDepth - 1, true);
			}
		}

		if (shouldFollowWriteAccesses(dfNode.getType())) {
			final var writeAccessNodes = dataFlowService.find(q -> q.withRelationshipFrom(dfNode.getId(), WRITE_ACCESS));
			for (final DataFlowNodePojo writeAccess : writeAccessNodes) {
				LOG.trace(() -> String.format(TRAVERSE_LINK_LOG_MESSAGE, dfNode.getId(), dfNode.getName(),
						"write access", writeAccess.getId(), writeAccess.getName()));
				traverseNode(writeAccess.getId(), getNextModuleDistance(dfNode, writeAccess, currentModuleDistance),
						remainingRecursionDepth - 1, true);
			}
		}
	}

	private void buildGraph() {
		progressMonitor.setStepDescription(String.format(STATUS_MESSAGE_BUILD_GRAPH, 0, traversedNodes.size()));

		final AtomicInteger processed = new AtomicInteger();
		final var moduleCache = createModuleNameCache();
		for (final var nodeId: traversedNodes) {
			futures.add(executor.submit(() -> {
				processNode(nodeId, moduleCache);
				progressMonitor.setStepDescription(String.format(STATUS_MESSAGE_BUILD_GRAPH, processed.incrementAndGet(), traversedNodes.size()));
			}));
		}

		try {
			FutureUtil.awaitAll(futures);
		} catch (final ExecutionException e) {
			throw new IllegalStateException(e);
		} catch (final InterruptedException e) {
			LOG.error("Interrupted while executing data flow graph query", e);
			Thread.currentThread().interrupt();
			throw new OperationCanceledException();
		} finally {
			futures.clear();
		}
	}

	private void processNode(final UUID nodeId, final LoadingCache<UUID, String> moduleCache) {
		final DataFlowNodePojo dfNode = dataFlowService.findAny(q -> q.byId(nodeId))
				.orElseThrow(() -> new IllegalStateException("While processing DataFlowNodePojo " + nodeId + ": DataFlowNodePojo does not exist"));

		/* Needs to be removed once we support Exec Cics */
		if (dfNode.getName().contains("Exec Cics")) {
			return;
		}

		if ( ! dfNode.isTraced()) {
			final List<DataFlowErrorPojo> errors = dataFlowService.findErrors(q -> q.ofNode(nodeId));
			if (parameters.isStrictTracing() && errors.isEmpty()) {
				dataFlowService.createError(new DataFlowErrorPojoPrototype()
												.setNodeId(dfNode.getId())
												.setSeverity(DataFlowErrorPojo.Severity.ERROR)
												.setText("The node could not be traced, for unknown reason"));
			}
			if ( ! errors.isEmpty()) {
				LOG.warn(() -> "DataFlowNode could not be traced due to:" + errors);
			}
		}

		/* add node to the graph only if it is a PROXY_FIELD, or it has a read/write access from/to at least one traversed field */
		if (dfNode.getType().equals(DataFlowNodePojo.Type.PROXY_FIELD) || containsRelationsToTraversedNodes(dfNode.getId())) {
			final DataFlowGraphNode graphNode = graphNodes.computeIfAbsent(nodeId, rid -> createGraphNode(dfNode, moduleCache));
			LOG.trace(() -> {
				/* synchronized is required here, in case the node is concurrently being modified by linkNodes() */
				synchronized (graphNode) {
					return String.format(BUILD_GRAPH_LOG_MESSAGE, nodeId, dfNode.getName(), "created graph node: " + graphNode);
				}
			});

			processLinks(dfNode, moduleCache);
		}
	}

	private DataFlowGraphNode createGraphNode(final DataFlowNodePojo dfNode, final LoadingCache<UUID, String> moduleCache) {
		final DataFlowGraphNode node;
		switch (dfNode.getType()) {
			case FIELD: {
				node = new FieldNode(getIdForDataFlowNode(dfNode), dfNode.getName(), dfNode.getLocation().orElse(null));
				break;
			}
			case STATEMENT: {
				node = new StatementNode(getIdForDataFlowNode(dfNode), dfNode.getName(), dfNode.getLocation().orElse(null));
				break;
			}
			case PROXY_FIELD: {
				node = new DataInterfaceNode(getIdForDataFlowNode(dfNode), dfNode.getName(), dfNode.getLocation().orElse(null));
				break;
			}
			default:
				throw new IllegalStateException("unhandled DataFlowNodePojo Type " + dfNode.getType());
		}

		if (dfNode.getAstNodeId().isPresent()) {
			final AstNodePojo astNode = dfNode.getAstNode().orElse(null);
			if (astNode != null) {
				final var sourceModuleId = astNode.getIncludedModule().orElse(astNode.getModule());
				final var astLocation = astNode.getLocation();
				final var sourceLocation = new ModuleLocation(astLocation.getRetracedOffset().orElse(0), astLocation.getRetracedLength().orElse(0));
				node.setSourceLocation(new SourceLocation(sourceModuleId.getNid(), moduleCache.getUnchecked(sourceModuleId.getUid()), sourceLocation));
			}
		}
		final var nodeId = dfNode.getId();
		final boolean isUpstream = upstreamTraversalDistances.containsKey(nodeId);
		final boolean isDownstream = downstreamTraversalDistances.containsKey(nodeId);
		if (isUpstream && isDownstream) {
			node.setDirection(DataFlowGraphNode.NodeDirection.BOTH);
		} else if (isUpstream) {
			node.setDirection(DataFlowGraphNode.NodeDirection.INCOMING);
		} else if (isDownstream) {
			node.setDirection(DataFlowGraphNode.NodeDirection.OUTGOING);
		}

		node.setStatementLabel(getStatementLabel(dfNode));
		node.setParentModule(getIdForModuleNode(dfNode.getModuleId()));
		node.setErrors(dataFlowService.findErrors(q -> q.ofNode(dfNode.getId())).stream()
														.map(e -> new DataFlowError(e.getSeverity(), e.getText()))
														.collect(Collectors.toSet()));
		graphNodesById.put(node.getId(), node);

		return node;
	}

	private void processLinks(final DataFlowNodePojo dfNode, final LoadingCache<UUID, String> moduleCache) {
		if (shouldFollowReadAccesses(dfNode.getType())) {
			final var readAccesses = dataFlowService.find(q -> q.withRelationshipFrom(dfNode.getId(), READ_ACCESS));
			for (final DataFlowNodePojo otherNode : onlyTraversedNodes(readAccesses)) {
				linkNodes(dfNode, otherNode, dfNode.getType() != DataFlowNodePojo.Type.STATEMENT, true, moduleCache);
			}
		}
		if (shouldFollowWriteAccesses(dfNode.getType())) {
			final var writeAccesses = dataFlowService.find(q -> q.withRelationshipFrom(dfNode.getId(), WRITE_ACCESS));
			for (final DataFlowNodePojo otherNode : onlyTraversedNodes(writeAccesses)) {
				linkNodes(dfNode, otherNode, dfNode.getType() == DataFlowNodePojo.Type.STATEMENT, true, moduleCache);
			}
		}
	}

	private void linkNodes(final DataFlowNodePojo first, final DataFlowNodePojo second, final boolean isOutgoing, final boolean processRelated,
			final LoadingCache<UUID, String> moduleCache) {
		final DataFlowGraphNode firstGraphNode = graphNodes.computeIfAbsent(first.getId(), rid -> createGraphNode(first, moduleCache));
		final DataFlowGraphNode secondGraphNode = graphNodes.computeIfAbsent(second.getId(), rid -> createGraphNode(second, moduleCache));

		/* We need to lock both the current and the previous node in order to link the two.
		 * Avoid deadlock by using deterministic lock order. */
		final Object firstLock;
		final Object secondLock;
		if (firstGraphNode.getId().compareTo(secondGraphNode.getId()) < 0) {
			firstLock = firstGraphNode;
			secondLock = secondGraphNode;
		} else {
			firstLock = secondGraphNode;
			secondLock = firstGraphNode;
		}

		synchronized(firstLock) {
			synchronized (secondLock) {
				if (isOutgoing) {
					LOG.trace(() -> String.format(BUILD_GRAPH_LINK_OUTGOING_LOG_MESSAGE, first.getId(), first.getName(), second.getId(), second.getName()));
					firstGraphNode.getOutgoings().add(secondGraphNode.getId());
					secondGraphNode.getIncomings().add(firstGraphNode.getId());
				} else {
					LOG.trace(() -> String.format(BUILD_GRAPH_LINK_INCOMING_LOG_MESSAGE, first.getId(), first.getName(), second.getId(), second.getName()));
					firstGraphNode.getIncomings().add(secondGraphNode.getId());
					secondGraphNode.getOutgoings().add(firstGraphNode.getId());
				}
			}
		}

		if (processRelated) {
			processRelatedFields(first, second, isOutgoing, moduleCache);
		}
	}

	private Collection<DataFlowNodePojo> onlyTraversedNodes(final Collection<DataFlowNodePojo> nodes) {
		return nodes.stream().filter(node -> traversedNodes.contains(node.getId())).collect(Collectors.toMap(DataFlowNodePojo::getId, UnaryOperator.identity())).values();
	}

	private boolean containsRelationsToTraversedNodes(final UUID node) {
		return dataFlowService.findIds(q -> q.withRelationshipFrom(node, READ_ACCESS, WRITE_ACCESS)).stream()
								.anyMatch(traversedNodes::contains);
	}

	private void processRelatedFields(final DataFlowNodePojo first, final DataFlowNodePojo second, final boolean isOutgoing, final LoadingCache<UUID, String> moduleCache) {
		final var relatedFields = dataFlowService.find(q -> q.withRelationshipFrom(second.getId(), RELATED_FIELD));
		for (final DataFlowNodePojo relatedNode : relatedFields) {
			/* "Weird" Special Handling of Proxy Fields:
			 * In the DB Proxy Fields are modeled as being "related to" other fields and other proxy fields, so it looks something like this:
			 * [Statement] --write--> [Proxy1] --related-- [Proxy2] --related-- [Field]
			 *
			 * For normal fields we would "resolve" the related fields, meaning that in the resulting DataFlowGraph
			 * [Statement] would have a direct write access to [Proxy1], [Proxy2] and [Field].
			 *
			 * But instead, we want it to look like this in the Graph:
			 * [Statement] --write--> [Proxy1] --write--> [Proxy2] --write--> [Field]
			 *
			 * Also, for regular fields the "related fields" are not resolved recursively, but for proxy fields we must do so. */
			if (second.getType() == DataFlowNodePojo.Type.PROXY_FIELD || relatedNode.getType() == DataFlowNodePojo.Type.PROXY_FIELD) {
				/* we must not "walk around in circles" between the proxy fields of two modules.
				 * This check enforces that we can only walk in one direction */
				if ( ! first.getModuleId().equals(relatedNode.getModuleId()) && traversedNodes.contains(relatedNode.getId())) {
					LOG.trace(() -> String.format(BUILD_GRAPH_PROXY_LOG_MESSAGE, first.getId(), first.getName(), second.getId(), second.getName(),
							relatedNode.getId(), relatedNode.getName()));
					linkNodes(second, relatedNode, isOutgoing, true, moduleCache);
				}
			} else {
				/* If the related field is used then it gets linked to the field and the related fields are not processed recursively. */
				LOG.trace(() -> String.format(BUILD_GRAPH_RELATED_LOG_MESSAGE, first.getId(), first.getName(), relatedNode.getId(), relatedNode.getName()));
				if (containsRelationsToTraversedNodes(relatedNode.getId())) {
					linkNodes(second, relatedNode, isOutgoing, false, moduleCache);
				}
			}
		}
	}

	/**
	 * Adjusts the graph to the detail-level. Only includes nodes that are included in the detail-level and redirects edges accordingly.
	 *
	 * @return List containing only nodes included in the detail-level with adjusted incoming and outgoing edges
	 */
	private List<DataFlowGraphNode> adjustedLevelNodes() {
		progressMonitor.setStepDescription(STATUS_MESSAGE_DETAIL_LEVEL);
		/* We do not need to touch anything if the level is STATEMENT since all nodes are included anyway */
		if (parameters.getDetailLevel() == DetailLevel.STATEMENT) {
			return new ArrayList<>(graphNodes.values());
		}

		final ArrayList<DataFlowGraphNode> adjustedList = new ArrayList<>();
		
		/* Sorting this byType in order to iterate Statement type nodes in the end because while rearranging the edges for statement nodes link to the field
		 * nodes are deleted from Statement node but not from the field node which creates an incorrect graph sometimes. */
		for (final DataFlowGraphNode node : graphNodes.values().stream().sorted(Comparator.comparing(DataFlowGraphNode::getType))
				.collect(Collectors.toList())) {
			final DataFlowGraphNode.Type type = node.getType();
			switch (parameters.getDetailLevel()) {
				case MODULE:
					if (type == DataFlowGraphNode.Type.DATA_INTERFACE || type == DataFlowGraphNode.Type.MODULE) {
						adjustedList.add(node);
					} else {
						rearrangeEdges(node);
					}
					break;
				case FIELD:
					if (type == DataFlowGraphNode.Type.DATA_INTERFACE || type == DataFlowGraphNode.Type.MODULE || type == DataFlowGraphNode.Type.FIELD) {
						adjustedList.add(node);
					} else {
						rearrangeEdges(node);
					}
					break;
				default:
					break;
			}
		}
		return adjustedList;
	}

	/**
	 * This method deletes a node from the list and reconnects the incoming edges with the outgoing edges.
	 * Circular edges between 2 nodes are ignored.
	 *
	 * @param node node supposed to be deleted
	 */
	private void rearrangeEdges(final DataFlowGraphNode node) {
		for (final String out : node.getOutgoings()) {
			/* Gets the node corresponding to the name */
			final DataFlowGraphNode outNode = graphNodesById.get(out);
			node.getIncomings().stream()
					.filter(n -> ! node.getOutgoings().contains(n))	/* Ignore circular edges between 2 nodes */
					.forEach(outNode::addIncoming);
			outNode.getIncomings().remove(node.getId());
		}

		for (final String in : node.getIncomings()) {
			/* Gets the node corresponding to the name */
			final DataFlowGraphNode inNode = graphNodesById.get(in);
			node.getOutgoings().stream()
					.filter(n -> ! node.getIncomings().contains(n))
					.forEach(inNode::addOutgoing);
			inNode.getOutgoings().remove(node.getId());
		}
	}

	private List<ModuleNode> createModuleNodes(final List<DataFlowGraphNode> nodes) {
		progressMonitor.setStepDescription(STATUS_MESSAGE_FINALIZE_GRAPH);
		final ConcurrentMap<String, List<DataFlowGraphNode>> nodesByParent = nodes.parallelStream()
				.collect(Collectors.groupingByConcurrent(DataFlowGraphNode::getParentModule));
		return nodesByParent.entrySet().stream()
				.map(entry -> {
					final ModuleNode moduleNode = createModuleNode(Long.valueOf(entry.getKey().split("-")[1]));
					moduleNode.setChildren(entry.getValue().stream()
									.filter(childNode -> childNode.getType() != DataFlowGraphNode.Type.DATA_INTERFACE)
									.map(DataFlowGraphNode::getId)
									.collect(Collectors.toSet()));
					moduleNode.setDataInterfaces(entry.getValue().stream()
							.filter(childNode -> childNode.getType() == DataFlowGraphNode.Type.DATA_INTERFACE)
							.map(DataFlowGraphNode::getId)
							.collect(Collectors.toSet()));
					return moduleNode;
				})
				.collect(Collectors.toList());
	}

	private ModuleNode createModuleNode(final Long moduleId) {
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(b -> b.byNid(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException("Unable to load module with numeric id: " + moduleId));
		return new ModuleNode(getIdForModuleNode(module.identity()), moduleId, module.getName(), new ModuleLocation(0,0));
	}

	private int getNextModuleDistance(final DataFlowNodePojo currentNode, final DataFlowNodePojo nextNode, final int currentModuleDistance) {
		return nextNode.getModuleId().equals(currentNode.getModuleId()) ? currentModuleDistance : currentModuleDistance + 1;
	}

	private boolean shouldFollowReadAccesses(final DataFlowNodePojo.Type type) {
		if (currentDirection == QueryDirection.BOTH) {
			return true;
		}
		return (type == DataFlowNodePojo.Type.FIELD || type == DataFlowNodePojo.Type.PROXY_FIELD) && currentDirection == QueryDirection.DOWNSTREAM
				|| type == DataFlowNodePojo.Type.STATEMENT && currentDirection == QueryDirection.UPSTREAM;
	}

	private boolean shouldFollowWriteAccesses(final DataFlowNodePojo.Type type) {
		if (currentDirection == QueryDirection.BOTH) {
			return true;
		}
		return (type == DataFlowNodePojo.Type.FIELD || type == DataFlowNodePojo.Type.PROXY_FIELD) && currentDirection == QueryDirection.UPSTREAM
				|| type == DataFlowNodePojo.Type.STATEMENT && currentDirection == QueryDirection.DOWNSTREAM;
	}

	private String getIdForDataFlowNode(final DataFlowNodePojo node) {
		final String result;
		if (node.getType() == DataFlowNodePojo.Type.PROXY_FIELD) {
			final StringBuilder sb = new StringBuilder(getIdForModuleNode(node.getModuleId()));
			final ProxyContainerPojo container = node.getProxyContainer().orElse(null);
			if (container != null) {
				sb.append("-container-");
				sb.append(container.getType());
				final var statementLocation = container.getStatementLocation();
				if (statementLocation.isPresent()) {
					sb.append("-");
					sb.append(statementLocation.get().getOffset());
				}
				final int pos = sb.length() + 7; /* length of '-field-' */
				sb.append("-interface-");
				/* use the index from the old id if present or otherwise search for the field in the container */
				if (pos < node.getDataFlowId().getId().length()) {
					sb.append(node.getDataFlowId().getId().substring(pos));
				} else {
					final var fieldNodes = container.getFieldNodes().stream()
							.map(n -> n.getDataFlowId().getId())
							.sorted(Comparator.naturalOrder())
							.toList();
					sb.append(fieldNodes.indexOf(node.getDataFlowId().getId()));
				}
			} else {
				/* PROXY_FIELDs without a container shouldn't exist, but just in case we have this as a fallback */
				sb.append("-interface-");
				sb.append(node.getName());
			}
			result = sb.toString();
		} else {
			if (node.getLocation().isPresent()) {
				result = node.getDataFlowId().getId();
			} else {
				final StringBuilder sb = new StringBuilder(getIdForModuleNode(node.getModuleId()));
				sb.append("-");
				sb.append(node.getType() == DataFlowNodePojo.Type.FIELD ? "field-" : "statement-");
				/* assuming that if the statement has no offset, it should have a unique name */
				sb.append(node.getLocation().map(loc -> String.valueOf(loc.getOffset())).orElseGet(() -> "n" + cityHash64(node.getName())));
				result = sb.toString();
			}
		}

		return result;
	}

	private static String getIdForModuleNode(final EntityId moduleId) {
		return "module-" + (moduleId.hasNid() ? moduleId.getNid() : moduleId.getUid());
	}

	private LoadingCache<UUID, String> createModuleNameCache() {
		return CacheBuilder.newBuilder()
				.softValues()
				.maximumSize(50_000)
				.build(new CacheLoader<UUID, String>() {

					@Override
					public String load(@Nullable final UUID uid) {
						return moduleService.findAnyModuleLightweight(b -> b.byUid(assertNotNull(uid)))
											.map(ModuleLightweightPojo::getName)
											.orElseThrow(() -> new MiningEntityNotFoundException("Unable to load module with id: " + uid));
					}
				});
	}
}
