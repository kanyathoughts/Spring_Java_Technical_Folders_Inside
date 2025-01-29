/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.export.callchain;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.PostConstruct;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import innowake.lib.core.api.profiling.Profiler;
import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.lib.core.api.profiling.ProfilingSession;
import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.extensions.export.callchain.model.CallChain;
import innowake.mining.extensions.export.callchain.model.CallChain.CallChainDirection;
import innowake.mining.extensions.export.callchain.model.CallChain.CallChainEntry;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.extensions.export.callchain.model.CallChainGraph.CallChainEdge;
import innowake.mining.shared.FutureUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.ModuleInquiryBuilder;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.configuration.GenericConfiguration;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Type;

/**
 * Service which creates the {@link CallChain CallChains} e.g. for exporting functionality in the {@link CallChainExporterJob}.
 */
@Service
public class CallChainService {

	private static final String PROFILING_CATEGORY_PREFIX = "callchain";
	private static final Logger LOG = LoggerFactory.getLogger(CallChainService.class);

	private final ModuleService moduleService;
	private final TaxonomyService taxonomyService;
	private final int maximumTaskThreads;

	@Nullable
	private ProfilingSession profilingSession;
	@Nullable
	private Profiler profiler;

	/**
	 * Constructor
	 * 
	 * @param moduleService the {@link ModuleService}
	 * @param taxonomyService the {@link TaxonomyService}
	 * @param configProperties The {@link GenericConfiguration}
	 */
	@Autowired
	public CallChainService(final ModuleService moduleService, final TaxonomyService taxonomyService, final GenericConfiguration configProperties) {
		this.moduleService = moduleService;
		this.taxonomyService = taxonomyService;
		maximumTaskThreads = configProperties.getCallChainMaximumExportThreads() < 1
												? Runtime.getRuntime().availableProcessors()
												: configProperties.getCallChainMaximumExportThreads();
	}

	@PostConstruct
	public void init() {
		profilingSession = ProfilingFactory.getProfilingSession();
		profiler = assertNotNull(profilingSession).getProfiler(PROFILING_CATEGORY_PREFIX);
	}

	/**
	 * Returns all {@link CallChain CallChains} for the given {@code parameters}.
	 *
	 * @param progressMonitor The {@link ProgressMonitor}
	 * @param parameters The {@link Parameters} containing all export settings
	 * @return optional with a {@link List} of exported {@link CallChain CallChains}
	 */
	public Optional<List<CallChainGraph>> createCallChainGraphs(final ProgressMonitor progressMonitor, final Parameters parameters) {
		if (parameters.getDepth() == 0) {
			LOG.info(() -> "No CallChains are exported: Depth is 0. Parameters: " + parameters.toString());
			return Optional.of(Collections.emptyList());
		}

		final LoadingCache<Pair<Long, CallChainDirection>, List<ModuleRelationshipPojo>> referenceCache = createReferenceCache();
		final LoadingCache<UUID, ModuleLightweightPojo> moduleCache = createModuleCache();
		final LoadingCache<Long, Boolean> ignoredByTaxonomyCache = createIgnoredTaxonomyCache(parameters.getProjectId(), parameters.getIgnoredTaxonomy());

		progressMonitor.setJobDescription("Exporting Call Chains");
		LOG.info(() -> "Exporting CallChain with parameters: " + parameters.toString());

		/* Prepare start modules */
		final List<ModuleLightweightPojo> startModules = initializeStartModules(parameters);

		final BlockingQueue<Future<?>> futures = new LinkedBlockingQueue<>();
		final int nThreads = parameters.getParallel() < 1 ? Runtime.getRuntime().availableProcessors() : parameters.getParallel();
		final ExecutorService computationExecutor = createExecutorService(Math.min(maximumTaskThreads, nThreads));

		progressMonitor.setStepDescription(String.format("Beginning Call Chain computations for %d modules", Integer.valueOf(startModules.size())));
		LOG.debug(() -> "Begin CallChain computation ...");
		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();

		/* collect call chains for all configured directions starting with the configured start module
		 * directions can only contain "IN", "OUT" or both */
		final List<CallChainGraph> callChainGraphs = collectCallChains(startModules, parameters, futures, Pair.of(computationExecutor, progressMonitor),
				ignoredByTaxonomyCache, referenceCache, moduleCache);

		try {
			final long exportedCount = FutureUtil.awaitAll(futures);
			stopWatch.stop();
			LOG.info(() -> String.format("CallChain computation of %d entries took %s (H:mm:ss.SSS)", Long.valueOf(exportedCount), stopWatch));
		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
			LOG.error("Interrupted while waiting for CallChain result", e);
			throw new IllegalStateException(e);
		} catch (final ExecutionException e) {
			LOG.error("Error while building CallChain", e);
			throw new IllegalStateException(e);
		} finally {
			computationExecutor.shutdown();
		}

		if (progressMonitor.isCanceled()) {
			return Optional.empty();
		}

		stopWatch.reset();
		stopWatch.start();
		callChainGraphs.forEach(graph -> filterCallChainGraph(graph, parameters));
		stopWatch.stop();
		LOG.info(() -> String.format("Filtering the call chain took %s (H:mm:ss.SSS)", stopWatch));

		return Optional.of(callChainGraphs);
	}

	private List<ModuleLightweightPojo> initializeStartModules(final Parameters parameters) {
		List<ModuleLightweightPojo> startModules;
		if (parameters.getStartModuleIds().isEmpty() && parameters.getStartModuleTypes().isEmpty()) {
			LOG.info("Start module ids and types are not provided. Start module ids are automatically being determined based on modules with no incoming references");

			startModules = moduleService.findModulesLightweight(b -> {
				b.ofProject(parameters.getProjectId());
				/* find all unreferenced modules */
				if ( ! parameters.getCallTypes().isEmpty()) {
					filterRelationships(parameters, b);
				}
			});
		} else {
			LOG.info(() -> "Start module ids and types are provided.");
			if ( ! parameters.getStartModuleTypes().isEmpty()) {
				startModules = new ArrayList<>(moduleService.findModulesLightweight(b -> b.ofProject(parameters.getProjectId()).withTypes(parameters.getStartModuleTypes())));
			} else {
				startModules = new ArrayList<>();
			}
			if ( ! parameters.getStartModuleIds().isEmpty()) {
				if (startModules.isEmpty()) {
					startModules = moduleService.findModulesLightweight(b -> b.ofProject(parameters.getProjectId()).byIds(parameters.getStartModuleIds()));
				} else {
					startModules.addAll(moduleService.findModulesLightweight(b -> b.ofProject(parameters.getProjectId()).byIds(parameters.getStartModuleIds())));
				}
			}
		}

		return startModules;
	}
	
	private List<CallChainGraph> collectCallChains(final List<ModuleLightweightPojo> startModules, final Parameters parameters, final BlockingQueue<Future<?>> futures,
			final Pair<ExecutorService, ProgressMonitor> computationExeProgressMonitor, final LoadingCache<Long, Boolean> ignoredByTaxonomyCache,
			final LoadingCache<Pair<Long, CallChainDirection>, List<ModuleRelationshipPojo>> referenceCache,
			final LoadingCache<UUID, ModuleLightweightPojo> moduleCache) {
		final ProgressMonitor progressMonitor = computationExeProgressMonitor.getRight();
		final ExecutorService computationExecutor = computationExeProgressMonitor.getLeft();
		final List<CallChainGraph> callChainGraphs = new ArrayList<>(startModules.size() * parameters.getDirections().size());
		
		for (final CallChainDirection direction : parameters.getDirections()) {
			startModules.stream().distinct().forEach(startModule -> {
				if (progressMonitor.isCanceled()) {
					return;
				}
				if ( ! parameters.getIgnoredTaxonomy().isEmpty() && ignoredByTaxonomyCache.getUnchecked(startModule.getId()).booleanValue()) {
					/* Module is already ignored by taxonomy */
					LOG.info(() -> String.format("Module is ignored by taxonomies. Module %s with id %s", startModule.getName(), startModule.identity()));
				} else {
					final CallChainGraph callChainGraph = new CallChainGraph(direction, startModule);
					final boolean startFiltered = isStartModuleFiltered(startModule, parameters);
					if  ( ! startFiltered) {
						callChainGraphs.add(callChainGraph);
					}
	
					futures.add(computationExecutor.submit(() -> buildCallChain(computationExecutor, futures,
																				callChainGraph, startModule,
																				parameters.getDepth(),
																				parameters, referenceCache, moduleCache, ignoredByTaxonomyCache,
																				callChainGraphs, startFiltered)));
				}
			});
			if (progressMonitor.isCanceled()) {
				break;
			}
		}
		
		return callChainGraphs;
	}
	
	private boolean isStartModuleFiltered(final ModuleLightweightPojo startModule, final Parameters parameters) {
		final boolean startFiltered;
		if (parameters.getFilteredModuleTypes().contains(startModule.getType()) && ! parameters.getStartModuleIds().contains(startModule.identity())) {
			startFiltered = true;
		} else {
			startFiltered = false;
		}
		return startFiltered;
	}

	/**
	 * Returns the {@link ExecutorService} for scheduling the the call chain export tasks.
	 *
	 * @param nThreads the number of threads in the pool
	 * @return the {@link ExecutorService}
	 */
	/* protected for testability */
	protected ExecutorService createExecutorService(final int nThreads) {
		return Executors.newFixedThreadPool(nThreads);
	}

	private void buildCallChain(final ExecutorService executor, final BlockingQueue<Future<?>> futures,
								final CallChainGraph callChainGraph, final ModuleLightweightPojo currentModule, final int depth,
								final Parameters parameters,
								final LoadingCache<Pair<Long, CallChainDirection>, List<ModuleRelationshipPojo>> referenceCache,
								final LoadingCache<UUID, ModuleLightweightPojo> moduleCache,
								final LoadingCache<Long, Boolean> ignoredByTaxonomyCache,
								final List<CallChainGraph> callChainGraphs,
								final boolean startFiltered) {

		assertNotNull(profiler).start("buildCallChain");
		try {
			if (callChainGraph.getSize() % 10_000 == 1) {
				LOG.info(() -> String.format("CallChainGraph for %s with id %s has %d edges", callChainGraph.getRoot().getName(),
						callChainGraph.getRoot().identity(), Integer.valueOf(callChainGraph.getSize())));
			}

			final List<ModuleRelationshipPojo> references = getReferences(currentModule, callChainGraph.getDirection(), referenceCache, parameters);
			for (final ModuleRelationshipPojo reference : references) {
				final RelationshipType callType = reference.getRelationship();
				if (parameters.getCallTypes().contains(callType)) {
					final CallChainDirection effectiveDirection = getEffectiveDirection(callChainGraph, currentModule, parameters, reference);
					final ModuleLightweightPojo calledModule = moduleCache.getUnchecked(effectiveDirection == CallChainDirection.IN ? reference.getSrcModule() : reference.getDstModule());
					final CallChainGraph callChainGraphRef = new CallChainGraph(callChainGraph.getDirection(), calledModule);
					if ((callChainGraph.getDirection() == CallChainDirection.OUT
							&& ! checkConditionalDependency(reference, callChainGraph))
							|| (depth - 1 == 0 && ( ! parameters.getEndModuleIds().isEmpty() || ! parameters.getEndModuleTypes().isEmpty())
							&& ! isMatchingEndModule(calledModule, parameters))) {
						continue;
					}

					/* add the current module to the call chain, this is done even if there is a call loop,
					 * so that it is apparent that this call chain represents a loop (the first and last entry will be the same module) */
					if (callChainBuildingConditions(callType, calledModule, callChainGraph, currentModule, depth, parameters, ignoredByTaxonomyCache,
							startFiltered, reference)) {
						if (startFiltered) {
							if (parameters.getFilteredModuleTypes().contains(calledModule.getType())) {
								futures.add(executor.submit(() -> buildCallChain(executor, futures, callChainGraphRef, calledModule, depth - 1,
										parameters, referenceCache, moduleCache, ignoredByTaxonomyCache, callChainGraphs, true)));
							} else {
								callChainGraphs.add(callChainGraphRef);
								futures.add(executor.submit(() -> buildCallChain(executor, futures, callChainGraphRef, calledModule, depth - 1,
										parameters, referenceCache, moduleCache, ignoredByTaxonomyCache, callChainGraphs, false)));
							}
						} else {
							futures.add(executor.submit(() -> buildCallChain(executor, futures, callChainGraph, calledModule, depth - 1,
										parameters, referenceCache, moduleCache, ignoredByTaxonomyCache, callChainGraphs, false)));
						}
					}
				}
			}
		} finally {
			assertNotNull(profiler).stop();
			if (assertNotNull(profiler).isEnabled()) {
				assertNotNull(profilingSession).flushCurrentThread();
			}
		}
	}
	
	private boolean callChainBuildingConditions(final RelationshipType callType, final ModuleLightweightPojo calledModule, final CallChainGraph callChainGraph,
			final ModuleLightweightPojo currentModule, final int depth, final Parameters parameters, final LoadingCache<Long, Boolean> ignoredByTaxonomyCache,
			final boolean startFiltered, final ModuleRelationshipPojo reference) {
		if ( ! startFiltered && ! callChainGraph.add(currentModule, new CallChainEdge(reference.getId().toString(), calledModule, callType,
				reference.getProperties().orElseGet(Collections::emptyMap), reference.getValidIfReachedFrom()))) {
			/* if callChainGraph.add returns false then the edge already existed which means me can stop here */
			LOG.debug(() -> String.format("CallChainGraph already contains the edge %s for module %s with id %d",
										new CallChainEdge(reference.getId().toString(), calledModule, callType, reference.getProperties().orElseGet(Collections::emptyMap)),
										currentModule.getName(), currentModule.getId()));
			return false;
		} else if (callChainGraph.contains(calledModule)) {
			/* loop detected. break endless loop by exiting */
			LOG.info(() -> String.format("Loop detected for module %s with id %d", calledModule.getName(), calledModule.getId()));
			return false;
		} else if (depth - 1 == 0) {
			/* maximum depth reached */
			LOG.info(() -> String.format("Maximum depth reached for module %s with id %d", calledModule.getName(), calledModule.getId()));
			return false;
		} else if ( ! parameters.getIgnoredTaxonomy().isEmpty() && ignoredByTaxonomyCache.getUnchecked(calledModule.getId()).booleanValue()) {
			/* if some Taxonomies are ignored, check if the current module shall be ignored by looking up the value from the cache */
			LOG.info(() -> String.format("Module is ignored by taxonomies. Module %s with id %d", calledModule.getName(), calledModule.getId()));
			return false;
		} else if (parameters.getEndModuleIds().contains(calledModule.identity())
				|| parameters.getEndModuleTypes().contains(calledModule.getType())) {
			/* current module matches "end module ids" or "end module types", so stop the call chain
			 * this check is not applied to the start module */
			LOG.info(() -> String.format("End reached with module %s with id %d", calledModule.getName(), calledModule.getId()));
			return false;
		}
		
		return true;
	}
	
	/**
	 * Checks whether the condition of the conditional dependency of a {@code module_relationship} is true.
	 *
	 * @param reference The {@code module_relationship} potentially containing a conditional dependency.
	 * @param graph The existing {@linkplain CallChainGraph}. Used to check whether modules referred to by the condition already exist.
	 * @return {@code true} if one of the following is true:
	 * <li>There is no conditional dependency</li>
	 * <li>There is a conditional dependency but it doesn't refer to any modules</li>
	 * <li>At least one of the modules referred to by the conditional dependency already exists in the graph</li>
	 * <li>if the reference is not from dead code</li>
	 */
	private boolean checkConditionalDependency(final ModuleRelationshipPojo reference, final CallChainGraph graph) {

		final Map<String, Object> properties = reference.getProperties().orElse(Collections.emptyMap());
		final var fromDeadCode = properties.get("dead_code");
		if (fromDeadCode instanceof Boolean && ((Boolean) fromDeadCode).booleanValue()) {
			return false;
		}
		
		final List<EntityId> ifReachedFromModuleIds = moduleService.findModuleIds(b -> b.withConditionalRelationship(reference.getId()));
		if (ifReachedFromModuleIds.isEmpty()) {
			return true;
		}

		for (final var ifReachedFromModuleId : ifReachedFromModuleIds) {
			if (graph.contains(ifReachedFromModuleId.getNid())) {
				return true;
			}
		}

		return false;
	}

	private static CallChainDirection getEffectiveDirection(final CallChainGraph graph, final ModuleLightweightPojo module, final Parameters parameters,
			final ModuleRelationshipPojo reference) {
		final CallChainDirection effectiveDirection;
		if (parameters.isDataAccessBased() && RelationshipType.ACCESSES == reference.getRelationship()) {
			if (module.getType() == Type.FILE || module.getType() == Type.TABLE) {
				effectiveDirection = CallChainDirection.IN;
			} else {
				effectiveDirection = CallChainDirection.OUT;
			}
		} else {
			effectiveDirection = graph.getDirection();
		}

		return effectiveDirection;
	}
	
	private List<ModuleRelationshipPojo> getReferences(final ModuleLightweightPojo currentModule, final CallChainDirection currentCallChainDirection,
			final LoadingCache<Pair<Long, CallChainDirection>, List<ModuleRelationshipPojo>> referenceCache, final Parameters parameters) {
		final Stream<ModuleRelationshipPojo> references;
		if (parameters.isDataAccessBased()) {
			/* special handling for "data access based call chain" */
			if (currentModule.getType() == Type.FILE || currentModule.getType() == Type.TABLE) {
				/* files and tables only have _incoming_ ReadsWrites relationships, but the access type property is important here */
				references = handleFileAndTableBasedCalls(currentModule, currentCallChainDirection, referenceCache);
			} else {
				/* for other things we track outgoing ReadsWrites edges, with the reverse file access type */
				final Stream<ModuleRelationshipPojo> filteredReferences = referenceCache.getUnchecked(Pair.of(currentModule.getId(), CallChainDirection.OUT)).stream()
						.filter(ref -> trackReadWrites(ref, currentCallChainDirection));
				if (currentCallChainDirection == CallChainDirection.OUT) {
					references = filteredReferences;
				} else {
					references = Stream.concat(filteredReferences, referenceCache.getUnchecked(Pair.of(currentModule.getId(), CallChainDirection.IN)).stream()
									.filter(ref -> RelationshipType.ACCESSES != ref.getRelationship()));
				}
			}
		} else {
			references = referenceCache.getUnchecked(Pair.of(currentModule.getId(), currentCallChainDirection)).stream();
		}

		return parameters.getEdgePropertyFilter().isEmpty() ?
					references.collect(Collectors.toList()) :
					references.filter(reference -> isIncludedByFilter(reference.getProperties().orElse(Collections.emptyMap()), parameters.getEdgePropertyFilter()))
								.collect(Collectors.toList());
	}
	
	private boolean trackReadWrites(final ModuleRelationshipPojo ref, final CallChainDirection currentCallChainDirection) {
		if (RelationshipType.ACCESSES == ref.getRelationship() && currentCallChainDirection == CallChainDirection.IN) {
			/* for inbound call chain we track READS from files */
			return isReadAccess(ref.getProperties());
		} else if (RelationshipType.ACCESSES == ref.getRelationship() && currentCallChainDirection == CallChainDirection.OUT) {
			/* for outbound call chains we track WRITES to files */
			return isWriteAccess(ref.getProperties());
		} else {
			return currentCallChainDirection == CallChainDirection.OUT;
		}
	}
	
	private Stream<ModuleRelationshipPojo> handleFileAndTableBasedCalls(final ModuleLightweightPojo currentModule, final CallChainDirection currentCallChainDirection,
			final LoadingCache<Pair<Long, CallChainDirection>, List<ModuleRelationshipPojo>> referenceCache) {
		Stream<ModuleRelationshipPojo> references;
		final Stream<ModuleRelationshipPojo> filteredReferences = referenceCache.getUnchecked(Pair.of(currentModule.getId(), CallChainDirection.IN)).stream()
				.filter(ref -> {
					if (RelationshipType.ACCESSES == ref.getRelationship() && currentCallChainDirection == CallChainDirection.IN) {
						/* for inbound call chain we track WRITES to files or tables */
						return isWriteAccess(ref.getProperties());
					} else if (RelationshipType.ACCESSES == ref.getRelationship() && currentCallChainDirection == CallChainDirection.OUT) {
						/* for outbound call chains we track READS from files */
						return isReadAccess(ref.getProperties());
					} else {
						return currentCallChainDirection == CallChainDirection.IN;
					}
				});
		if (currentCallChainDirection == CallChainDirection.IN) {
			references = filteredReferences;
		} else {
			references = Stream.concat(filteredReferences, referenceCache.getUnchecked(Pair.of(currentModule.getId(), CallChainDirection.OUT)).stream()
								.filter(ref -> RelationshipType.ACCESSES != ref.getRelationship()));
		}
		return references;
	}

	private static boolean isReadAccess(final Optional<Map<String, Object>> referenceProperties) {
		if (referenceProperties.isEmpty() || referenceProperties.get().isEmpty()) {
			return false;
		}

		final String fileAccessTypes = getProperty(referenceProperties.get(), ModelAttributeKey.FILE_ACCESS_TYPE);
		final String dbAccessTypes = getProperty(referenceProperties.get(), ModelAttributeKey.DB_ACCESS_TYPE);

		return StringUtils.contains(fileAccessTypes, ModelAttributeValue.FileAccess.READ.toString())
				|| StringUtils.contains(dbAccessTypes, DatabaseAccessType.READ.toString());
	}

	private static boolean isWriteAccess(final Optional<Map<String, Object>> referenceProperties) {
		if (referenceProperties.isEmpty() || referenceProperties.get().isEmpty()) {
			return false;
		}

		final String fileAccessTypes = getProperty(referenceProperties.get(), ModelAttributeKey.FILE_ACCESS_TYPE);
		final String dbAccessTypes = getProperty(referenceProperties.get(), ModelAttributeKey.DB_ACCESS_TYPE);

		return StringUtils.contains(fileAccessTypes, ModelAttributeValue.FileAccess.WRITE.toString())
				|| StringUtils.contains(dbAccessTypes, DatabaseAccessType.STORE.toString())
				|| StringUtils.contains(dbAccessTypes, DatabaseAccessType.UPDATE.toString())
				|| StringUtils.contains(dbAccessTypes, DatabaseAccessType.DELETE.toString());
	}

	@Nullable
	private static String getProperty(final Map<String, Object> properties, final ModelAttributeKey key) {
		final var property = properties.get(key.toString());
		return property == null ? null : property.toString();
	}

	@SuppressWarnings("unchecked")
	private static boolean isIncludedByFilter(final Map<String, Object> referenceProperties, final Map<String, Serializable> filterObject) {
		for (final Map.Entry<String, Serializable> filterObjectEntry : filterObject.entrySet()) {
			switch (filterObjectEntry.getKey()) {
				case "_and":
					/* handle { _and: [ filterObjects... ] } */
					return ((List<Map<String, Serializable>>) filterObjectEntry.getValue()).stream().allMatch(nestedObject ->
								isIncludedByFilter(referenceProperties, nestedObject));
				case "_or":
					/* handle { _or: [ filterObjects... ] } */
					return ((List<Map<String, Serializable>>) filterObjectEntry.getValue()).stream().anyMatch(nestedObject ->
							isIncludedByFilter(referenceProperties, nestedObject));
				case "_not":
					/* handle { _not: { filterObject } } */
					return ! isIncludedByFilter(referenceProperties, (Map<String, Serializable>) filterObjectEntry.getValue());
				default:
					/* handle { propertyName: { operator: value } } */
					return handleFilterOperator(referenceProperties, filterObjectEntry.getKey(), (Map<String, Serializable>) filterObjectEntry.getValue());
			}
		}
		/* reaching this means map was empty -> no filters defined: all edges are included */
		return true;
	}

	private static boolean handleFilterOperator(final Map<String, Object> referenceProperties, final String key,
			final Map<String, Serializable> operatorArgumentMap) {
		/* for now, only the "eq" operator is supported */
		final Object value = operatorArgumentMap.get("eq");
		if (value == null) {
			/* handle { eq: null } */
			return ! referenceProperties.containsKey(key);
		} else {
			/* handle { eq: "some value" } */
			return Objects.equals(value.toString(), referenceProperties.get(key));
		}
	}

	/**
	 * Traverses all paths in the given {@code graph} and creates a {@link CallChain} for each. For every {@link CallChain}, which is tested as valid according
	 * to the specified {@code parameters}, the given {@code callChainConsumer} gets called. The consumer can then operate on the {@link CallChain} end e.g.
	 * export it into a CSV file or extract required information out of it.
	 *
	 * @param graph the {@link CallChainGraph} to traverse
	 * @param parameters the {@link Parameters} for testing if a {@link CallChain} is valid
	 * @param callChainConsumer the {@link Consumer} that is called for every valid {@link CallChain}
	 * @param processedCallChains the set containing the already traversed {@link CallChain CallChains}
	 */
	public void traverseGraph(final CallChainGraph graph,  final Parameters parameters, final Consumer<CallChain> callChainConsumer,
			final Set<String> processedCallChains) {
		final CallChain callChain = new CallChain(graph.getDirection());
		addToCallChain(callChain, graph.getRoot(), "", Collections.emptyMap());

		final Set<Long> processedIds = new HashSet<>();
		processedIds.add(graph.getRoot().getId());

		traverseGraph(graph, graph.getRoot(), callChain, parameters, callChainConsumer, processedIds, processedCallChains);
	}

	private static void traverseGraph(final CallChainGraph graph, final ModuleLightweightPojo module, final CallChain callChain, final Parameters parameters, 
			final Consumer<CallChain> callChainConsumer, final Set<Long> processedModules, final Set<String> processedCallChains) {
		final Collection<CallChainEdge> edges = graph.getEdgeMap().get(module);
		if (edges.isEmpty()) {
			if (processedCallChains.add(callChain.createKey())) {
				callChainConsumer.accept(callChain);
			}
		} else {
			processModuleEdges(graph, callChain, parameters, callChainConsumer, processedModules, processedCallChains, edges);
		}
	}

	private static void processModuleEdges(final CallChainGraph graph, final CallChain callChain, final Parameters parameters,
			final Consumer<CallChain> callChainConsumer, final Set<Long> processedModules, final Set<String> processedCallChains,
			final Collection<CallChainEdge> edges) {
		final CallChain originalCallChain = edges.size() == 1 ? callChain : callChain.copy();
		final Iterator<CallChainEdge> iterator = edges.iterator();
		boolean first = true;
		final Set<UUID> callChainEntryIds = callChain.getCallChainEntries().stream().map(entry -> entry.getModule().getUid()).collect(Collectors.toSet());
		while (iterator.hasNext()) {
			final CallChainEdge edge = iterator.next();
			
			if ( ! shouldProceedWithEdge(callChainEntryIds, parameters, edge)) {
				continue;
			}

			final CallChain currentCallChain;
			if (first) {
				first = false;
				currentCallChain = callChain;
			} else {
				currentCallChain = originalCallChain.copy();
			}

			addToCallChain(currentCallChain, edge.getTarget(), StringUtils.capitalize(edge.getRelationship().toString().toLowerCase()),
					edge.getRelationshipAttributes());

			/* processedModules.contains() now does the loop detection */
			if ( ! processTargetModule(graph, parameters, callChainConsumer, processedModules, processedCallChains, edge.getTarget(), currentCallChain)) {
				break;
			}
		}
	}

	private static boolean processTargetModule(final CallChainGraph graph, final Parameters parameters, final Consumer<CallChain> callChainConsumer,
			final Set<Long> processedModules, final Set<String> processedCallChains, final ModuleLightweightPojo targetModule, final CallChain currentCallChain) {
		final int maxDepth = parameters.getDepth() < 0 ? Integer.MAX_VALUE : parameters.getDepth();
		if ( ! processedModules.contains(targetModule.getId()) && currentCallChain.getCallChainEntries().size() <= maxDepth) {
			processedModules.add(targetModule.getId());
			traverseGraph(graph, targetModule, currentCallChain, parameters, callChainConsumer, processedModules, processedCallChains);
			processedModules.remove(targetModule.getId());
		} else {
			LOG.debug(() -> "Last node in the chain: " + targetModule);

			final List<EntityId> endModuleIds = parameters.getEndModuleIds();
			if ( ! endModuleIds.isEmpty() && ! endModuleIds.contains(targetModule.identity())) {
				LOG.debug(() -> "Invalid end Module because it's not part of the valid end module IDs");
				return false;
			}

			/* We are at the end of the call chain */
			if (processedCallChains.add(currentCallChain.createKey())) {
				callChainConsumer.accept(currentCallChain);
			}
		}
		return true;
	}

	private static boolean shouldProceedWithEdge(final Set<UUID> callChainEntryIds, final Parameters parameters, final CallChainEdge edge) {
		if ( ! parameters.getDirections().contains(CallChainDirection.IN)) {
			final List<UUID> reachedFromModules = edge.getReachedFromModules();
			if ( ! reachedFromModules.isEmpty()) {
				/* Test if any of the preceding call chain entry modules is contained in the conditional dependency modules set */
				if (reachedFromModules.stream()
									.noneMatch(callChainEntryIds::contains)) {
					/* discard current CallChain if none of the preceding call chain entry modules is contained in the conditional dependency modules set */
					LOG.debug(() -> String.format("Module %s has conditional dependencies: %s but none of the preceding entries in the call chain is contained in the conditional deps. ",
							edge.getTarget().toString(), reachedFromModules.toString()));
					return false;
				}
			}
		}
		return true;
	}

	private static void addToCallChain(final CallChain callChain, final ModuleLightweightPojo currentModule, final String currentCallType,
			final Map<String, Object> relationshipAttributes) {
		if (callChain.getDirection() == CallChainDirection.IN) {
			/* When direction is "IN" it means we are following incoming edges, thus we are walking upward the call chain.
			* This means that the call chain is constructed from last to first element, therefore new elements are inserted at the
			* start of the call chain */
			callChain.getCallChainEntries().add(0, new CallChainEntry(currentModule, currentCallType, relationshipAttributes));
		} else {
			/* When direction is "OUT" it means that we are following outgoing edges. As each entry in the call chain contains information about
			* how the NEXT entry is called, we update the call type of the previous entry and add the current entry without call type.
			* Consequently, the last entry in the chain will not have a call type, which is correct since it doesn't call anything. */
			if ( ! callChain.getCallChainEntries().isEmpty()) {
				/* fill in call type of the preceding entry */
				final int lastIndex = callChain.getCallChainEntries().size() - 1;
				callChain.getCallChainEntries().set(lastIndex,
						new CallChainEntry(callChain.getCallChainEntries().get(lastIndex).getModule(), currentCallType, relationshipAttributes));
			}
			/* Add the current entry at the beginning of the call chain without call type or relationship attributes.
			* It will either be filled when adding the next module or, if this is the final module in the call chain,
			* it will be left without call type (the last module in the call chain never has call type) */
			callChain.getCallChainEntries().add(new CallChainEntry(currentModule, "", Collections.emptyMap()));
		}
	}

	private static void filterCallChainGraph(final CallChainGraph graph, final Parameters parameters) {
		final StopWatch stopWatch = new StopWatch();
		if ( ! parameters.getFilteredModuleNames().isEmpty() || ! parameters.getFilteredModuleTypes().isEmpty()) {
			stopWatch.start();
			graph.filterCallChainGraphNodes(module -> isMatchingModule(module, parameters));
			stopWatch.stop();
			LOG.info(() -> String.format("Filterung the Call Chain Graph Nodes took %s (H:mm:ss.SSS)", stopWatch));
		}
		if ( ! parameters.getEndModuleIds().isEmpty() || ! parameters.getEndModuleTypes().isEmpty()) {
			stopWatch.reset();
			stopWatch.start();
			graph.filterCallChainGraphEndNodes(module -> isMatchingEndModule(module, parameters));
			stopWatch.stop();
			LOG.info(() -> String.format("Filterung the Call Chain Graph End Nodes took %s (H:mm:ss.SSS)", stopWatch));
		}
	}

	private static boolean isMatchingModule(final ModuleLightweightPojo module, final Parameters parameters) {
		return parameters.getEndModuleIds().contains(module.identity())
				|| ( ! parameters.getFilteredModuleNames().contains(module.getName()) && ! parameters.getFilteredModuleTypes().contains(module.getType()));
	}

	private static boolean isMatchingEndModule(final ModuleLightweightPojo module, final Parameters parameters) {
		return parameters.getEndModuleIds().contains(module.identity()) 
				|| parameters.getEndModuleTypes().contains(module.getType());
	}

	/**
	 * Creates a cache that caches whether a certain Module is ignored based on Taxonomy. Key is the Module id and value is {@code true} when the Module should
	 * be ignored and {@code false} otherwise.
	 * 
	 * @param projectId the ID of the project
	 * @param ignoredTaxonomy the to be ignored {@code Taxonomy} IDs
	 */
	private LoadingCache<Long, Boolean> createIgnoredTaxonomyCache(final EntityId projectId, final List<EntityId> ignoredTaxonomy) {
		return CacheBuilder.newBuilder()
				.softValues()
				.maximumSize(10_000)
				.build(new CacheLoader<Long, Boolean>() {
					@Override
					public Boolean load(@Nullable final Long moduleId) {
						final List<EntityId> taxonomyList = taxonomyService.findIds(q -> {
									q.ofProject(projectId);
									if (moduleId != null) {
										q.ofModule(EntityId.of(moduleId));
									}
								}).stream()
								.filter(ignoredTaxonomy::contains)
							.collect(Collectors.toList());
						return Boolean.valueOf( ! taxonomyList.isEmpty());
					}
				});
	}
	
	private LoadingCache<Pair<Long, CallChainDirection>, List<ModuleRelationshipPojo>> createReferenceCache() {
		return CacheBuilder.newBuilder()
				.softValues()
				.maximumSize(10_000)
				.build(new CacheLoader<Pair<Long, CallChainDirection>, List<ModuleRelationshipPojo>>() {
					@Override
					public List<ModuleRelationshipPojo> load(@Nullable final Pair<Long, CallChainDirection> key) {
						final Pair<Long, CallChainDirection> keyNonNull = assertNotNull(key);
						return moduleService.findRelationship(b -> b.ofModuleInDirection(EntityId.of(keyNonNull.getLeft()),
								keyNonNull.getRight() == CallChainDirection.IN ? RelationshipDirection.IN : RelationshipDirection.OUT)
																	.withTypes(RelationshipType.DEPENDENCY_TYPES));
					}
				});
	}

	private LoadingCache<UUID, ModuleLightweightPojo> createModuleCache() {
		return CacheBuilder.newBuilder()
				.softValues()
				.maximumSize(10_000)
				.build(new CacheLoader<UUID, ModuleLightweightPojo>() {
					@Override
					public ModuleLightweightPojo load(@Nullable final UUID uid) {
						return moduleService.findAnyModuleLightweight(b -> b.byUid(assertNotNull(uid)))
								.orElseThrow(() -> new MiningEntityNotFoundException("Unable to load module with numeric id: " + uid));
					}
				});
	}

	private static void filterRelationships(final Parameters parameters, final ModuleInquiryBuilder builder) {
		final List<CallChainDirection> directions = parameters.getDirections();
		
		if (directions.containsAll(Arrays.asList(CallChainDirection.IN, CallChainDirection.OUT))) {
			builder.notWithSourceOrDestinationRelationships(parameters.getCallTypes());
		} else if (directions.contains(CallChainDirection.IN)) {
			builder.notWithSourceRelationships(parameters.getCallTypes());
		} else {
			builder.notWithDestinationRelationships(parameters.getCallTypes());
		}
	}
}
