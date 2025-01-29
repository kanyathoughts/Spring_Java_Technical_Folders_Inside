/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.core;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.springframework.stereotype.Service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.operations.DataLineageResolver;
import innowake.mining.server.datalineage.operations.DataLineageTracer;
import innowake.mining.server.datalineage.persistence.DataLineagePersistenceService;
import innowake.mining.server.datalineage.query.DataFlowGraphQueryService;
import innowake.mining.server.locking.ModuleLockService;
import innowake.mining.shared.access.DataFlowService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.datalineage.DataFlowId;
import innowake.mining.shared.model.datalineage.DataLineageResult;

/**
 * Main service for Data Lineage, used to trace the data flow in a module and to link the data flow between modules.
 * <p>
 * This service aggregates {@link DataLineageTracer} and {@link DataLineageResolver} components which implement the actual tracing and resolving/linking
 * functionality respectively. It is responsible for calling the appropriate component and persisting the result to the database via
 * {@link DataLineagePersistenceService}.
 * <p>
 * For retrieving the data flow, use {@link DataFlowGraphQueryService}.
 */
@Service
public class DataLineageCoreService {

	public static final String TRACE_EXECUTED_PROPERTY = "DataLineageTraceExecuted";

	private static final String TRACE_LOCK_NAME = "traceModule";
	private static final String RESOLVE_LOCK_NAME = "resolveProxyContainerPojo";
	
	private static final Logger LOG = LoggerFactory.getLogger(DataLineageCoreService.class);

	private final List<DataLineageTracer> tracer;
	private final List<DataLineageResolver> resolvers;
	private final DataLineagePersistenceService persistenceService;
	private final ModuleService moduleService;
	private final DataFlowService dataFlowService;
	private final ModuleLockService moduleLockService;

	/**
	 * Constructor
	 * @param operations to trace the data flow inside a Module
	 * @param resolutions to resolve and link data flow between modules
	 * @param persistenceService service allowing to persist {@link DataLineageResult} into the db
	 * @param moduleService Module data access service.
	 * @param dataFlowService DAO for {@link DataFlowNodePojo}
	 * @param moduleLockService service allowing module locking
	 */
	public DataLineageCoreService(final Optional<List<DataLineageTracer>> operations,
								  final Optional<List<DataLineageResolver>> resolutions,
								  final DataLineagePersistenceService persistenceService,
								  final ModuleService moduleService,
								  final DataFlowService dataFlowService,
								  final ModuleLockService moduleLockService) {
		this.tracer = operations.orElse(Collections.emptyList());
		this.resolvers = resolutions.orElse(Collections.emptyList());
		this.persistenceService = persistenceService;
		this.moduleService = moduleService;
		this.dataFlowService = dataFlowService;
		this.moduleLockService = moduleLockService;

		this.resolvers.forEach(op -> op.setDataLineageCoreService(this));
	}

	/**
	 * Returns whether the module with the given id is supported by Data Lineage.
	 *
	 * @param projectId the id of the project containing the module
	 * @param moduleId the id of the module
	 * @return {@code true} if the module is supported by data lineage
	 */
	public boolean isSupported(final EntityId projectId, final EntityId moduleId) {
		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(b -> b.byNid(moduleId.getNid()))
				.orElseThrow(() -> new MiningEntityNotFoundException("Unable to load module with numeric id: " + moduleId.getNid()));
		return findTracer(module).isPresent();
	}

	/**
	 * Traces the internal data flow and discovers all proxy containers on the given module (but does not link these proxy containers with any other module).
	 * @param context the {@code DataLineageContext} for the operation
	 * @param moduleId the id of the module to trace
	 */
	public void traceModule(final DataLineageContext context, final EntityId moduleId) {
		try (final ModuleLockService.Lock lock = moduleLockService.tryLock(TRACE_LOCK_NAME, context.getProjectId(), moduleId,
				/* tracing can potentially take some time, so apply a generous timeout */
				30, TimeUnit.MINUTES)) {
			doTrace(context, moduleId);
		}
	}

	/**
	 * Resolves the given proxy container, connecting its proxy fields to all applicable adjacent modules.
	 * @param context the {@code DataLineageContext} for the operation
	 * @param moduleId the id of the module to trace
	 * @param proxyContainerId the id of the proxy container to resolve
	 */
	public void resolveProxyContainer(final DataLineageContext context, final EntityId moduleId, final DataFlowId proxyContainerId) {
		final String lockName = RESOLVE_LOCK_NAME + proxyContainerId.getId();
		try (final ModuleLockService.Lock lock = moduleLockService.tryLock(lockName, context.getProjectId(), moduleId,
				/* tracing can potentially take some time, so apply a generous timeout */
				30, TimeUnit.MINUTES)) {
			doResolveProxyContainer(context, moduleId, proxyContainerId);
		}

	}
	
	/**
	 * Discovers all proxy containers of the given type on the given module. This is a shortcut for {@link #traceModule(DataLineageContext, EntityId)},
	 * which would also discover all proxy containers.
	 * <p>
	 * This method is mostly used by {@link DataLineageResolver} implementation in order to discover all proxy containers required for linking two modules.
	 * @param context the {@code DataLineageContext} for the operation
	 * @param moduleId the id of the module on which to discover proxy containers
	 * @param type the type of proxy container to discover
	 * @return the (existing or newly created) proxy containers on the module with the given type
	 */
	public Collection<ProxyContainerPojo> getProxyContainersOfType(final DataLineageContext context, final EntityId moduleId, final ProxyContainerPojo.Type type) {
		try (final ModuleLockService.Lock lock = moduleLockService.tryLock(TRACE_LOCK_NAME, context.getProjectId(), moduleId,
				/* tracing can potentially take some time, so apply a generous timeout */
				30, TimeUnit.MINUTES)) {
			return doGetProxyContainersOfType(context, moduleId, type);
		}
	}

	private void doResolveProxyContainer(final DataLineageContext context, final EntityId moduleId, final DataFlowId proxyContainerId) {
		final Optional<ProxyContainerPojo> proxyContainer = persistenceService.findProxyContainerByDataFlowId(proxyContainerId);
		if ( ! proxyContainer.isPresent()) {
			throw new IllegalArgumentException("Cannot resolve Proxy Container " + proxyContainerId + ": Proxy Container not founds");
		}


		if (proxyContainer.get().getFieldNodes().stream().allMatch(DataFlowNodePojo::isTraced)) {
			/* proxy container is already resolved */
			return;
		}

		final ModuleLightweightPojo module = moduleService.findAnyModuleLightweight(b -> b.byId(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException("Cannot execute Proxy Containers on Module. Module does not exist: " + moduleId));
		
		final Optional<DataLineageResolver> res = findResolution(module, proxyContainer.get());
		if (res.isPresent()) {
			final DataLineageResult resolutionResult = res.get().resolve(context, module, proxyContainer.get());
			persistenceService.importDataLineageResult(context, moduleId, resolutionResult);
			persistenceService.markProxyContainerResolved(context, proxyContainerId);
		} else {
			LOG.debug(() -> "Module with technology and type is not supported by Data Lineage: " + module.toString());
		}
	}
	

	private void doTrace(final DataLineageContext context, final EntityId moduleId) {
		final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(context.getProjectId()).byId(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException("Cannot execute Data Lineage on Module. Module does not exist: " + moduleId));
		
		if (module.getInfo().orElse(Collections.emptyMap()).containsKey(TRACE_EXECUTED_PROPERTY) && dataFlowService.findAny(q -> q.ofModule(moduleId)).isPresent()) {
			return;
		}
		
		final ModuleLightweightPojo moduleLightweight = new ModuleLightweightPojo(module);
		final Optional<DataLineageTracer> op = findTracer(moduleLightweight);
		if (op.isPresent()) {
			context.getProgressMonitor().setStepDescription("Tracing Data Lineage in " + module.getName());
			final DataLineageResult traceResult = op.get().trace(context, moduleLightweight);
			context.getProgressMonitor().setStepDescription("Storing Data Lineage result for " + module.getName());
			persistenceService.importDataLineageResult(context, moduleId, traceResult);
			context.getProgressMonitor().setStepDescription("Finishing " + module.getName());
			persistenceService.markModuleResolved(context, moduleId);
			markModuleTraced(context, module);
		} else {
			LOG.debug("Module " + moduleId + " with technology " + module.getTechnology() + " and type " + module.getType()
					+ " is not supported by Data Lineage.");
		}
	}

	private void markModuleTraced(final DataLineageContext context, final ModulePojo module) {
		final Map<String, Object> info = module.getInfo().map(HashMap::new).orElseGet(HashMap::new);
		info.put(TRACE_EXECUTED_PROPERTY, "true");
		final ModulePojoPrototype proto = new ModulePojoPrototype().setProject(context.getProjectId()).setUid(module.getUid()).setInfo(info);
		moduleService.update(proto);
	}

	private Collection<ProxyContainerPojo> doGetProxyContainersOfType(final DataLineageContext context, final EntityId moduleId, final ProxyContainerPojo.Type type) {
		final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(context.getProjectId()).byId(moduleId))
				.orElseThrow(() -> new MiningEntityNotFoundException("Cannot execute Data Lineage on Module " + moduleId + ": Module does not exist"));

		if ( ! module.getInfo().orElse(Collections.emptyMap()).containsKey(TRACE_EXECUTED_PROPERTY) ||
				dataFlowService.findAny(q -> q.ofModule(moduleId)).isEmpty()) {
			traceModule(context, moduleId);
		}
		return persistenceService.findProxyContainersByModuleAndType(moduleId, type);
	}

	private Optional<DataLineageTracer> findTracer(final ModuleLightweightPojo module) {
		return tracer.stream()
				.filter(op -> op.isSupported(module))
				.findFirst();
	}

	private Optional<DataLineageResolver> findResolution(final ModuleLightweightPojo module, final ProxyContainerPojo proxyContainer) {
		return resolvers.stream()
				.filter(res -> res.isSupported(module, proxyContainer))
				.findFirst();
	}
}
