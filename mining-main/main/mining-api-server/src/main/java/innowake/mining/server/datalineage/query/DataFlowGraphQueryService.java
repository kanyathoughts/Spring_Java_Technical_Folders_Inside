/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage.query;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.shared.access.AstService;
import innowake.mining.shared.access.DataFlowService;
import innowake.mining.server.datalineage.context.DataLineageContextProvider;
import innowake.mining.server.datalineage.core.DataLineageCoreService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * Service for executing data flow graph queries.
 */
@Service
public class DataFlowGraphQueryService {

	private final DataFlowService dataFlowService;
	private final ModuleService moduleService;
	private final DataLineageContextProvider contextProvider;
	private final DataLineageCoreService coreService;

	private final ExecutorService executor;
	private final AstService astService;

	/**
	 * Construct new DataFlowGraphQueryService
	 *
	 * @param dataFlowService DAO for accessing data flow node
	 * @param moduleService   for providing additional module details in the result
	 * @param contextProvider factory to provide data lineage context
	 * @param coreService     service for tracing the data flow in a module
	 * @param astService      service for accessing AST nodes
	 */
	@Autowired
	public DataFlowGraphQueryService(final DataFlowService dataFlowService,
									 final ModuleService moduleService,
									 final DataLineageContextProvider contextProvider,
									 final DataLineageCoreService coreService,
									 final AstService astService) {
		this.dataFlowService = dataFlowService;
		this.moduleService = moduleService;
		this.contextProvider = contextProvider;
		this.coreService = coreService;
		this.astService = astService;

		final int poolSize = Runtime.getRuntime().availableProcessors();
		final ThreadPoolExecutor threadPoolExecutor = new ThreadPoolExecutor(poolSize, poolSize,
				60L, TimeUnit.SECONDS,
				new LinkedBlockingQueue<>());
		threadPoolExecutor.allowCoreThreadTimeOut(true);
		executor = threadPoolExecutor;
	}

	/**
	 * Computes and returns a data flow graph with the given parameters.
	 *
	 * @param progressMonitor a progress monitor to report progress and cancel the query
	 * @param parameters the query parameters
	 * @return the computed data flow graph
	 */
	public DataFlowGraph buildDataFlowGraph(final ProgressMonitor progressMonitor, final Parameters parameters) {
		return new DataFlowGraphQuery(executor, dataFlowService, contextProvider, coreService, moduleService, astService, progressMonitor, parameters).execute();
	}
}

