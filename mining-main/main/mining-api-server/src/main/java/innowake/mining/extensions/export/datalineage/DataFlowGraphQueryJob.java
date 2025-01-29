/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.datalineage;

import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.mining.server.datalineage.query.DataFlowGraphQueryService;
import innowake.mining.server.datalineage.query.DetailLevel;
import innowake.mining.server.datalineage.query.Parameters;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraph;
import innowake.mining.shared.model.datalineage.graph.DataFlowGraphNode;

/**
 * Job that produces a {@link DataFlowGraph}.
 * <p>
 * This job traverses {@linkplain DataFlowNodePojo DataFlowNodes} stored in the database, starting with a set of provided start nodes.
 * It maps DataFlowNodes to {@link DataFlowGraphNode DataFlowGraphNodes} and, depending on the selected {@link DetailLevel} adds them to the resulting
 * {@link DataFlowGraph}.
 */
public class DataFlowGraphQueryJob extends MiningJob<DataFlowGraph> {

	private final Parameters parameters;

	@Autowired
	private transient DataFlowGraphQueryService queryService;


	public DataFlowGraphQueryJob(final Parameters parameters) {
		super(parameters.getProjectId());
		this.parameters = parameters;
	}

	@Override
	protected Result<DataFlowGraph> run(final ProgressMonitor progressMonitor) {

		final DataFlowGraph dataFlowGraph = queryService.buildDataFlowGraph(progressMonitor, parameters);
		return new Result<>(dataFlowGraph);
	}
}

