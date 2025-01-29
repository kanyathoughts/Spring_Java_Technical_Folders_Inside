/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.datalineage;

import innowake.mining.shared.access.DataFlowService;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.EntityId;

/**
 * Deletes data lineage data from a project or module.
 */
public class DataLineageResetJob extends MiningJob<Boolean> {

	@Autowired
	private transient DataFlowService dataFlowService;

	@Nullable
	private final EntityId moduleId;

	public DataLineageResetJob(final EntityId projectId, @Nullable final EntityId moduleId) {
		super(projectId, moduleId);
		this.moduleId = moduleId;
	}

	@Override
	protected Result<Boolean> run(final ProgressMonitor progressMonitor) {
		final EntityId moduleIdNotNull = moduleId;
		if (moduleIdNotNull != null) {
			progressMonitor.setJobDescription("Reset data lineage for Module " + moduleIdNotNull);
			dataFlowService.deleteForModule(moduleIdNotNull);
		} else {
			progressMonitor.setJobDescription("Reset data lineage for Project " + projectId);
			dataFlowService.deleteForProject(projectId);
		}
		return new Result<>(true);
	}
}
