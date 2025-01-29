/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import java.util.concurrent.Future;

import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.ResultOrderTaskProcessor;
import innowake.lib.job.api.task.Task;
import innowake.mining.data.model.discovery.ModelArtifact;

/**
 * Special task processor that only submits locally on the node where the current job is running.
 */
class LocalTaskProcessor extends ResultOrderTaskProcessor<ModelArtifact[]> {

	LocalTaskProcessor(final JobManager jobManager, final JobMonitor jobMonitor) {
		super(jobManager, jobMonitor);
	}

	@Override
	protected Future<Result<ModelArtifact[]>> submitTask(final Task<ModelArtifact[]> task) {
		/* Only submit the tasks on the local machine! */
		return jobManager.submitLocal(jobMonitor, task);
	}

}
