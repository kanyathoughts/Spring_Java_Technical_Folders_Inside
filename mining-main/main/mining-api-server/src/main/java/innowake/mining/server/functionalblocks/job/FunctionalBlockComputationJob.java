/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.google.common.collect.Iterables;

import innowake.lib.core.lang.Assert;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.ReportMessageExceptionHandler;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.ResultOrderTaskProcessor;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskProcessor;
import innowake.lib.job.api.task.TaskSource;

/**
 * Job that executes {@linkplain innowake.mining.server.functionalblocks.service.FunctionalBlockComputationService#compute(Collection)
 * functional block computation} on a selection of functional blocks.
 * <p>
 * This job forks one Task per functional block.
 */
public class FunctionalBlockComputationJob extends Job<Boolean> {

	/* fork one FunctionalBlockComputationTask for every 1000 blocks */
	private static final int BATCH_SIZE = 1000;

	private final Set<UUID> functionalBlockIds;

	/**
	 * Creates new FunctionalBlockComputationJob to run computation for the given functional blocks.
	 * @param functionalBlockIds the ids of the functional blocks to run on
	 */
	public FunctionalBlockComputationJob(final Set<UUID> functionalBlockIds) {
		this.functionalBlockIds = functionalBlockIds;
	}

	@Override
	protected Result<Boolean> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Functional Block Computation");

		final int totalTasks = (int) Math.ceil(functionalBlockIds.size() / (double) BATCH_SIZE);
		progressMonitor.begin(totalTasks);

		final Iterable<List<UUID>> partitions = Iterables.partition(functionalBlockIds, BATCH_SIZE);
		final Iterator<List<UUID>> iterator = partitions.iterator();
		final TaskProcessor<Boolean> taskProcessor = new ResultOrderTaskProcessor<>(jobManager, Assert.assertNotNull(jobMonitor));
		final TaskSource<Boolean> taskSource = new TaskSource<>() {

			@Override
			public boolean hasNextTask() {
				return iterator.hasNext();
			}

			@Override
			public Task<Boolean> nextTask() {
				return new FunctionalBlockComputationTask(progressMonitor, jobId, iterator.next());
			}
		};
		final ResultConsumer<Boolean> resultConsumer = new ResultConsumer<>(new ReportMessageExceptionHandler<>(Assert.assertNotNull(jobMonitor))) {

			int taskCount = 0;

			@Override
			protected void handleResult(final String taskId, final Result<Boolean> result) {
				taskCount++;
				progressMonitor.setStepDescription(String.format("Functional Block computation (%d/%d)", taskCount, totalTasks));
				progressMonitor.worked(1);
			}
		};

		forkTasks(taskProcessor, taskSource, resultConsumer);

		return new Result<>(Boolean.TRUE);
	}
}
