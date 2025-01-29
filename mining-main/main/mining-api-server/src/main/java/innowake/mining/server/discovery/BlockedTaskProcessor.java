/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery;

import java.io.Serializable;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.DefaultTaskProcessor;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.Task;

/**
 * Special task processor that always submits a specific amount of tasks at once and then collects the results before submitting again.
 * This leads to a much smoother memory usage. Otherwise the Future results will be held in memory until the executor
 * queue is full, which can take long.
 * 
 * @param <T> The concrete result type of the tasks being submitted
 */
public class BlockedTaskProcessor<T extends Serializable> extends DefaultTaskProcessor<T> {
	
	private final int taskAmount;
	
	int tasksSubmitted = 0;

	/**
	 * Constructor.
	 * 
	 * @param jobManager the {@link JobManager} used to submit {@linkplain Task Tasks}
	 * @param jobMonitor the {@link JobMonitor} of the {@link Job} forking the tasks
	 * @param taskAmount the amount of tasks to submit in one block, before collecting the results
	 */
	public BlockedTaskProcessor(final JobManager jobManager, final JobMonitor jobMonitor, final int taskAmount) {
		super(jobManager, jobMonitor);
		this.taskAmount = taskAmount;
	}
	
	@Override
	protected void submitNextTask(final Task<T> task, final ResultConsumer<T> resultConsumer) {
		if (tasksSubmitted < taskAmount) {
			tasksSubmitted++;
		} else {
			tasksSubmitted = 0;
			handleTaskResults(resultConsumer);
		}
		super.submitNextTask(task, resultConsumer);
	}
	
}
