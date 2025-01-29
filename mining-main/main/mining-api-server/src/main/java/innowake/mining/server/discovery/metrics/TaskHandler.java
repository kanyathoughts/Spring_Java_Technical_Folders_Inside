/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import java.io.Serializable;

import brave.Span;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskSource;

/**
 * This handler can be used to fork additional tasks and to get all information required for task forking.
 */
public interface TaskHandler {

	/**
	 * Forks {@linkplain Task Tasks} to be executed in parallel. Every forked task may be executed on a different machine in the cluster.
	 * This method blocks until all submitted tasks are finished and their results resolved.
	 *
	 * @param <R> the concrete {@link Result} type of the tasks
	 * @param taskSource the {@link TaskSource} providing the {@link Task} instances
	 * @param resultConsumer the {@link ResultConsumer} consuming all task {@linkplain Result Results}
	 */
	<R extends Serializable> void forkJobTasks(TaskSource<R> taskSource, ResultConsumer<R> resultConsumer);

	/**
	 * @return the unique job Id
	 */
	String getJobId();

	/**
	 * @return the {@link JobMonitor} of the current job
	 */
	JobMonitor getJobMonitor();
	
	/**
	 *
	 * @return the job/task span
	 */
	Span getSpan();
}
