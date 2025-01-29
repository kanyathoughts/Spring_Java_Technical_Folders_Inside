/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.task;

import java.io.Serializable;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.CapacityExceededException;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.OperationCanceledException;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.internal.Logging;

/**
 * Default implementation of a {@link TaskProcessor}.
 * 
 * @param <S> the concrete return type of the task
 */
public class DefaultTaskProcessor<S extends Serializable> implements TaskProcessor<S> {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.TASK_PROCESSOR);
	
	protected final Map<String, Future<Result<S>>> tasks = new ConcurrentHashMap<>();
	protected final JobManager jobManager;
	protected final JobMonitor jobMonitor;
	
	/**
	 * Constructor.
	 * 
	 * @param jobManager the {@link JobManager} used to submit {@linkplain Task Tasks}
	 * @param jobMonitor the {@link JobMonitor} used to resolve information of the {@link Job} submitting the {@linkplain Task Tasks}
	 */
	public DefaultTaskProcessor(final JobManager jobManager, final JobMonitor jobMonitor) {
		this.jobManager = jobManager;
		this.jobMonitor = jobMonitor;
	}
	
	@Override
	public void process(final TaskSource<S> taskSource, final ResultConsumer<S> resultConsumer) {
		try {
			/* Iterate over all tasks provided by the task source and submit for execution. */
			for (final Task<S> task : taskSource) {
				submitNextTask(task, resultConsumer);
			}
			
			/* Get the result of all submitted tasks. This blocks until all tasks are done. */
			handleTaskResults(resultConsumer);
		} catch (final Exception e) {
			/* In case there is any exception (i.e. cancel request or unhandled), we wait until all already submitted tasks are
			 * done and then re-throw the exception to the job. This is done to avoid any zombie tasks still running in the background,
			 * while the job is already being terminated and marked as failed. */
			waitForNonCollectedTasks();
			throw e;
		} finally {
			tasks.clear();
		}
	}
	
	/**
	 * Submits the next {@link Task} for execution.
	 * <p>
	 * In case of a {@link CapacityExceededException}, this default implementation will first wait until all already
	 * submitted tasks are finished (inclusive consuming their result) and then re-tries to submit the task until succeeding.
	 * 
	 * @param task the {@link Task} to submit for execution
	 * @param resultConsumer the {@link ResultConsumer} consuming task {@linkplain Result Results}
	 */
	protected void submitNextTask(final Task<S> task, final ResultConsumer<S> resultConsumer) {
		LOG.debug(() -> "Submitting next task for execution: " + task.getTaskId());
		String taskId = null;
		while (StringUtils.isBlank(taskId)) {
			try {
				final Future<Result<S>> taskFuture = submitTask(task);
				taskId = task.getTaskId();
				tasks.put(taskId, taskFuture);
			} catch (final CapacityExceededException e) {
				/* Check for cancel request to avoid any endless loops, in case the submission is stuck here. */
				checkTaskCanceled(task);
				
				LOG.trace(() -> "Cluster capacity exceeded. Gathering all task results and re-trying to submit: " + task.getTaskId());
				/* Wait for all submitted tasks to be finished and consume their results before re-trying to submit. */
				handleTaskResults(resultConsumer);
			}
		}
	}
	
	/**
	 * Submits one single {@link Task} for execution.
	 * 
	 * @param task the {@link Task} to submit
	 * @return the Future of the submitted {@link Task}
	 */
	protected Future<Result<S>> submitTask(final Task<S> task) {
		return jobManager.submit(jobMonitor, task);
	}
	
	/**
	 * Gathers the {@link Result} of all already submitted {@linkplain Task Tasks} and delegates them to the {@link ResultConsumer}.
	 * This method blocks until the tasks are finished.
	 * 
	 * @param resultConsumer the {@link ResultConsumer} consuming task {@linkplain Result Results}
	 */
	protected void handleTaskResults(final ResultConsumer<S> resultConsumer) {
		LOG.debug(() -> "Gathering task results.");
		tasks.entrySet().removeIf(entry -> {
			try {
				final String taskId = entry.getKey();
				LOG.trace(() -> "Getting task result for: " + taskId);
				resultConsumer.accept(entry.getKey(), entry.getValue().get());
			} catch (final InterruptedException | ExecutionException e) {
				/* This will only happen if there is something totally wrong, like a cluster failure. */
				Thread.currentThread().interrupt();
				throw new IllegalStateException("Unable to retrieve result of task: " + entry.getKey(), e);
			}
			
			return true;
		});
	}
	
	/**
	 * Checks whether the given task has been canceled. 
	 * 
	 * @param task the Task
	 * @throws OperationCanceledException if the task has been canceled
	 */
	protected void checkTaskCanceled(final Task<?> task) {
		task.getProgressMonitor().checkCanceled();
	}
	
	private void waitForNonCollectedTasks() {
		/* Get all task results, discarding any results and exceptions. */
		LOG.trace(() -> "Waiting for all non collected task results.");
		tasks.entrySet().removeIf(entry -> {
			try {
				entry.getValue().get();
			} catch (final Exception e) {
				LOG.error(() -> "Error while collecting result of task: " + entry.getKey(), e);
				if (e instanceof InterruptedException) {
					Thread.currentThread().interrupt();
				}
				return false;
			}
			return true;
		});
		
		/* Update the cluster information by subtracting the amount of tasks whose future threw an exception.
		 * All tasks terminated in a controlled fashion updated this information by themself. */
		jobManager.getClusterInformation().modifyActiveTaskCount(-tasks.size());
	}
	
}
