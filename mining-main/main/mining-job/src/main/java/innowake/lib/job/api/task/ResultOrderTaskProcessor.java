/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.task;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.CapacityExceededException;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.internal.Logging;

/**
 * Special task processor that processes results in the order in which tasks finish, rather than the order in which they were submitted.
 * <p>
 * When using this task processor, keep in mind that the order in which the ResultConsumer receives the results will not be the same as the order
 * in which the TaskSource produced the tasks.
 * <p>
 * This approach has the following benefits:
 * <ul>
 * <li> higher throughput since slow tasks no longer block us from obtaining results of subsequent tasks (no "head of line blocking")
 * <li> smoother memory usage: results are consumed "immediately" when available so typically only a few result objects can queue up in memory
 * </ul>
 * Note: "immediately" means that available results are consumed whenever a new task is submitted and after the TaskSource has been exhausted.
 * 
 * @param <T> The concrete result type of the tasks being submitted
 */
public class ResultOrderTaskProcessor<T extends Serializable> extends DefaultTaskProcessor<T> {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.TASK_PROCESSOR);
	/**
	 * Time to wait for the next task to finish when the cluster can no longer accept new tasks.
	 * If the time expires, we will check if the scheduled task was canceled and otherwise continue waiting.
	 */
	private static final int JOIN_TASK_WAIT_TIME = 10; /* seconds */
	
	private static class TaskIdAndFuture<T extends Serializable> {
		
		final String taskId;
		final Future<Result<T>> taskFuture;

		public TaskIdAndFuture(final String taskId, final Future<Result<T>> taskFuture) {
			this.taskId = taskId;
			this.taskFuture = taskFuture;
		}

		@Override
		public int hashCode() {
			return Objects.hash(taskId);
		}

		@Override
		public boolean equals(@Nullable final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if ( ! (obj instanceof TaskIdAndFuture<?>)) {
				return false;
			}
			TaskIdAndFuture<?> other = (TaskIdAndFuture<?>) obj;
			return Objects.equals(taskId, other.taskId);
		}
	}
	
	private final BlockingQueue<TaskIdAndFuture<T>> finishedTasks = new LinkedBlockingQueue<>();
	private final Set<TaskIdAndFuture<T>> activeTasks = new HashSet<>();

	/**
	 * Constructor.
	 * 
	 * @param jobManager the {@link JobManager} used to submit {@linkplain Task Tasks}
	 * @param jobMonitor the {@link JobMonitor} of the {@link Job} forking the tasks
	 */
	public ResultOrderTaskProcessor(final JobManager jobManager, final JobMonitor jobMonitor) {
		super(jobManager, jobMonitor);
	}
	
	@Override
	public void process(final TaskSource<T> taskSource, final ResultConsumer<T> resultConsumer) {
		try {
			/* Iterate over all tasks provided by the task source and submit for execution. */
			for (final Task<T> task : taskSource) {
				submitNextTask(task, resultConsumer);
			}
			
			/* Get the result of all submitted tasks. This blocks until all tasks are done. */
			joinTasks(resultConsumer);
		} catch (final Exception e) {
			LOG.error(() -> "Error while processing tasks", e);
			/* In case there is any exception (i.e. cancel request or unhandled), we wait until all already submitted tasks are
			 * done and then re-throw the exception to the job. This is done to avoid any zombie tasks still running in the background,
			 * while the job is already being terminated and marked as failed. */
			discardRunningTasks();
			throw e;
		}
	}
	
	@Override
	protected void submitNextTask(final Task<T> task, final ResultConsumer<T> resultConsumer) {
		/* drain finished tasks */
		handleFinishedTasks(resultConsumer);
		
		boolean taskSubmittedSuccessfully = false;
		while ( ! taskSubmittedSuccessfully) {
			try {
				final Future<Result<T>> taskFuture = submitTask(task);
				
				/* this will never go wrong, lol */
				final CompletableFuture<Result<T>> completableTaskFuture = (CompletableFuture<Result<T>>) taskFuture;
				final CompletableFuture<Result<T>> afterComplete = completableTaskFuture.whenComplete((result, throwable) -> {
					onTaskCompleted(new TaskIdAndFuture<>(task.getTaskId(), taskFuture));
				});
				activeTasks.add(new TaskIdAndFuture<>(task.getTaskId(), afterComplete));
				LOG.trace(() -> "Task started: " + task.getTaskId() + " active tasks: " + activeTasks.size() + ", waiting finished tasks: " + finishedTasks.size());
				taskSubmittedSuccessfully = true;
			} catch (final CapacityExceededException e) {
				/* Check for cancel request to avoid any endless loops, in case the submission is stuck here. */
				checkTaskCanceled(task);
				/* no more capacity to submit new tasks - block and wait until at least one more task completes */
				joinNextTask(resultConsumer);
			}
		}
	}
	
	private void onTaskCompleted(final TaskIdAndFuture<T> task) {
		try {
			finishedTasks.put(task);
		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException(e);
		}
		LOG.trace(() -> "Task finished: " + task.taskId + " active tasks: " + activeTasks.size() + ", waiting finished tasks: " + finishedTasks.size());
	}
	
	private void handleFinishedTasks(final ResultConsumer<T> resultConsumer) {
		final List<TaskIdAndFuture<T>> finishedTasksSnapshot = new ArrayList<>(finishedTasks.size());
		finishedTasks.drainTo(finishedTasksSnapshot);
		
		for (final TaskIdAndFuture<T> task : finishedTasksSnapshot) {
			activeTasks.remove(task);
			try {
				resultConsumer.accept(task.taskId, task.taskFuture.get());
			} catch (final InterruptedException | ExecutionException e) {
				Thread.currentThread().interrupt();
				throw new IllegalStateException("Unable to retrieve result of task: " + task.taskId, e);
			}
		}
	}
	
	/**
	 * Waits a certain amount of time for any task to complete.
	 * <p>
	 * If a task completes within the configured time frame of {@value #JOIN_TASK_WAIT_TIME} seconds, its result is passed to the given
	 * {@code resultConsumer} and this method returns {@code true}. Otherwise {@code false} is returned
	 * to indicate that no task has completed.
	 *
	 * @param resultConsumer the {@link ResultConsumer} that should receive the task result
	 * @return {@code true} if a task was successfully joined, {@code false} otherwise
	 */
	private boolean joinNextTask(final ResultConsumer<T> resultConsumer){
		TaskIdAndFuture<T> nextFinishedTask = null;
		try {
			nextFinishedTask = finishedTasks.poll(JOIN_TASK_WAIT_TIME, TimeUnit.SECONDS);
			if (nextFinishedTask == null) {
				/* no task completed within the timeout */
				return false;
			}
			activeTasks.remove(nextFinishedTask);
			resultConsumer.accept(nextFinishedTask.taskId, nextFinishedTask.taskFuture.get());
			return true;
		} catch (final InterruptedException | ExecutionException e) {
			Thread.currentThread().interrupt();
			if (nextFinishedTask == null) {
				throw new IllegalStateException("Interrupted while waiting for the next task", e);
			} else {
				throw new IllegalStateException("Unable to retrieve result of task: " + nextFinishedTask.taskId, e);
			}
		}
	}
	
	private void joinTasks(final ResultConsumer<T> resultConsumer) {
		for (final TaskIdAndFuture<T> task : activeTasks) {
			try {
				resultConsumer.accept(task.taskId, task.taskFuture.get());
			} catch (final InterruptedException | ExecutionException e) {
				Thread.currentThread().interrupt();
				throw new IllegalStateException("Unable to retrieve result of task: " + task.taskId, e);
			}
		}
		activeTasks.clear();
		/* all active tasks have been collected synchronously, so clear the finishedTask queue */
		finishedTasks.clear();
		/* there should be no active or waiting tasks now */
		LOG.trace(() -> "All tasks joined: active tasks: " + activeTasks.size() + ", waiting finished tasks: " + finishedTasks.size());
	}
	
	private void discardRunningTasks() {
		LOG.warn(() -> "Fatal exception encountered - discarding all running tasks");
		/* Get all task results, discarding any results and exceptions. */
		for (final TaskIdAndFuture<T> task : activeTasks) {
			try {
				task.taskFuture.get();
			} catch (final Exception e) {
				LOG.error(() -> "Error while collecting result of task: " + task.taskId, e);
				if (e instanceof InterruptedException) {
					Thread.currentThread().interrupt();
				}
			}
		}
		activeTasks.clear();
		/* all active tasks have been collected synchronously, so clear the finishedTask queue */
		finishedTasks.clear();
		/* there should be no active or waiting tasks now */
		LOG.trace(() -> "All running tasks discarded: active tasks: " + activeTasks.size() + ", waiting finished tasks: " + finishedTasks.size());
	}
}
