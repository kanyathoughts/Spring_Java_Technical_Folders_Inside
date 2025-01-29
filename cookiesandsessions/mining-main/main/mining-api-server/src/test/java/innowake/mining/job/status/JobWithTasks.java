/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.status;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskExceptionHandler;
import innowake.lib.job.api.task.TaskSource;

/**
 * Test job with multiple tasks and different status values.
 */
public class JobWithTasks extends Job<String> {

	@Override
	protected Result<String> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(25);
		progressMonitor.setJobDescription("Job with tasks");

		final Map<String, TestTask.ExecutionMode> executionModes = new HashMap<>();
		final int[] taskCount = new int[1];
		final int[] resultCount = new int[1];
		final int[] exceptionCount = new int[1];
		final TaskSource<String> taskSource = new TaskSource<String>() {

			@Override
			public boolean hasNextTask() {
				return taskCount[0] < 6;
			}

			@Override
			public Task<String> nextTask() {
				final ProgressMonitor subProgressMonitor = progressMonitor.subMonitor(1);
				final TestTask task;
				switch (taskCount[0]++) {
					case 0:
						task = new TestTask(TestTask.ExecutionMode.RUN_SUCCESSFUL, subProgressMonitor, jobId);
						break;
					case 1:
						task = new TestTask(TestTask.ExecutionMode.RESULT_WITH_WARNING_SEVERITY, subProgressMonitor, jobId);
						break;
					case 2:
						task = new TestTask(TestTask.ExecutionMode.RESULT_WITH_ERROR_SEVERITY, subProgressMonitor, jobId);
						break;
					case 3:
						task = new TestTask(TestTask.ExecutionMode.RESULT_WITH_STATUS_EXCEPTION, subProgressMonitor, jobId);
						break;
					case 4:
						task = new TestTask(TestTask.ExecutionMode.THROW_UNHANDLED_EXCEPTION, subProgressMonitor, jobId);
						break;
					case 5:
						task = new TestTask(TestTask.ExecutionMode.THROW_ERROR, subProgressMonitor, jobId);
						break;
					default:
						throw new IllegalStateException("Unexpected task count: " + taskCount);
				}
				executionModes.put(task.getTaskId(), task.executionMode);
				return task;
			}
		};

		final TaskExceptionHandler<String> exceptionHandler = new TaskExceptionHandler<String>() {

			@Override
			public void handle(final String taskId, final Result<String> result) {
				final TestTask.ExecutionMode executionMode = executionModes.get(taskId);
				if (executionMode != TestTask.ExecutionMode.RESULT_WITH_STATUS_EXCEPTION
						&& executionMode != TestTask.ExecutionMode.THROW_UNHANDLED_EXCEPTION
						&& executionMode != TestTask.ExecutionMode.THROW_ERROR) {
					throw new IllegalStateException("Unexpected task with execution mode: " + executionMode.name());
				}

				exceptionCount[0]++;
			}
		};

		final ResultConsumer<String> resultConsumer = new ResultConsumer<String>(exceptionHandler) {

			@Override
			protected void handleResult(final String taskId, final Result<String> result) {
				resultCount[0]++;

				assertNotNull(result);
				final Severity severity = result.status.getSeverity();
				final String message = result.status.getMessage();

				switch (executionModes.get(taskId)) {
					case RUN_SUCCESSFUL:
						assertEquals(Severity.OK, severity);
						assertNull(message);
						break;
					case RESULT_WITH_WARNING_SEVERITY:
						assertEquals(Severity.WARNING, severity);
						assertNull(message);
						break;
					case RESULT_WITH_ERROR_SEVERITY:
						assertEquals(Severity.ERROR, severity);
						assertNull(message);
						break;
					case RESULT_WITH_STATUS_EXCEPTION:
						assertEquals(Severity.ERROR, severity);
						assertEquals("my special exception", message);
						break;
					case THROW_UNHANDLED_EXCEPTION:
						assertEquals(Severity.ERROR, severity);
						assertEquals("random unhandled exception", message);
						break;
					case THROW_ERROR:
						assertEquals(Severity.ERROR, severity);
						assertEquals("something has gone seriously wrong", message);
						break;
					default:
						throw new IllegalStateException("Unexpected execution mode: " + executionModes.get(taskId));
				}
			}
		};

		forkTasks(createTaskProcessor(), taskSource, resultConsumer);

		assertEquals(6, taskCount[0]);
		assertEquals(6, resultCount[0]);
		assertEquals(3, exceptionCount[0]);

		final Severity severity = resultConsumer.getHighestSeverity();
		assertEquals(Severity.ERROR, severity);

		return new Result<String>(new Status(severity));
	}

	private static class TestTask extends Task<String> {

		public enum ExecutionMode {

			RUN_SUCCESSFUL,
			RESULT_WITH_WARNING_SEVERITY,
			RESULT_WITH_ERROR_SEVERITY,
			RESULT_WITH_STATUS_EXCEPTION,
			THROW_UNHANDLED_EXCEPTION,
			THROW_ERROR
		}

		private final ExecutionMode executionMode;

		private TestTask(final ExecutionMode executionMode, final ProgressMonitor progressMonitor, final String jobId) {
			super(progressMonitor, jobId);
			this.executionMode = executionMode;
		}

		@Override
		protected Result<String> run(final ProgressMonitor progressMonitor) {
			progressMonitor.begin(5);
			int i = 0;
			while (i < 5) {
				if (i == 2) {
					switch (executionMode) {
						case RESULT_WITH_WARNING_SEVERITY:
							return new Result<>(new Status(Severity.WARNING), "Some result");
						case RESULT_WITH_ERROR_SEVERITY:
							return new Result<>(new Status(Severity.ERROR), "Some result");
						case RESULT_WITH_STATUS_EXCEPTION:
							return new Result<>(new Status(new IllegalStateException("my special exception")), "Some result");
						case THROW_UNHANDLED_EXCEPTION:
							throw new IllegalStateException("random unhandled exception");
						case THROW_ERROR:
							throw new Error("something has gone seriously wrong");
						case RUN_SUCCESSFUL:
						default:
							break;
					}
				}

				try {
					TimeUnit.SECONDS.sleep(1);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
				}
				progressMonitor.worked(1);
				i++;
			}
			return new Result<String>(Status.OK);
		}
	}
}
