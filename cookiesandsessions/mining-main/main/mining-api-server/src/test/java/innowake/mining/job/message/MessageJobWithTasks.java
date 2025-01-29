/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.message;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.IgnoreExceptionHandler;
import innowake.lib.job.api.task.Task;
import innowake.mining.shared.model.job.Message;

/**
 * Test job with tasks that write messages.
 */
public class MessageJobWithTasks extends Job<String> {

	@Override
	protected Result<String> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(3);
		progressMonitor.setJobDescription("Message job with tasks");

		writeMessage(Message.Severity.INFO, "Task 1");
		startTask(progressMonitor, Message.Severity.INFO, "Info message from task");
		writeMessage(Message.Severity.INFO, "Task 2");
		startTask(progressMonitor, Message.Severity.WARNING, "Warning message from task");
		writeMessage(Message.Severity.INFO, "Task 3");
		startTask(progressMonitor, Message.Severity.ERROR, "Error message from task");

		return new Result<String>(Status.OK);
	}

	private void startTask(final ProgressMonitor progressMonitor, final Message.Severity severity, final String text) {
		forkTask(createTaskProcessor(), new TestTask(severity, text, progressMonitor.subMonitor(1), jobId),
				new ResultConsumer<String>(new IgnoreExceptionHandler<>()) {

					@Override
					protected void handleResult(final String taskId, final Result<String> result) {
						/* do nothing */
					}

				});
	}

	private static class TestTask extends Task<String> {

		private final Message.Severity severity;
		private final String text;

		private TestTask(final Message.Severity severity, final String text, final ProgressMonitor progressMonitor, final String jobId) {
			super(progressMonitor, jobId);
			this.severity = severity;
			this.text = text;
		}

		@Override
		protected Result<String> run(final ProgressMonitor progressMonitor) {
			progressMonitor.begin(1);
			writeMessage(severity, text);
			progressMonitor.worked(1);
			return new Result<String>(Status.OK);
		}
	}
}
