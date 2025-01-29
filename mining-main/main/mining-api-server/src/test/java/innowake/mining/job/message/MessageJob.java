/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.message;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.shared.model.job.Message;

/**
 * Test job that writes messages with all three supported severities.
 */
public class MessageJob extends Job<String> {

	@Override
	protected Result<String> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(1);
		progressMonitor.setJobDescription("Message job");
		writeMessage(Message.Severity.INFO, "Info message");
		writeMessage(Message.Severity.WARNING, "Warning message");
		writeMessage(Message.Severity.ERROR, "Error message");
		progressMonitor.worked(1);
		return new Result<>(Status.OK);
	}
}
