/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.status;

import java.util.concurrent.TimeUnit;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;

/**
 * Test job for cancel handling.
 */
public class CancelJob extends Job<String> {
	
	public enum ExecutionMode {
		CANCEL_SCHEDULED_JOB,
		RUN_SUCCESSFUL,
		IGNORE_CANCEL_REQUEST,
		RETURN_CANCEL_RESULT,
		CANCEL_RUNNING_JOB
	}

	private final ExecutionMode executionMode;

	public CancelJob(final ExecutionMode executionMode) {
		this.executionMode = executionMode;
	}

	@Override
	protected Result<String> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(10);
		progressMonitor.setJobDescription("Cancel job");
		int i = 0;
		while (i < 5) {
			if (i == 2 && executionMode == ExecutionMode.RETURN_CANCEL_RESULT) {
				return new Result<String>(new Status(Severity.CANCELED));
			}
			
			/* simulating that the job never does any explicit cancel checks and also doesn't start
			 * any sub progress monitors and also doesn't call work(...) which would implicitly trigger the check. */
			if (executionMode != ExecutionMode.IGNORE_CANCEL_REQUEST) {
				progressMonitor.checkCanceled();
			}
			
			try {
				TimeUnit.SECONDS.sleep(1);
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
			}
			/* using internalWork(...) instead of worked(...) on purpose to circumvent the implicit cancel check */
			progressMonitor.internalWork(1);
			i++;
		}
		return new Result<String>(Status.OK, "Ran even if canceled");
	}
	
}
