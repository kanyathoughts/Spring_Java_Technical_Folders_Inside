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
 * Test job for failure and exception handling.
 */
public class FailureJob extends Job<String> {
	
	public enum ExecutionMode {
		
		RESULT_WITH_WARNING_SEVERITY,
		RESULT_WITH_ERROR_SEVERITY,
		RESULT_WITH_STATUS_EXCEPTION,
		THROW_UNHANDLED_EXCEPTION,
		THROW_ERROR
	}

	private final ExecutionMode executionMode;

	public FailureJob(final ExecutionMode executionMode) {
		this.executionMode = executionMode;
	}

	@Override
	protected Result<String> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(10);
		progressMonitor.setJobDescription("Failure job");

		int i = 0;
		while (i < 5) {
			if (i == 2) {
				switch (executionMode) {
					case RESULT_WITH_WARNING_SEVERITY:
						return new Result<>(new Status(Severity.WARNING));
					case RESULT_WITH_ERROR_SEVERITY:
						return new Result<>(new Status(Severity.ERROR));
					case RESULT_WITH_STATUS_EXCEPTION:
						return new Result<>(new Status(new IllegalStateException("my special exception")));
					case THROW_UNHANDLED_EXCEPTION:
						throw new IllegalStateException("random unhandled exception");
					case THROW_ERROR:
						throw new Error("something has gone seriously wrong");
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
