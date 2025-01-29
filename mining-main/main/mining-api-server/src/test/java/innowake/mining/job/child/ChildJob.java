/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.child;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;

/**
 * Test child Job.
 */
public class ChildJob extends Job<String> {

	@Override
	protected Result<String> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(1);
		progressMonitor.setJobDescription("Child Job");
		progressMonitor.worked(1);
		return new Result<>(Status.OK);
	}
}
