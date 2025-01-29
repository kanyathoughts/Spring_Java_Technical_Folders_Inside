/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.child;

import static innowake.lib.core.lang.Assert.assertNotNull;
import java.io.Serializable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;

/**
 * The parent test Job submitting the child test Job.
 */
public class ParentJobRunningChildJob extends Job<Serializable> {

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(1);
		progressMonitor.setJobDescription("Parent Job inside which another Job will be run");
		final ChildJob childJob = new ChildJob();
		progressMonitor.setStepDescription("child Job started");
		final Status childJobStatus = jobManager.submitFromJobAndWait(childJob, assertNotNull(jobMonitor));
		progressMonitor.setStepDescription("child Job status" + childJobStatus);
		progressMonitor.worked(1);
		return new Result<>(Status.OK);
	}
}
