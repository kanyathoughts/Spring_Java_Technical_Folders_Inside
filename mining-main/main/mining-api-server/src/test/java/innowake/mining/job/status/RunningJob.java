/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.job.status;

import java.io.Serializable;
import java.util.concurrent.TimeUnit;

import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;


/**
 * A dummy job that runs for 10 seconds with no outcome.
 */
@WithMockUser(username = "ad")
public class RunningJob extends Job<Serializable> {

	@Override
	protected Result<Serializable> run(ProgressMonitor progressMonitor) {
		for (int i = 0; i < 10; i++) {
			progressMonitor.checkCanceled();
			try {
				TimeUnit.SECONDS.sleep(1);
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}
		}

		return new Result<Serializable>(Status.OK);
	}

}
