/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.job.fields;

import java.io.Serializable;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;


/**
 * A dummy job that overrides the job name.
 */
public class CustomNameJob extends Job<Serializable> {

	@Override
	protected Result<Serializable> run(ProgressMonitor progressMonitor) {
		return new Result<Serializable>(Status.OK);
	}

	@Override
	public String getJobName() {
		return "Example";
	}

}
