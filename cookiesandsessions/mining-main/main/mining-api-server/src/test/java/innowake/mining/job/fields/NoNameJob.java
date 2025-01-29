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
 * A dummy job where the name of the job is not overridden. 
 */
public class NoNameJob extends Job<Serializable> {

	@Override
	protected Result<Serializable> run(ProgressMonitor progressMonitor) {
		return new Result<Serializable>(Status.OK);
	}

}
