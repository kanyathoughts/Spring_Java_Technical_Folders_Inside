/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server;

import java.io.Serializable;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.EntityId;

/**
 * A dummy job that does nothing. This can be used in test cases related to job framework.
 */
public class TestJob extends MiningJob<Serializable> {

	public TestJob(final EntityId projectId, @Nullable final EntityId moduleId, final String jobName) {
		super(projectId, moduleId);
		this.jobName = jobName;
	}
	
	public TestJob(final EntityId projectId) {
		super(projectId);
		this.jobName = "";
	}
	
	public TestJob() {
		super(EntityId.VOID);
		this.jobName = "";
	}

	private String jobName;
	
	@Override
	protected Result<Serializable> run(ProgressMonitor progressMonitor) {
		return new Result<Serializable>(Status.OK);
	}

	@Override
	public String getJobName() {
		return jobName;
	}
	
	public TestJob(final String jobName) {
		super(EntityId.VOID);
		this.jobName = jobName;
	}

}
