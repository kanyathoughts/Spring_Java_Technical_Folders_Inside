/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.manager;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobGroup;

/**
 * Abstract base class for jobs updating the state of the {@link RemoteJobManager}.
 */
public abstract class AbstractUpdateJob extends Job {
	
	/* Only allow one job to be executed at a time. */
	private static final JobGroup JOB_GROUP = new JobGroup("job-info-update", 1, 1);
	
	protected final RemoteJobManager remoteJobManager;

	/**
	 * Constructor.
	 * 
	 * @param remoteJobManager the {@link RemoteJobManager} instance to be used by the jobs
	 */
	public AbstractUpdateJob(final RemoteJobManager remoteJobManager) {
		super("Job Information Update");
		setJobGroup(JOB_GROUP);
		setSystem(true);
		this.remoteJobManager = remoteJobManager;
	}
	
}
