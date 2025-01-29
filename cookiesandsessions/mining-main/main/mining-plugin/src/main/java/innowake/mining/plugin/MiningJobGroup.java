/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobGroup;

/**
 * This class will group common {@link Job} under a single name to avoid overlapping executions
 */
public class MiningJobGroup extends JobGroup {

	/**
	 * Singleton instance
	 */
	public static final MiningJobGroup INSTANCE = new MiningJobGroup("innowake.mining.jobs.MiningJobs", 1, 1);
	
	/**
	 * Creates the mining {@link JobGroup} with the specified name and maxThread.
	 * 
	 * @param name the name of the job group.
	 * @param maxThreads the maximum number of threads to be scheduled concurrently
	 * @param seedJobsCount the initial number of jobs that will be added to the job group
	 */
	private MiningJobGroup(String name, int maxThreads, int seedJobsCount) {
		super(name, maxThreads, seedJobsCount);
	}
}
