/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.job;

import java.util.EnumSet;
import java.util.Set;

/**
 * The execution status of a {@code Job}.
 */
public enum JobStatus {
	
	/** The status of the job is not known. */
	UNKNOWN,
	
	/** The job has been scheduled for execution. */
	SCHEDULED,
	
	/** The job is currently being executed. */
	RUNNING,
	
	/** The job finished successfully. */
	SUCCESS,
	
	/** The job finished with errors. */
	FAILURE,
	
	/** The job has been canceled because it ran into a timeout. */
	TIMEOUT,
	
	/** It has been requested to cancel the job. */
	CANCEL_REQUESTED,
	
	/** The job has been canceled. */
	CANCELED;
	
	/**
	 * Job states in which the job is considered "active" or "not finished", i.e. either currently executing
	 * or scheduled for execution.
	 */
	public static final Set<JobStatus> ACTIVE_JOB_STATUS = EnumSet.of(RUNNING, SCHEDULED, CANCEL_REQUESTED);
	
	/**
	 * Returns {@code true} if the given {@code JobStatus} is considered "active" or "not finished", i.e. the job is either currently executing
	 * or scheduled for execution.
	 * 
	 * @param jobStatus the {@code JobStatus} to check 
	 * @return  {@code true} if the job status is "active" or {@code false} otherwise
	 */
	public static boolean isActive(final JobStatus jobStatus) {
		return ACTIVE_JOB_STATUS.contains(jobStatus);
	}
}
