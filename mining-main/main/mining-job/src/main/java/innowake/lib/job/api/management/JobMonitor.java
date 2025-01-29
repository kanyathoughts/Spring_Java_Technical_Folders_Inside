/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.management;

import java.time.Instant;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * The job monitor for one single job.
 */
public interface JobMonitor extends ProgressMonitor {
	
	/**
	 * @return the Id of the job that belongs to this monitor
	 */
	String getJobId();
	
	/**
	 * @return the {@link JobInformation} of the job that belongs to this monitor. Can be {@code null} if {@link #destroy()} has already been called.
	 */
	@Nullable
	JobInformation getJobInformation();
	
	/**
	 * Modifies the amount of current pending tasks that belong to the job. 
	 *
	 * @param delta the delta to apply to the counter. Positive value to increment and negative value to decrement
	 * @return the amount of pending tasks after the delta has been applied
	 */
	int modifyPendingTasks(int delta);
	
	/**
	 * @return the amount of current pending tasks that belong to the job
	 */
	int getPendingTasks();

	/**
	 * Sets the overall amount of work units that have been processed on by the job.
	 *
	 * @param workUnits the amount of work units that have been processed
	 */
	void setProcessedWorkUnits(int workUnits);
	
	/**
	 * Sets the total amount of work units of the job.
	 *
	 * @param workUnits the total amount of work units
	 * @see {@link #begin(int)}
	 */
	void setTotalWorkUnits(int workUnits);
	
	/**
	 * @return the total amount of work units of this {@link ProgressMonitor}. Will be {@link #INDETERMINISTIC} in case the progress monitor
	 * is set to an indeterministic state.
	 */
	int getTotalWorkUnits();
	
	/**
	 * Sets the status of the job.
	 *
	 * @param jobStatus the {@link JobStatus}
	 */
	void setStatus(JobStatus jobStatus);
	
	/**
	 * @return the {@link JobStatus} of the job
	 */
	JobStatus getStatus();

	/**
	 * Sets the time when the job is scheduled to be started.
	 *
	 * @param scheduledStartTime the time when the job is scheduled to be started
	 */
	void setScheduledStartTime(Instant scheduledStartTime);
	
	/**
	 * Sets the time when the job execution has started.
	 *
	 * @param startTime the time when the job execution has started
	 */
	void setStartTime(Instant startTime);
	
	/**
	 * Sets the time when the job execution has finished.
	 *
	 * @param finishTime the time when the job execution has finished
	 */
	void setFinishTime(Instant finishTime);
	
	/**
	 * Adds a message to the job.
	 * 
	 * @param message the {@link Message}
	 */
	void addMessage(Message message);
	
	/**
	 * Updates the heartbeat of the job that is used to track if it's still alive.
	 */
	void updateHeartbeat();

	/**
	 * Releases all runtime resources associated with this job monitor.
	 */
	void destroy();
	
}
