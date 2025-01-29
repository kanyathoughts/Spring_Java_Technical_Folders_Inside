/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.management;

import java.io.Serializable;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * Provides information of a job that is either currently being executed or already finished.
 */
public interface JobInformation {

	/**
	 * @return the {@link UUID} of the job
	 */
	UUID getId();

	/**
	 * @return the Id of the job
	 */
	default String getJobId() {
		return getId().toString();
	}
	
	/**
	 * @return the name of the job
	 */
	String getJobName();
	
	/**
	 * @return the name of the user that this job belongs to
	 */
	String getUserName();

	/**
	 * @return the description of the job
	 */
	@Nullable
	String getJobDescription();

	/**
	 * @return the description of the job step currently being executed
	 */
	@Nullable
	String getStepDescription();

	/**
	 * @return the current {@link JobStatus} of the job
	 */
	JobStatus getStatus();

	/**
	 * @return the time (UTC) the job has been submitted for execution
	 */
	Instant getSubmitTime();

	/**
	 * @return the time (UTC) the job has been scheduled to start its execution
	 */
	@Nullable
	Instant getScheduledStartTime();

	/**
	 * @return the time (UTC) the job execution had started
	 */
	@Nullable
	Instant getStartTime();

	/**
	 * @return the time (UTC) the job execution had finished
	 */
	@Nullable
	Instant getFinishTime();

	/**
	 * @return the overall duration of the job execution
	 */
	@Nullable
	default Duration getDuration() {
		final var finishTime = getFinishTime();
		if (finishTime != null) {
			final var startTime = getStartTime();
			if (startTime != null) {
				return Duration.between(startTime, finishTime);
			}
		}

		return null;
	}

	/**
	 * @return the estimated time when the job execution will be finished
	 */
	@Nullable
	default Instant getEta() {
		final var startTime = getStartTime();
		final var finishTime = getFinishTime();
		final var total = getTotalWorkUnits();
		final var worked = getProcessedWorkUnits();

		if (finishTime != null || startTime == null || total == ProgressMonitor.INDETERMINISTIC || Double.compare(worked, 0.0d) == 0) {
			return null;
		} else {
			final long millisConsumed = Duration.between(startTime, Instant.now()).toMillis();
			final long millisOverall = (long) ((millisConsumed * total) / worked);
			final long millisRemaining = Math.max(0, millisOverall - millisConsumed);
			return Instant.now().plusMillis(millisRemaining);
		}
	}

	/**
	 * @return the job specific {@link Result} or {@code null} if not available
	 */
	@Nullable
	Result<Serializable> getResult();

	/**
	 * @return the amount of forked tasks that are currently being executed or waiting to be executed
	 */
	int getPendingTasks();

	/**
	 * @return the total amount of work units defined for the job by {@link ProgressMonitor#begin(int)}
	 */
	int getTotalWorkUnits();

	/**
	 * @return the current amount of work units that are already consumed by the job
	 */
	double getProcessedWorkUnits();

	/**
	 * @return an unmodifiable list of {@linkplain Message Messages} that have been written during the execution of the job
	 */
	List<Message> getMessages();

}
