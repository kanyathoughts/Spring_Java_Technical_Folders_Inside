/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import static innowake.lib.core.lang.Assert.assertNull;

import java.io.Serializable;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * Modifiable {@link JobInformation} implementation that contains all job data.
 */
public class JobInfo implements JobInformation, Serializable {

	private static final String UNASSIGNED = "unassigned";
	private static final long serialVersionUID = 1L;

	private final UUID id;
	private String jobName = UNASSIGNED;
	private String userName = UNASSIGNED;
	@Nullable
	private String description;
	@Nullable
	private String stepDescription;
	private Instant submitTime = Instant.now();
	@Nullable
	private Instant scheduledStartTime;
	@Nullable
	private Instant startTime;
	@Nullable
	private Instant finishTime;
	private JobStatus status = JobStatus.UNKNOWN;
	@Nullable
	private Result<Serializable> result;
	private int pendingTasks;
	private int total;
	private double worked;
	private List<Message> messages = new ArrayList<>();
	private Instant heartbeat = Instant.now();

	/**
	 * Constructor.
	 *
	 * @param jobId the globally unique Id of the job
	 * @param jobName the name of the job
	 * @param userName the name of the user the job belongs to
	 */
	public JobInfo(final String jobId, final String jobName, final String userName) {
		this.id = UUID.fromString(jobId);
		this.jobName = jobName;
		this.userName = userName;
		this.startTime = Instant.now();
	}

	@Override
	public UUID getId() {
		return id;
	}

	@Override
	public String getUserName() {
		return userName;
	}

	/**
	 * Sets the description of the job.
	 *
	 * @param description the description of the job
	 */
	public void setJobDescription(final String description) {
		this.description = description;
	}

	@Nullable
	@Override
	public String getJobDescription() {
		return description;
	}

	/**
	 * Sets the description of the current job step.
	 *
	 * @param stepDescription the job step description
	 */
	public void setStepDescription(final String stepDescription) {
		this.stepDescription = stepDescription;
	}

	@Nullable
	@Override
	public String getStepDescription() {
		return stepDescription;
	}

	/**
	 * Sets the time when the job has been submitted for execution.
	 *
	 * @param submitTime the {@link Instant}
	 */
	public void setSubmitTime(final Instant submitTime) {
		this.submitTime = submitTime;
	}

	@Override
	public Instant getSubmitTime() {
		return submitTime;
	}

	/**
	 * Sets the time the job is being scheduled to start.
	 *
	 * @param scheduledStartTime the time the job is being scheduled to start
	 */
	public void setScheduledStartTime(final Instant scheduledStartTime) {
		this.scheduledStartTime = scheduledStartTime;
	}

	@Nullable
	@Override
	public Instant getScheduledStartTime() {
		return scheduledStartTime;
	}

	/**
	 * Sets the time when the job execution has started.
	 *
	 * @param startTime the time when the job execution has started
	 */
	public void setStartTime(final Instant startTime) {
		this.startTime = startTime;
	}

	@Nullable
	@Override
	public Instant getStartTime() {
		return startTime;
	}

	/**
	 * Sets the time when the job execution has finished.
	 *
	 * @param finishTime the time when the job execution has finished
	 */
	public void setFinishTime(final Instant finishTime) {
		assertNull(this.finishTime, "Finish time of job " + getJobId() + " was already set");
		this.finishTime = finishTime;
	}

	@Nullable
	@Override
	public Instant getFinishTime() {
		return finishTime;
	}

	/**
	 * Sets the status of the job.
	 *
	 * @param status the {@link JobStatus}
	 */
	public void setStatus(final JobStatus status) {
		this.status = status;
	}

	@Override
	public JobStatus getStatus() {
		return status;
	}

	/**
	 * Modifies the amount of current pending tasks that belong to the job.
	 *
	 * @param delta the delta to apply to the counter. Positive value to increment and negative value to decrement
	 * @return the amount of pending tasks after the delta has been applied
	 */
	public final int modifyPendingTasks(final int delta) {
		pendingTasks += delta;
		return pendingTasks;
	}

	@Override
	public int getPendingTasks() {
		return pendingTasks;
	}

	/**
	 * Sets the total amount of work units of the job.
	 *
	 * @param total the total amount of work units
	 */
	public void setTotalWorkUnits(final int total) {
		this.total = total;
	}

	@Override
	public int getTotalWorkUnits() {
		return total;
	}

	/**
	 * Sets the overall amount of work units that have been processed on by the job.
	 *
	 * @param worked the amount of work units that have been processed
	 */
	public void setProcessedWorkUnits(final double worked) {
		this.worked = worked;
	}

	@Override
	public double getProcessedWorkUnits() {
		return worked;
	}

	/**
	 * Sets the result of the job.
	 *
	 * @param result the serializable result
	 */
	public void setResult(final Result<Serializable> result) {
		this.result = result;
	}

	@Nullable
	@Override
	public Result<Serializable> getResult() {
		return result;
	}

	/**
	 * Adds a {@link Message} to the existing ones.
	 *
	 * @param message the {@link Message} to add
	 */
	public void addMessage(final Message message) {
		messages.add(message);
	}

	/**
	 * Sets the {@linkplain Message Messages} for this job.
	 *
	 * @param messages the {@linkplain Message Messages}
	 */
	public void setMessages(final List<Message> messages) {
		this.messages = messages;
	}

	@Override
	public List<Message> getMessages() {
		return Collections.unmodifiableList(messages);
	}

	/**
	 * Updates the heartbeat to the current time.
	 */
	public void updateHeartbeat() {
		this.heartbeat = Instant.now();
	}

	/**
	 * @return the last heartbeat that has been recorded for the job
	 */
	public Instant getLastHeartbeat() {
		return heartbeat;
	}

	/**
	 * Transitions the internal job status to the provided one if allowed.
	 *
	 * @param status the {@link JobStatus} to transition to
	 */
	public final void nextStatus(final JobStatus status) {
		switch (status) {
			case UNKNOWN:
				/* the initial default status */
				break;
			case SCHEDULED:
				switchStatus(EnumSet.of(JobStatus.UNKNOWN), status);
				break;
			case RUNNING:
				switchStatus(EnumSet.of(JobStatus.SCHEDULED), status);
				break;
			case CANCEL_REQUESTED:
				switchStatus(EnumSet.of(JobStatus.CANCEL_REQUESTED, JobStatus.SCHEDULED, JobStatus.RUNNING), status);
				break;
			case CANCELED:
			case SUCCESS:
			case FAILURE:
			case TIMEOUT:
				switchStatus(EnumSet.of(JobStatus.CANCEL_REQUESTED, JobStatus.RUNNING), status);
				break;
			default:
				throw new IllegalArgumentException("Unsupported job status: " + status.name());
		}
	}

	/**
	 * Increments the total progress of the job by the provided amount of work.
	 *
	 * @param work the amount of work that has been completed
	 */
	public final void internalWork(final double work) {
		worked = Math.min(total, worked + work);
	}

	private final void switchStatus(final EnumSet<JobStatus> from, final JobStatus to) {
		if (status != to) {
			if (from.contains(status)) {
				status = to;
			} else {
				throw new IllegalArgumentException("Transition of job status current=" + status + " from=" + from + " to=" + to + " is not allowed");
			}
		}
	}

	@Override
	public String getJobName() {
		return this.jobName;
	}
}
