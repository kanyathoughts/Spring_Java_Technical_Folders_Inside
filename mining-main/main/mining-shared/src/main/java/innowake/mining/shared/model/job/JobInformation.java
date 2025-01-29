/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.model.job;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.time.Instant;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.preset.AbstractBuilder;

/**
 * Contains information of a job that is either currently being executed or already finished.
 */
public class JobInformation {

	private final String jobId;
	private final String jobName;
	private final String userName;
	@Nullable
	private final String jobDescription;
	@Nullable
	private final String stepDescription;
	@Nullable
	private final Instant submitTime;
	@Nullable
	private final Instant scheduledStartTime;
	@Nullable
	private final Instant startTime;
	@Nullable
	private final Instant finishTime;
	@Nullable
	private final Instant eta;
	private final JobStatus status;
	@Nullable
	private final ResultStatus resultStatus;
	private final int totalWorkUnits;
	private final double processedWorkUnits;
	private final List<Message> messages;
	@Nullable
	private final Long projectId;
	@Nullable
	private final Long moduleId;

	private JobInformation() {
		/* This constructor is required for Jackson. */
		jobId = "";
		jobName = "";
		userName = "";
		jobDescription = null;
		stepDescription = null;
		submitTime = null;
		scheduledStartTime = null;
		startTime = null;
		finishTime = null;
		eta = null;
		status = JobStatus.UNKNOWN;
		resultStatus = null;
		totalWorkUnits = 0;
		processedWorkUnits = 0;
		messages = Collections.emptyList();
		projectId = null;
		moduleId = null;
	}

	private JobInformation(final Builder builder) {
		this.jobId = assertNotNull(builder.jobId);
		this.jobName = assertNotNull(builder.jobName);
		this.userName = assertNotNull(builder.userName);
		this.jobDescription = builder.jobDescription;
		this.stepDescription = builder.stepDescription;
		this.submitTime = builder.submitTime;
		this.scheduledStartTime = builder.scheduledStartTime;
		this.startTime = builder.startTime;
		this.finishTime = builder.finishTime;
		this.eta = builder.eta;
		this.status = builder.status;
		this.resultStatus = builder.resultStatus;
		this.totalWorkUnits = builder.totalWorkUnits;
		this.processedWorkUnits = builder.processedWorkUnits;
		this.messages = builder.messages;
		this.projectId = builder.projectId;
		this.moduleId = builder.moduleId;
	}

	/**
	 * @return the Id of the job
	 */
	public String getJobId() {
		return jobId;
	}

	/**
	 * @return the name of the job
	 */
	public String getJobName() {
		return jobName;
	}

	/**
	 * @return the name of the user that this job belongs to
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * @return the description of the job
	 */
	@Nullable
	public String getJobDescription() {
		return jobDescription;
	}

	/**
	 * @return the description of the job step currently being executed
	 */
	@Nullable
	public String getStepDescription() {
		return stepDescription;
	}

	/**
	 * @return the current {@link JobStatus} of the job
	 */
	public JobStatus getStatus() {
		return status;
	}

	/**
	 * @return the time (UTC) the job has been submitted for execution
	 */
	@Nullable
	public Instant getSubmitTime() {
		return submitTime;
	}

	/**
	 * @return the time (UTC) the job has been scheduled to start its execution
	 */
	@Nullable
	public Instant getScheduledStartTime() {
		return scheduledStartTime;
	}

	/**
	 * @return the time (UTC) the job execution had started
	 */
	@Nullable
	public Instant getStartTime() {
		return startTime;
	}

	/**
	 * @return the time (UTC) the job execution had finished
	 */
	@Nullable
	public Instant getFinishTime() {
		return finishTime;
	}

	/**
	 * @return the estimated time (UTC) when the job execution will be finished
	 */
	@Nullable
	public Instant getEta() {
		return eta;
	}

	/**
	 * @return the {@link ResultStatus} or {@code null} if not available
	 */
	@Nullable
	public ResultStatus getResultStatus() {
		return resultStatus;
	}

	/**
	 * @return the total amount of work units defined by the job
	 */
	public int getTotalWorkUnits() {
		return totalWorkUnits;
	}

	/**
	 * @return the current amount of work units that are already processed by the job
	 */
	public double getProcessedWorkUnits() {
		return processedWorkUnits;
	}

	/**
	 * @return an unmodifiable list of {@link Message}s that have been written during the execution of the job
	 */
	public List<Message> getMessages() {
		return messages;
	}

	@Nullable
	public Long getProjectId() {
		return projectId;
	}

	@Nullable
	public Long getModuleId() {
		return moduleId;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(jobId)
				.append(jobName)
				.append(userName)
				.append(jobDescription)
				.append(stepDescription)
				.append(submitTime)
				.append(scheduledStartTime)
				.append(startTime)
				.append(finishTime)
				.append(eta)
				.append(status)
				.append(resultStatus)
				.append(totalWorkUnits)
				.append(processedWorkUnits)
				.append(messages)
				.append(projectId)
				.append(moduleId)
				.toHashCode();
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final JobInformation other = (JobInformation) obj;
		return new EqualsBuilder().append(jobId, other.jobId)
			.append(jobName, other.jobName)
			.append(userName, other.userName)
			.append(jobDescription, other.jobDescription)
			.append(stepDescription, other.stepDescription)
			.append(submitTime, other.submitTime)
			.append(scheduledStartTime, other.scheduledStartTime)
			.append(startTime, other.startTime)
			.append(finishTime, other.finishTime)
			.append(eta, other.eta)
			.append(status, other.status)
			.append(resultStatus, other.resultStatus)
			.append(totalWorkUnits, other.totalWorkUnits)
			.append(processedWorkUnits, other.processedWorkUnits)
			.append(messages, other.messages)
			.append(projectId, other.projectId)
			.append(moduleId, other.moduleId)
			.isEquals();
	}

	/**
	 * Returns a new {@link Builder} with all values from the given {@code original}.
	 *
	 * @param original The source {@link JobInformation}
	 * @return the {@link Builder}.
	 */
	public static Builder copy(final JobInformation original) {
		final Builder builder = new Builder()
	            .setDescription(original.jobDescription)
	            .setEta(original.eta)
	            .setFinishTime(original.finishTime)
	            .setJobId(original.jobId)
	            .setMessages(original.messages)
	            .setScheduledStartTime(original.scheduledStartTime)
	            .setStartTime(original.startTime)
	            .setStatus(original.status)
	            .setStepDescription(original.stepDescription)
	            .setTotalWorkUnits(original.totalWorkUnits)
	            .setJobName(original.jobName)
	            .setUserName(original.userName)
	            .setWorked(original.processedWorkUnits)
	            .setProjectId(original.projectId)
	            .setModuleId(original.moduleId);

		if (original.resultStatus != null) {
			builder.setResultStatus(original.resultStatus);
		}
		if (original.submitTime != null) {
			builder.setSubmitTime(original.submitTime);
		}

		return builder;
    }

	/**
	 * Builder for {@link JobInformation}.
	 */
	public static class Builder extends AbstractBuilder<JobInformation, Builder> {

		@Nullable
		private String jobId;
		@Nullable
		private String jobName;
		@Nullable
		private String userName;
		@Nullable
		private String jobDescription;
		@Nullable
		private String stepDescription;
		@Nullable
		private Instant submitTime;
		@Nullable
		private Instant scheduledStartTime;
		@Nullable
		private Instant startTime;
		@Nullable
		private Instant finishTime;
		@Nullable
		private Instant eta;
		private JobStatus status = JobStatus.UNKNOWN;
		@Nullable
		private ResultStatus resultStatus;
		private int totalWorkUnits;
		private double processedWorkUnits;
		private List<Message> messages = Collections.emptyList();
		@Nullable
		private Long projectId;
		@Nullable
		private Long moduleId;

		/**
		 * Sets the unique Id of the job.
		 *
		 * @param jobId the Id of the job
		 * @return the {@link Builder} instance
		 */
		public Builder setJobId(final String jobId) {
			this.jobId = jobId;
			return getThis();
		}

		/**
		 * Sets the name of the job.
		 *
		 * @param jobName the Id of the job
		 * @return the {@link Builder} instance
		 */
		public Builder setJobName(final String jobName) {
			this.jobName = jobName;
			return getThis();
		}

		/**
		 * Sets the name of the user that the job is assigned to
		 *
		 * @param userName the name of the user
		 * @return the {@link Builder} instance
		 */
		public Builder setUserName(final String userName) {
			this.userName = userName;
			return getThis();
		}

		/**
		 * Sets the description of the job.
		 *
		 * @param description the description
		 * @return the {@link Builder} instance
		 */
		public Builder setDescription(@Nullable final String description) {
			this.jobDescription = description;
			return getThis();
		}

		/**
		 * Sets the description of the current running step.
		 *
		 * @param stepDescription the step description
		 * @return the {@link Builder} instance
		 */
		public Builder setStepDescription(@Nullable final String stepDescription) {
			this.stepDescription = stepDescription;
			return getThis();
		}

		/**
		 * Sets the {@link Instant} (UTC) the job has been submitted for execution.
		 *
		 * @param submitTime the {@link Instant}
		 * @return the {@link Builder} instance
		 */
		public Builder setSubmitTime(final Instant submitTime) {
			this.submitTime = submitTime;
			return getThis();
		}

		/**
		 * Sets the {@link Instant} (UTC) the job has been scheduled to start.
		 *
		 * @param scheduledStartTime the {@link Instant}
		 * @return the {@link Builder} instance
		 */
		public Builder setScheduledStartTime(@Nullable final Instant scheduledStartTime) {
			this.scheduledStartTime = scheduledStartTime;
			return getThis();
		}

		/**
		 * Sets the {@link Instant} (UTC) the job execution has started.
		 *
		 * @param startTime the {@link Instant}
		 * @return the {@link Builder} instance
		 */
		public Builder setStartTime(@Nullable final Instant startTime) {
			this.startTime = startTime;
			return getThis();
		}

		/**
		 * Sets the {@link Instant} (UTC) the job execution has finished.
		 *
		 * @param finishTime the {@link Instant}
		 * @return the {@link Builder} instance
		 */
		public Builder setFinishTime(@Nullable final Instant finishTime) {
			this.finishTime = finishTime;
			return getThis();
		}

		/**
		 * Sets the {@link Instant} (UTC) providing the ETA of the job execution.
		 *
		 * @param eta the {@link Instant}
		 * @return the {@link Builder} instance
		 */
		public Builder setEta(@Nullable final Instant eta) {
			this.eta = eta;
			return getThis();
		}

		/**
		 * Sets the {@link JobStatus} of the job.
		 *
		 * @param status the {@link JobStatus}
		 * @return the {@link Builder} instance
		 */
		public Builder setStatus(final JobStatus status) {
			this.status = status;
			return getThis();
		}

		/**
		 * Sets the {@link ResultStatus} of the job.
		 *
		 * @param resultStatus the {@link ResultStatus}
		 * @return the {@link Builder} instance
		 */
		public Builder setResultStatus(final ResultStatus resultStatus) {
			this.resultStatus = resultStatus;
			return getThis();
		}

		/**
		 * Sets the total work units of the job.
		 *
		 * @param totalWorkUnits the total work units
		 * @return the {@link Builder} instance
		 */
		public Builder setTotalWorkUnits(final int totalWorkUnits) {
			this.totalWorkUnits = totalWorkUnits;
			return getThis();
		}

		/**
		 * Sets the already processed work units of the job.
		 *
		 * @param processedWorkUnits the processed work units
		 * @return the {@link Builder} instance
		 */
		public Builder setWorked(final double processedWorkUnits) {
			this.processedWorkUnits = processedWorkUnits;
			return getThis();
		}

		/**
		 * Sets the {@link Message}s emitted by the job.
		 *
		 * @param messages the {@link Message}s
		 * @return the {@link Builder} instance
		 */
		public Builder setMessages(final List<Message> messages) {
			this.messages = messages;
			return getThis();
		}


		public Builder setProjectId(@Nullable final Long projectId) {
			this.projectId = projectId;
			return getThis();
		}


		public Builder setModuleId(@Nullable final Long moduleId) {
			this.moduleId = moduleId;
			return getThis();
		}

		@Override
		protected Builder reset() {
			jobId = null;
			jobName = null;
			userName = null;
			jobDescription = null;
			stepDescription = null;
			submitTime = null;
			scheduledStartTime = null;
			startTime = null;
			finishTime = null;
			eta = null;
			status = JobStatus.UNKNOWN;
			resultStatus = null;
			totalWorkUnits = 0;
			processedWorkUnits = 0;
			messages = Collections.emptyList();
			projectId = null;
			moduleId = null;
			return getThis();
		}

		@Override
		protected JobInformation internalBuild() {
			return new JobInformation(this);
		}

	}
}
