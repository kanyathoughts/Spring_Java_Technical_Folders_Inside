/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.io.Serializable;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * {@code job_info} entity class.
 */
@MiningDataType(name = MiningEnitityNames.JOB_INFO)
public class JobInfoPojo implements Serializable {

	private final UUID id;
	private final String name;
	private final Optional<String> description;
	private final Optional<String> stepDescription;
	private final Optional<JobStatus> status;
	private final List<Message> messages;
	private final Optional<byte[]> result;
	private final int pendingTasks;
	private final int totalWorkUnits;
	private final double processedWorkUnits;
	private final Optional<Instant> submitTime;
	private final Optional<Instant> scheduledStartTime;
	private final Optional<Instant> startTime;
	private final Optional<Instant> finishTime;
	private final String createdByUserId;

	public JobInfoPojo(
			@JsonProperty("id") final UUID id,
			@JsonProperty("name") final String name,
			@JsonProperty("description") @JsonAlias("jobDescription") @Nullable final String description,
			@JsonProperty("stepDescription") @Nullable final String stepDescription,
			@JsonProperty("status") @Nullable final JobStatus status,
			@JsonProperty("messages") final List<Message> messages,
			@JsonProperty("result") @Nullable final byte[] result,
			@JsonProperty("pendingTasks") final int pendingTasks,
			@JsonProperty("totalWorkUnits") final int totalWorkUnits,
			@JsonProperty("processedWorkUnits") final double processedWorkUnits,
			@JsonProperty("submitTime") @Nullable final Instant submitTime,
			@JsonProperty("scheduledStartTime") @Nullable final Instant scheduledStartTime,
			@JsonProperty("startTime") @Nullable final Instant startTime,
			@JsonProperty("finishTime") @Nullable final Instant finishTime,
			@JsonProperty("createdByUserId") @JsonAlias("userName") final String createdByUserId) {
		this.id = id;
		this.name = name;
		this.description = Optional.ofNullable(description);
		this.stepDescription = Optional.ofNullable(stepDescription);
		this.status = Optional.ofNullable(status);
		this.messages = messages;
		this.result = Optional.ofNullable(result);
		this.pendingTasks = pendingTasks;
		this.totalWorkUnits = totalWorkUnits;
		this.processedWorkUnits = processedWorkUnits;
		this.submitTime = Optional.ofNullable(submitTime);
		this.scheduledStartTime = Optional.ofNullable(scheduledStartTime);
		this.startTime = Optional.ofNullable(startTime);
		this.finishTime = Optional.ofNullable(finishTime);
		this.createdByUserId = createdByUserId;
	}

	public UUID getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	public Optional<String> getDescription() {
		return description;
	}

	public Optional<String> getStepDescription() {
		return stepDescription;
	}

	public Optional<JobStatus> getStatus() {
		return status;
	}

	public List<Message> getMessages() {
		return messages;
	}

	public Optional<byte[]> getResult() {
		return result;
	}

	public int getPendingTasks() {
		return pendingTasks;
	}

	public int getTotalWorkUnits() {
		return totalWorkUnits;
	}

	public double getProcessedWorkUnits() {
		return processedWorkUnits;
	}

	public Optional<Instant> getSubmitTime() {
		return submitTime;
	}

	public Optional<Instant> getScheduledStartTime() {
		return scheduledStartTime;
	}

	public Optional<Instant> getStartTime() {
		return startTime;
	}

	public Optional<Instant> getFinishTime() {
		return finishTime;
	}

	public String getCreatedByUserId() {
		return createdByUserId;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("name", name)
				.append("description", description)
				.append("stepDescription", stepDescription)
				.append("status", status)
				.append("messages", messages)
				.append("result", result)
				.append("pendingTasks", pendingTasks)
				.append("totalWorkUnits", totalWorkUnits)
				.append("processedWorkUnits", processedWorkUnits)
				.append("submitTime", submitTime)
				.append("scheduledStartTime", scheduledStartTime)
				.append("startTime", startTime)
				.append("finishTime", finishTime)
				.append("createdByUserId", createdByUserId)
				.toString();
	}
}