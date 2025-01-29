/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.time.Instant;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.model.job.JobStatus;

/**
 * {@code job_info} entity request class.
 */
public class JobInfoPojoPrototype implements PojoPrototype {

	public final Definable<UUID> id = new Definable<>(false, "JobInfo.id");
	public final Definable<String> name = new Definable<>(false, "JobInfo.name");
	public final Definable<String> description = new Definable<>(true, "JobInfo.description");
	public final Definable<String> stepDescription = new Definable<>(true, "JobInfo.stepDescription");
	public final Definable<JobStatus> status = new Definable<>(true, "JobInfo.status");
	public final Definable<Integer> pendingTasks = new Definable<>(true, "JobInfo.pendingTasks");
	public final Definable<Integer> totalWorkUnits = new Definable<>(true, "JobInfo.totalWorkUnits");
	public final Definable<Double> processedWorkUnits = new Definable<>(true, "JobInfo.processedWorkUnits");
	public final Definable<Instant> submitTime = new Definable<>(true, "JobInfo.submitTime");
	public final Definable<Instant> scheduledStartTime = new Definable<>(true, "JobInfo.scheduledStartTime");
	public final Definable<Instant> startTime = new Definable<>(true, "JobInfo.startTime");
	public final Definable<Instant> finishTime = new Definable<>(true, "JobInfo.finishTime");
	public final Definable<String> createdByUserId = new Definable<>(false, "JobInfo.createdByUserId");

	public JobInfoPojoPrototype setId(final UUID id) {
		this.id.set(id);
		return this;
	}

	public JobInfoPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}

	public JobInfoPojoPrototype setDescription(@Nullable final String description) {
		this.description.set(description);
		return this;
	}

	public JobInfoPojoPrototype setStepDescription(@Nullable final String stepDescription) {
		this.stepDescription.set(stepDescription);
		return this;
	}

	public JobInfoPojoPrototype setStatus(final JobStatus status) {
		this.status.set(status);
		return this;
	}

	public JobInfoPojoPrototype setPendingTasks(@Nullable final Integer pendingTasks) {
		this.pendingTasks.set(pendingTasks);
		return this;
	}

	public JobInfoPojoPrototype setTotalWorkUnits(@Nullable final Integer totalWorkUnits) {
		this.totalWorkUnits.set(totalWorkUnits);
		return this;
	}

	public JobInfoPojoPrototype setProcessedWorkUnits(@Nullable final Double processedWorkUnits) {
		this.processedWorkUnits.set(processedWorkUnits);
		return this;
	}

	public JobInfoPojoPrototype setSubmitTime(@Nullable final Instant submitTime) {
		this.submitTime.set(submitTime);
		return this;
	}

	public JobInfoPojoPrototype setScheduledStartTime(@Nullable final Instant scheduledStartTime) {
		this.scheduledStartTime.set(scheduledStartTime);
		return this;
	}

	public JobInfoPojoPrototype setStartTime(@Nullable final Instant startTime) {
		this.startTime.set(startTime);
		return this;
	}

	public JobInfoPojoPrototype setFinishTime(@Nullable final Instant finishTime) {
		this.finishTime.set(finishTime);
		return this;
	}

	public JobInfoPojoPrototype setCreatedByUserId(final String createdByUserId) {
		this.createdByUserId.set(createdByUserId);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id.orElse(null))
				.append("name", name.orElse(null))
				.append("createdBy", createdByUserId.orElse(null))
				.toString();
	}
}