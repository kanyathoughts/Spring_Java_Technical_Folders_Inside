/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * {@code mining_job_info} entity class.
 */
@MiningDataType(name = MiningEnitityNames.MINING_JOB_INFO)
public final class MiningJobInfoPojo {

	private final Optional<EntityId> project;
	private final Optional<EntityId> module;
	private final UUID jobId;

	public MiningJobInfoPojo(
			@Nullable final EntityId project,
			@Nullable final EntityId module,
			final UUID jobId) {
		this.project = Optional.ofNullable(project);
		this.module = Optional.ofNullable(module);
		this.jobId = jobId;
	}

	/**
	 * @return the {@link EntityId} of the module this job info belongs to
	 */
	public Optional<EntityId> getModule() {
		return module;
	}

	/**
	 * @return the {@link EntityId} of the project this job info belongs to
	 */
	public Optional<EntityId> getProject() {
		return project;
	}

	/**
	 * @return the id of this job info
	 */
	public UUID getJobId() {
		return jobId;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("module", module)
				.append("project", project)
				.append("jobId", jobId)
				.toString();
	}
}
