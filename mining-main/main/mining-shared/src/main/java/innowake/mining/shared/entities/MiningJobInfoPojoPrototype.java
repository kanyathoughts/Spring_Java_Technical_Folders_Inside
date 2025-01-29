/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;

/**
 * {@code mining_job_info} entity request class.
 */
public final class MiningJobInfoPojoPrototype implements PojoPrototype {

	public final Definable<EntityId> module = new Definable<>(true, "MiningJobInfo.module");
	public final Definable<EntityId> project = new Definable<>(false, "MiningJobInfo.project");
	public final Definable<UUID> jobId = new Definable<>(false, "MiningJobInfo.jobId");

	@JsonAlias("moduleId")
	public MiningJobInfoPojoPrototype setModule(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}

	@JsonAlias("projectId")
	public MiningJobInfoPojoPrototype setProject(final EntityId project) {
		this.project.set(this.project.orElseNonNull(EntityId.VOID).merge(project));
		return this;
	}

	public MiningJobInfoPojoPrototype setJobId(final UUID jobId) {
		this.jobId.set(jobId);
		return this;
	}
}
