/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import java.io.Serializable;
import java.util.Objects;
import java.util.UUID;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.MiningJobInfoService;
import innowake.mining.shared.entities.MiningJobInfoPojoPrototype;


/**
 * Mining specific generic job implementation that can be submitted to any {@link JobManager} implementation.
 *
 * @param <X> the concrete result type of the job
 */
public abstract class MiningJob<X extends Serializable> extends Job<X> {

	@Autowired
	private transient MiningJobInfoService miningJobInfoService;
	protected EntityId projectId;
	@Nullable
	private EntityId moduleId;
	private boolean isConstructed;

	protected MiningJob(final EntityId projectId) {
		this(projectId, EntityId.VOID);
	}

	protected MiningJob(final EntityId projectId, @Nullable final EntityId moduleId) {
		this.projectId = projectId;
		this.moduleId = moduleId == null ? EntityId.VOID : moduleId;
	}

	@PostConstruct
	private void saveMiningJobInfo() {
		if ( ! isConstructed) {
			final var miningJobInfo = new MiningJobInfoPojoPrototype()
					.setJobId(UUID.fromString(jobId));
			if ( ! projectId.isEmpty()) {
				miningJobInfo.setProject(projectId);
			}
			if (moduleId != null && ! moduleId.isEmpty()) {
				miningJobInfo.setModule(Objects.requireNonNull(moduleId));
			}

			miningJobInfoService.create(miningJobInfo);
			isConstructed = true;
		}
	}
}
