/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import java.io.Serializable;
import java.util.UUID;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.FileJob;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.MiningJobInfoService;
import innowake.mining.shared.entities.MiningJobInfoPojoPrototype;

/**
 * Mining specific base implementation for all jobs that require file access (read/write/delete).
 * <p>By default this class uses the job result folder on the server's file system for all file operations,
 * see {@link JobConfigurationProperties#getJobResultFolder()}.</p>
 * 
 * @param <X> the concrete result type of the job
 */
public abstract class MiningFileJob<X extends Serializable> extends FileJob<X> {
	
	@Autowired
	private transient MiningJobInfoService miningJobInfoService;
	@Nullable
	private EntityId projectId;
	@Nullable
	private EntityId moduleId;
	private boolean isConstructed;

	protected MiningFileJob(final JobConfigurationProperties jobConfigurationProperties, final EntityId projectId) {
		super(jobConfigurationProperties);
		this.projectId = projectId;
	}

	protected MiningFileJob(final JobConfigurationProperties jobConfigurationProperties, final EntityId projectId, @Nullable final EntityId moduleId) {
		super(jobConfigurationProperties);
		this.projectId = projectId;
		this.moduleId = moduleId;
	}

	protected MiningFileJob(final JobConfigurationProperties jobConfigurationProperties) {
		super(jobConfigurationProperties);
	}

	@PostConstruct
	private void postConstruct() {
		if ( ! isConstructed) {
			final var miningJobInfo = new MiningJobInfoPojoPrototype()
					.setJobId(UUID.fromString(jobId));
			if (projectId != null) {
				miningJobInfo.setProject(projectId);
			}
			if (moduleId != null) {
				miningJobInfo.setModule(moduleId);
			}

			miningJobInfoService.create(miningJobInfo);
			isConstructed = true;
		}
	}
}
