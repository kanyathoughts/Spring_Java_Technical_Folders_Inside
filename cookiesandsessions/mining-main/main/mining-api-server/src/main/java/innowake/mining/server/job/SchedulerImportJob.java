/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import java.io.Serializable;

import innowake.mining.shared.access.EntityId;

/**
 * Base class for all scheduler import jobs.
 * @param <X> The type of the result of the job.
 */
public abstract class SchedulerImportJob<X extends Serializable> extends MiningJob<X> {

	protected SchedulerImportJob(final EntityId projectId) {
		super(projectId);
	}

}
