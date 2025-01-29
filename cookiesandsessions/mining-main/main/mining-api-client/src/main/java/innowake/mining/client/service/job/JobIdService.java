/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;

/**
 * Abstract HTTP rest service requiring a job Id.
 * 
 * @param <T> the type of the service result
 * @param <R> the type of the service implementation
 */
public abstract class JobIdService<T, R extends JobIdService<T, R>> extends RestService<T> {

	@Nullable
	protected String jobId;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	JobIdService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the job Id.
	 *
	 * @param jobId the job Id
	 * @return {@code this}
	 */
	@SuppressWarnings("unchecked")
	public R setJobId(final String jobId) {
		this.jobId = jobId;
		return (R) this;
	}

	@Override
	protected void validate() {
		if (jobId == null) {
			throw new IllegalStateException("Job id must be set.");
		}
	}
}
