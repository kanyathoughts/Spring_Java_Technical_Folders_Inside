/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import java.io.IOException;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service to cancel a running job.
 */
public class CancelJob extends JobIdService<Void, CancelJob> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/jobs/%s/cancel";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected CancelJob(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Requests a running job to be canceled.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if there is no job with the provided Id
	 * 
	 * @return a result of {@link Void}
	 */
	@Override
	public Result<Void> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, jobId));
		return execute(httpPut());
	}

}
