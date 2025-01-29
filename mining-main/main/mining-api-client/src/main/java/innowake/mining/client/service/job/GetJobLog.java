/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import java.io.IOException;
import java.util.Map;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service to request the job log of a single specific job.
 */
public class GetJobLog extends JobIdService<Map<String, String>, GetJobLog> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/jobs/%s/log";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected GetJobLog(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Request the job log(s) by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if there is no job with the provided Id
	 * 
	 * @return a result holding the job log(s) if the call was successful
	 */
	@Override
	public Result<Map<String, String>> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, jobId));
		return execute(httpGet(), new TypeReference<Map<String, String>>() {});
	}

}
