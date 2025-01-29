/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.job.JobInformation;

/**
 * HTTP REST service to request the {@link JobInformation} of a single specific job.
 */
public class GetJobInfo extends JobIdService<JobInformation, GetJobInfo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/jobs/%s/info";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected GetJobInfo(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Request the {@link JobInformation} of a job by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if there is no job with the provided Id
	 * 
	 * @return a result holding the {@link JobInformation} if the call was successful
	 */
	@Override
	public Result<JobInformation> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, jobId));
		return execute(httpGet(), new TypeReference<JobInformation>() {});
	}

}
