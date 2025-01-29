/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.job;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.CustomResponseHandler;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.job.ResultContainer;

/**
 * HTTP REST service to request the result of a finished job.
 */
public class GetJobResult extends JobIdService<ResultContainer, GetJobResult> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/jobs/%s/result";
	
	@Nullable
	private CustomResponseHandler<ResultContainer> customResponseHandler;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	GetJobResult(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Sets the {@link CustomResponseHandler} that should be used for this request to handle responses
	 * having a content type different from JSON.
	 *
	 * @param customResponseHandler the {@link CustomResponseHandler}
	 * @return {@code this}
	 */
	public GetJobResult setCustomResponseHandler(final CustomResponseHandler<ResultContainer> customResponseHandler) {
		this.customResponseHandler = customResponseHandler;
		return this;
	}

	/**
	 * Requests the job specific result object by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if there is no job with the provided Id
	 * <li><strong>204</strong>: if the job has no result
	 * 
	 * @return a result holding the {@link ResultContainer} if the call was successful and with JSON content type;
	 * the {@link ResultContainer} may also be {@code null} depending on the implementation of a optional provided {@link CustomResponseHandler}
	 */
	@Override
	public Result<ResultContainer> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, jobId));
		return execute(httpGet(), new TypeReference<ResultContainer>() {}, customResponseHandler);
	}

}
