/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.discovery;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for executing Discover Code.
 */
public class DiscoverCode extends ProjectIdService<DiscoverCode, String> {
	/**
	 * The endpoint for running Discover Code.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/discovery/discover-code";

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	protected DiscoverCode(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Executes Discover Code by sending a HTTP POST request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>204</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the ID of the submitted job
	 * @throws IOException in case of an error
	 */
	@Override
	public Result<String> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpPost(), new TypeReference<String>() {});
	}
}
