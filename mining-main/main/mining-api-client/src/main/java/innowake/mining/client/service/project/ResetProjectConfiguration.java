/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.project;

import java.io.IOException;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service to reset a project's configuration.
 */
public class ResetProjectConfiguration extends ProjectIdService<ResetProjectConfiguration, Void> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/defaultConfiguration";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	ResetProjectConfiguration(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Resets a project's configuration by sending a HTTP POST request to the {@value #ENDPOINT} endpoint.
	 * 
	 * @return a result holding the found Project if the call was successful
	 */
	@Override
	public Result<Void> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpPost());
	}

}
