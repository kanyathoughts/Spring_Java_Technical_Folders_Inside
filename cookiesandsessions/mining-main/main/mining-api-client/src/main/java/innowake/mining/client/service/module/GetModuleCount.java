/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for getting the count of all modules of a given project.
 */
public class GetModuleCount extends ProjectIdService<GetModuleCount, Long> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/count";

	GetModuleCount(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Gets the number of modules of a given project by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * 
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the number of modules of a project if the call was successful
	 */
	@Override
	public Result<Long> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpGet(), new TypeReference<Long>() {});
	}
}
