/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.project;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * HTTP REST service for find single project.
 */
public class FindProjectById extends ProjectIdService<FindProjectById, ProjectPojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindProjectById(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds a project by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project id does not exist
	 * 
	 * @return a result holding the found Project if the call was successful
	 */
	@Override
	public Result<ProjectPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpGet(), new TypeReference<ProjectPojo>() {});
	}
}
