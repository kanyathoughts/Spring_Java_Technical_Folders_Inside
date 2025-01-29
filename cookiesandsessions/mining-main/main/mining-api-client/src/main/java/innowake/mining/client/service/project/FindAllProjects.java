/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.project;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.ProjectPojo;

/**
 * HTTP REST service for find all projects.
 */
public class FindAllProjects extends RestService<ProjectPojo[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAllProjects(final ConnectionInfo connectionInfo) {
		super(connectionInfo, ENDPOINT);
	}

	/**
	 * Find all projects by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * 
	 * @return a result holding all found Project if the call was successful
	 */
	@Override
	public Result<ProjectPojo[]> execute() throws IOException {
		return execute(httpGet(), new TypeReference<ProjectPojo[]>() {});
	}
}
