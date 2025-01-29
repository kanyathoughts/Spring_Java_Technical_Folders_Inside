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
import innowake.mining.shared.entities.ModulePojo;

/**
 * HTTP REST service for getting all modules of a given project.
 */
public class FindAllModules extends ProjectIdService<FindAllModules, ModulePojo[]> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAllModules(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Finds all modules of a given project by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding all {@linkplain ModulePojo modules} on success
	 */
	@Override
	public Result<ModulePojo[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpGet(), new TypeReference<ModulePojo[]>() {});
	}
}
