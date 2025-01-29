/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST Service to check if AST Nodes have been created for specified module.
 */
public class HasAstNodes extends ModuleIdService<HasAstNodes, Boolean> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/hasAstNodes";

	/**
	 * Creates a new instance of the service.
	 *
	 * @param connectionInfo the connection info to use
	 */
	public HasAstNodes(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public Result<Boolean> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId)));
		return execute(httpGet(), new TypeReference<Boolean>() {});
	}

}
