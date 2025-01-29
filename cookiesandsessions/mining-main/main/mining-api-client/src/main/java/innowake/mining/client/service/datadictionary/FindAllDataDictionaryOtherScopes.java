/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.datadictionary;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for getting all DataDictionaryOtherScopes of a given project.
 */
public class FindAllDataDictionaryOtherScopes extends ProjectIdService<FindAllDataDictionaryOtherScopes, String[]> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/data-dictionary/other-scopes";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAllDataDictionaryOtherScopes(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Finds all DataDictionaryOtherScopes of a given project by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding all DataDictionaryOtherScopePojo on success
	 */
	@Override
	public Result<String[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpGet(), new TypeReference<String[]>() {});
	}
}
