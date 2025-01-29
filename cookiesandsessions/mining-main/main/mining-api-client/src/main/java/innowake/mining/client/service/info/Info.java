/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.info;

import java.io.IOException;
import java.util.Map;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for info.
 */
public class Info extends RestService<Map<String, String>> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/info";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	Info(final ConnectionInfo connectionInfo) {
		super(connectionInfo, ENDPOINT);
	}

	/**
	 * Gets information about the REST API by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * 
	 * @return a result holding a map containing the information as key-value properties with keys "api-version" and "userId"
	 */
	@Override
	public Result<Map<String, String>> execute() throws IOException {
		return execute(httpGet());
	}
}
