/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.version;

import java.io.IOException;
import java.util.Map;

import org.apache.http.Header;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for version information.
 */
public class Version extends RestService<Map<String, String>> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/version";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	Version(final ConnectionInfo connectionInfo) {
		super(connectionInfo, ENDPOINT);
	}

	/**
	 * Gets version information about the REST API by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * 
	 * @return a result holding a map containing the information with the key "version"
	 */
	@Override
	public Result<Map<String, String>> execute() throws IOException {
		return execute(httpGet());
	}

	@Nullable 
	@Override
	protected Header getHeaderWithAuth() {
		/* Even though the endpoint is unsecured, meaning it requires no authentication, Spring will activate authentication
		 * if an Authorization header is sent, so we need to suppress this for this particular service call. */
		return null;
	}
}
