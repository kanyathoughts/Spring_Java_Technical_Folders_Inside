/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.auth;

import java.io.IOException;
import java.util.Map;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * Session login/status service
 */
public class LoginService extends RestService<Map<String, String>> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/auth/login";

	LoginService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public Result<Map<String, String>> execute() throws IOException {
		setServiceUrl(ENDPOINT);
		return execute(httpGet());
	}
	
}
