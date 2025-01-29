/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.auth;

import java.io.IOException;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * Session logout service
 */
public class LogoutService extends RestService<Void> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/auth/logout";

	LogoutService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public Result<Void> execute() throws IOException {
		setServiceUrl(ENDPOINT);
		return execute(httpGet());
	}

}
