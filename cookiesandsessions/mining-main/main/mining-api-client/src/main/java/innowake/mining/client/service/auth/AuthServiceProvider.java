/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.auth;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Allows to start or end a user session at the backend server.
 */
public class AuthServiceProvider extends ServiceProvider<AuthServiceProvider> {

	public AuthServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	public LoginService login() {
		return new LoginService(connectionInfo);
	}
	
	public LogoutService logout() {
		return new LogoutService(connectionInfo);
	}

}
