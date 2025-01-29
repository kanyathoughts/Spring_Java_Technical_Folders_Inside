/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.version;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to version services.
 */
public class VersionServiceProvider extends ServiceProvider<VersionServiceProvider> {

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public VersionServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link Version}.
	 *
	 * @return the service instance
	 */
	public Version version() {
		return new Version(connectionInfo);
	}
}
