/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import innowake.mining.client.ConnectionInfo;

/**
 * Base class for all service provider classes.
 * 
 * @param <T> the type of the actual service provider class
 */
public abstract class ServiceProvider<T extends ServiceProvider<T>> {

	/**
	 * Shared access to {@link ConnectionInfo}.
	 */
	protected final ConnectionInfo connectionInfo;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected ServiceProvider(final ConnectionInfo connectionInfo) {
		this.connectionInfo = connectionInfo;
	}
}
