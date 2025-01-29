/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.discovery;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to Discovery services.
 */
public class DiscoveryServiceProvider extends ServiceProvider<DiscoveryServiceProvider> {

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	public DiscoveryServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link DiscoverMetrics}.
	 *
	 * @return the service instance
	 */
	public DiscoverMetrics discoverMetrics() {
		return new DiscoverMetrics(connectionInfo);
	}
	
	/**
	 * Access to {@link DownloadConfiguration}.
	 *
	 * @return the service instance
	 */
	public DownloadConfiguration downloadConfiguration() {
		return new DownloadConfiguration(connectionInfo);
	}
	
	/**
	 * Access to {@link UploadConfiguration}.
	 *
	 * @return the service instance
	 */
	public UploadConfiguration uploadConfiguration() {
		return new UploadConfiguration(connectionInfo);
	}
	
	/**
	 * Access to {@link DiscoverCode}.
	 *
	 * @return the service instance
	 */
	public DiscoverCode discoverCode() {
		return new DiscoverCode(connectionInfo);
	}
	
	/**
	 * Access to {@link FindCommunities}.
	 *
	 * @return the service instance
	 */
	public FindCommunities findCommunities() {
		return new FindCommunities(connectionInfo);
	}
}
