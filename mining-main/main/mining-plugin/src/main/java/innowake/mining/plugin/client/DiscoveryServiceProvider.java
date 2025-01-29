/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.discovery.DiscoverCode;
import innowake.mining.client.service.discovery.DiscoverMetrics;
import innowake.mining.client.service.discovery.DownloadConfiguration;
import innowake.mining.client.service.discovery.FindCommunities;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides access to {@link innowake.mining.client.service.discovery.DiscoveryServiceProvider} with project id already set. 
 */
public class DiscoveryServiceProvider extends innowake.mining.client.service.discovery.DiscoveryServiceProvider {

	@Nullable private final ProjectData projectData;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	DiscoveryServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}
	
	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	DiscoveryServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
		projectData = null;
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public DiscoverMetrics discoverMetrics() {
		return init(super.discoverMetrics());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public DownloadConfiguration downloadConfiguration() {
		return init(super.downloadConfiguration());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public DiscoverCode discoverCode() {
		return init(super.discoverCode());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindCommunities findCommunities() {
		return init(super.findCommunities());
	}
	
	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		if (projectData != null) {
			return (T) service.setProjectId(projectData.getProjectId());
		}
		return (T) service;
	}
}
