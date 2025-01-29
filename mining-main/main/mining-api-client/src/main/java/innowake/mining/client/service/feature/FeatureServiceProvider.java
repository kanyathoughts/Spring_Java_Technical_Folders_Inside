/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.feature;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;
import innowake.mining.shared.model.Feature;

/**
 * Provides access to {@link Feature} services.
 */
public class FeatureServiceProvider extends ServiceProvider<FeatureServiceProvider> {

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	public FeatureServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link FindFeatureById}.
	 *
	 * @return the service instance
	 */
	public FindFeatureById findFeatureById() {
		return new FindFeatureById(connectionInfo);
	}
	
	/**
	 * Access to {@link ToggleFeature}.
	 *
	 * @return the service instance
	 */
	public ToggleFeature toggleFeature() {
		return new ToggleFeature(connectionInfo);
	}
}
