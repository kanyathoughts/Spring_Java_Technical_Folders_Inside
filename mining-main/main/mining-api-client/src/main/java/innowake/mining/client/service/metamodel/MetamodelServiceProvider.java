/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.metamodel;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to the metamodel services.
 */
public class MetamodelServiceProvider extends ServiceProvider<MetamodelServiceProvider> {

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public MetamodelServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Access to {@link FindMetamodel}.
	 *
	 * @return the service instance
	 */
	public FindMetamodel findMetamodel() {
		return new FindMetamodel(connectionInfo);
	}

	/**
	 * Access to {@link FindPropertyMetadata}.
	 *
	 * @return the service instance
	 */
	public FindPropertyMetadata findPropertyMetadata() {
		return new FindPropertyMetadata(connectionInfo);
	}
	
	/**
	 * Access to {@link RefreshMetamodel}.
	 *
	 * @return the service instance
	 */
	public RefreshMetamodel refresh() {
		return new RefreshMetamodel(connectionInfo);
	}

}
