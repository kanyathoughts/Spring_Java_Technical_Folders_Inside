/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomytype;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to taxonomy type services.
 */
public class TaxonomyTypeServiceProvider extends ServiceProvider<TaxonomyTypeServiceProvider> {

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	public TaxonomyTypeServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllTaxonomyTypes}.
	 *
	 * @return the service instance
	 */
	public FindAllTaxonomyTypes findAllTaxonomyTypes() {
		return new FindAllTaxonomyTypes(connectionInfo);
	}
}
