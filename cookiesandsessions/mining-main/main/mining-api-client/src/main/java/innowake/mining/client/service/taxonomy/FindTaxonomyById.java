/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.FindByIdWithProjectIdService;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * HTTP REST service for finding a taxonomy by ID.
 */
public class FindTaxonomyById  extends FindByIdWithProjectIdService<FindTaxonomyById, TaxonomyPojo> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/taxonomies/%s";
	
	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	FindTaxonomyById(final ConnectionInfo connectionInfo) {
		super(connectionInfo, ENDPOINT, new TypeReference<TaxonomyPojo>() {});
	}
}
