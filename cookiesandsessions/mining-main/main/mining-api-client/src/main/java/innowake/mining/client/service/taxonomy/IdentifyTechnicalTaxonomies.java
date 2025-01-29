/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;


import innowake.mining.client.service.ModuleBasedService;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;


/**
 * HTTP REST service for identifying Technical Taxonomies on the given modules.
 */
public class IdentifyTechnicalTaxonomies extends ModuleBasedService<IdentifyTechnicalTaxonomies> {

	/**
	 * The REST endpoint URL called by this service.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/identify-technical-taxonomies";

	IdentifyTechnicalTaxonomies(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Override
	public String getEndPoint() {
		return ENDPOINT;
	}
}
