/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomytype;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.TaxonomyTypePojo;

/**
 * HTTP REST service for find all taxonomy types.
 */
public class FindAllTaxonomyTypes extends ProjectIdService<FindAllTaxonomyTypes, TaxonomyTypePojo[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/taxonomy-types";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAllTaxonomyTypes(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds all taxonomy type entities by sending a HTTP GET request to {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding all found {@link TaxonomyTypePojo} if the call was successful
	 */
	@Override
	public Result<TaxonomyTypePojo[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpGet(), new TypeReference<TaxonomyTypePojo[]>() {});
	}
}
