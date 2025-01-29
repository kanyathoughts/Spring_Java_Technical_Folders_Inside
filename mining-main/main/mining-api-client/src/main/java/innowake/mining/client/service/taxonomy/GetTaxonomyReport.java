/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.TaxonomyReport;

/**
 * HTTP REST service to find all taxonomyReports.
 */
public class GetTaxonomyReport extends ProjectIdService<GetTaxonomyReport, TaxonomyReport[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/taxonomies/reports";
	
	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	public GetTaxonomyReport(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Finds all taxonomyReports of a given projectId by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding all {@linkplain TaxonomyReport taxonomies} on success
	 */
	@Override
	public Result<TaxonomyReport[]> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		return execute(httpGet(), new TypeReference<TaxonomyReport[]>() {});
	}

	
}
