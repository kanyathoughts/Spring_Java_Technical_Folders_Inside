/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import java.io.IOException;

import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;

/**
 * HTTP REST service for set taxonomy assignments by module IDs and taxonomy IDs
 */
public class UpdateTaxonomyAssignments extends ProjectIdService<UpdateTaxonomyAssignments, Void> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/taxonomies/assignments";
	
	private TaxonomyAssignmentsSetRequest request;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	UpdateTaxonomyAssignments(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Set the request body of type {@link TaxonomyAssignmentsSetRequest}
	 * 
	 * @param request the request body
	 * 
	 * @return {@code this}
	 */
	public UpdateTaxonomyAssignments setRequest(final TaxonomyAssignmentsSetRequest request) {
		this.request = request;
		return this;
	}

	@Override
	public Result<Void> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		final HttpPut put = httpPut();
		put.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(request), ContentType.APPLICATION_JSON));
		return execute(put);
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (request == null) {
			throw new IllegalStateException("Request body cannot be null");
		}
	}
}
