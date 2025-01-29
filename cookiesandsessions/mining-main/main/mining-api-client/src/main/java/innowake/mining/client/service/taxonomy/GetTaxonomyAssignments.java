/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import java.io.IOException;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import com.fasterxml.jackson.core.type.TypeReference;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsGetRequest;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsGetResponse;

/**
 * HTTP REST service for get taxonomy assignments by list of module IDs
 */
public class GetTaxonomyAssignments extends ProjectIdService<GetTaxonomyAssignments, TaxonomyAssignmentsGetResponse> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/taxonomies/assignments";
	
	private TaxonomyAssignmentsGetRequest request;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	GetTaxonomyAssignments(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Set the request body of type {@link TaxonomyAssignmentsGetRequest}
	 * 
	 * @param request the request body
	 * 
	 * @return {@code this}
	 */
	public GetTaxonomyAssignments setRequest(final TaxonomyAssignmentsGetRequest request) {
		this.request = request;
		return this;
	}

	@Override
	public Result<TaxonomyAssignmentsGetResponse> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		final HttpPost post = httpPost();
		post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(request), ContentType.APPLICATION_JSON));
		return execute(post, new TypeReference<TaxonomyAssignmentsGetResponse>() {});
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (request == null) {
			throw new IllegalStateException("Request cannot be null, atleast one module ID should be provided");
		}
	}
}
