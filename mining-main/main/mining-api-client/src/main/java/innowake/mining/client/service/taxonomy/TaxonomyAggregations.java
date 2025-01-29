/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import com.fasterxml.jackson.core.type.TypeReference;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.model.TaxonomyFieldName;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import java.io.IOException;
import java.util.List;

/**
 * HTTP REST service for get taxonomy aggregations
 */
public class TaxonomyAggregations extends ProjectIdService<TaxonomyAggregations, List<AggregationResult<TaxonomyFieldName>>> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/taxonomies/aggregations";

	private AggregationRequest<TaxonomyFieldName> request;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	TaxonomyAggregations(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Set the request body of type {@link AggregationRequest<TaxonomyFieldName>}
	 * 
	 * @param request the request body
	 * 
	 * @return {@code this}
	 */
	public TaxonomyAggregations setRequest(final AggregationRequest<TaxonomyFieldName> request) {
		this.request = request;
		return this;
	}

	@Override
	public Result<List<AggregationResult<TaxonomyFieldName>>> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		final HttpPost post = httpPost();
		post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(request), ContentType.APPLICATION_JSON));
		return execute(post, new TypeReference<>() {});
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (request == null) {
			throw new IllegalStateException("Request cannot be null, at least one module ID should be provided");
		}
	}
}
