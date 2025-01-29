/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.module;

import java.io.IOException;
import java.util.List;

import innowake.mining.shared.model.ModuleFieldName;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;

/**
 * HTTP REST Service Get Aggregated values for utilities.
 */
public class GetAggregatedUtilityValues extends ProjectIdService<GetAggregatedUtilityValues, List<AggregationResult<ModuleFieldName>>> {
	
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/utility-aggregations";
	
	@Nullable
	private AggregationRequest<ModuleFieldName> aggregationRequest;
	
	/**
	 * Sets the aggregation request
	 * 
	 * @param aggregationRequest The aggregation request to use
	 * @return Object of GetAggregatedUtilityValues
	 */
	public GetAggregatedUtilityValues setAggregationRequest(final AggregationRequest<ModuleFieldName> aggregationRequest) {
		this.aggregationRequest = aggregationRequest;
		return this;
	}
	
	/**
	 * Creates a new instance of the service.
	 * 
	 * @param connectionInfo The connection info to use
	 */
	GetAggregatedUtilityValues(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Gets all aggregated utility values of a given project by sending a HTTP POST request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>400</strong>: if the given aggregation request is invalid.
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding all {@linkplain AggregationResult} of UtilityFieldName on success
	 */
	@Override
	public Result<List<AggregationResult<ModuleFieldName>>> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId)));
		final HttpPost post = httpPost();
		post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(aggregationRequest), ContentType.APPLICATION_JSON));
		return execute(post, new TypeReference<List<AggregationResult<ModuleFieldName>>>() {});
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (aggregationRequest == null) {
			throw new IllegalStateException("AggregationRequest must not be null.");
		}
	}
}
