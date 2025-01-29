/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.feature;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.Feature;
import innowake.mining.shared.model.FeatureId;

/**
 * HTTP REST service for finding a {@link Feature}.
 */
public class FindFeatureById extends RestService<Feature> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/features/%s";

	@Nullable
	private FeatureId featureId;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindFeatureById(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Sets the feature id to use.
	 *
	 * @param featureId the feature id
	 * @return {@code this}
	 */
	public FindFeatureById setFeatureId(final FeatureId featureId) {
		this.featureId = featureId;
		return this;
	}

	/**
	 * Finds a feature by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given feature does not exist
	 * 
	 * @return a result holding the found {@link Feature} if the call was successful
	 */
	@Override
	public Result<Feature> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, assertNotNull(featureId).getId()));
		return execute(httpGet(), new TypeReference<Feature>() {});
	}
	
	@Override
	protected void validate() {
		if (featureId == null) {
			throw new IllegalStateException("Feature ID must be set.");
		}
	}
}
