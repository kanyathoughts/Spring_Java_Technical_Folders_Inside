/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.feature;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.Feature;
import innowake.mining.shared.model.FeatureId;

/**
 * HTTP REST service for toggling a {@link Feature}.
 */
public class ToggleFeature extends RestService<Void> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/features/%s/toggle";

	@Nullable
	private FeatureId featureId;
	
	@Nullable
	private Boolean state;
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	ToggleFeature(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Sets the feature id to use.
	 *
	 * @param featureId the feature id
	 * @return {@code this}
	 */
	public ToggleFeature setFeatureId(final FeatureId featureId) {
		this.featureId = featureId;
		return this;
	}
	
	/**
	 * Sets the state to toggle.
	 *
	 * @param state the new state
	 * @return {@code this}
	 */
	public ToggleFeature setState(final Boolean state) {
		this.state = state;
		return this;
	}

	/**
	 * Toggles a feature by sending a HTTP POST request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>204</strong>: on success
	 * <li><strong>404</strong>: if the given feature does not exist
	 * 
	 * @return a result with the response status
	 */
	@Override
	public Result<Void> execute() throws IOException {
		validate();
		try {
			final URIBuilder uri = new URIBuilder(String.format(ENDPOINT, assertNotNull(featureId).getId()));
			uri.addParameter("state", assertNotNull(state).toString());
			setServiceUrl(uri.toString());
		} catch (final URISyntaxException e) {
			throw new IllegalStateException(e);
		}
		return execute(httpPost(), new TypeReference<Void>() {});
	}
	
	@Override
	protected void validate() {
		if (featureId == null) {
			throw new IllegalStateException("Feature ID must be set.");
		}
		if (state == null) {
			throw new IllegalStateException("State must be set.");
		}
	}
}
