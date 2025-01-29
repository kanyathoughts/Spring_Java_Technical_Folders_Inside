/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.discovery;

import java.io.IOException;
import java.net.URISyntaxException;
import org.apache.http.client.utils.URIBuilder;
import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;

/**
 * HTTP REST service for executing Discovery Metrics.
 */
public class DiscoverMetrics extends ProjectIdService<DiscoverMetrics, String> {

	/**
	 * The endpoint for running Discovery Metrics.
	 */
	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/discovery/discover-metrics";

	private boolean incremental = true;
	
	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	DiscoverMetrics(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the {@code incremental} discover metrics flag. Set the flag to {@code false} to force full discover metrics scans.
	 * <p>The default is {@code true}.</p>
	 *
	 * @param incremental {@code true} for incremental discovery or {@code false} to enforce full discovery.
	 * @return this {@link DiscoverMetrics} instance for method chaining
	 */
	public DiscoverMetrics setIncremental(final boolean incremental) {
		this.incremental = incremental;
		return this;
	}

	/**
	 * Executes Discovery Metrics by sending a HTTP POST request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>204</strong>: on success
	 * <li><strong>404</strong>: if the given project does not exist
	 * 
	 * @return a result holding the ID of the submitted job
	 * @throws IOException in case of an error
	 */
	@Override
	public Result<String> execute() throws IOException {
		validate();

		String serviceUrl = String.format(ENDPOINT, encode(projectId));
		if ( ! incremental) {
			try {
				final URIBuilder uri = new URIBuilder(serviceUrl);
				uri.addParameter("incremental", String.valueOf(incremental));
				serviceUrl = uri.toString();
			} catch (final URISyntaxException e) {
				throw new IllegalStateException(e);
			}
		}

		setServiceUrl(serviceUrl);
		return execute(httpPost(), new TypeReference<String>() {});
	}
}
