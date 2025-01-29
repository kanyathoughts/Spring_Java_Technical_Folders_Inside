/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences.ui;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Optional;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;

import innowake.mining.plugin.Logging;

/**
 * Mining server related utility methods.
 */
public class MiningServer {
	
	private static final String AUTHENTICATION_CONFIGURATION_FILE_NAME = "keycloak-eclipse.json";

	private MiningServer() {}

	/**
	 * Retrieves the authentication configuration from the given server.
	 * 
	 * @param serverBaseUrl the API server base URL with port, e.g http://localhost:8080
	 *
	 * @return an optional configuration if available, {@link Optional#empty()} otherwise
	 * 
	 * @throws IOException if an I/O exception occurs.
	 */
	public static Optional<String> retrieveAuthorizationConfiguration(final String serverBaseUrl) throws IOException {
		final String url = StringUtils.appendIfMissing(serverBaseUrl, "/") + AUTHENTICATION_CONFIGURATION_FILE_NAME;
		try (final CloseableHttpClient httpClient = HttpClientBuilder.create().build()) {
			final HttpGet getRequest = new HttpGet(url);
			final HttpResponse response = httpClient.execute(getRequest);

			if (response.getStatusLine().getStatusCode() != HttpStatus.SC_OK) {
				final String errorMessage = "Failed : HTTP error code : " + response.getStatusLine().getStatusCode();
				Logging.error(errorMessage);
				return Optional.empty();
			}

			final String configuration = IOUtils.toString(response.getEntity().getContent(), StandardCharsets.UTF_8);
			return Optional.of(configuration);
		} 
	}

}
