/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.authentication;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import org.apache.http.HttpException;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.Logging;

/**
 * Provides functionality for making HTTP REST calls.
 */
public class ApacheHttpClient {

	/**
	 * Logger {@link Logging#MINING_CLIENT}.
	 */
	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT);

	/**
	 * Private constructor to hide the public one.
	 */
	private ApacheHttpClient() { }

	/**
	 * The get method to make a HTTPGET call to fetch a resource.
	 *
	 * @param url the URL to fetch the resource from
	 * @return the string representation of the response if successful
	 * @throws HttpException if response received is not {@link HttpStatus#SC_OK}
	 * @throws IllegalStateException if some occurs while executing the get request or reading the response
	 */
	public static String get(final String url) throws HttpException {
		final StringBuilder responseString = new StringBuilder();
		try (final CloseableHttpClient httpClient = HttpClientBuilder.create().build()) {
			final HttpGet getRequest = new HttpGet(url);
			final HttpResponse response = httpClient.execute(getRequest);

			if (response.getStatusLine().getStatusCode() != HttpStatus.SC_OK) {
				final String errorMessage = "Failed : HTTP error code : " + response.getStatusLine().getStatusCode();
				LOG.error(() -> errorMessage);
				throw new HttpException(errorMessage);
			}

			final BufferedReader br = new BufferedReader(new InputStreamReader((response.getEntity().getContent())));
			br.lines().forEach(responseString::append);
			return responseString.toString();
		} catch (final IOException ioException) {
			LOG.error(() -> ioException.getMessage(), ioException);
			throw new IllegalStateException(ioException);
		}
	}
}
