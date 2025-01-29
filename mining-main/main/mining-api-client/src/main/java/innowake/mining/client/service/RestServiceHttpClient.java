/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import org.apache.http.client.config.RequestConfig;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;

/**
 * Provides the http client for {@link RestService}s.
 */
public final class RestServiceHttpClient {
	
	/**
	 * Disable redirects to mitigate missing custom response handler when the server is using Keycloak
	 * and wants to redirect to the Keycloak login page.
	 */
	private static final RequestConfig REQUEST_CONFIG = RequestConfig.copy(RequestConfig.DEFAULT)
			.setRedirectsEnabled(false)
			.setRelativeRedirectsAllowed(false)
			.build();

	private RestServiceHttpClient() {
		/* not supposed to be instantiated */
	}
	
	/**
	 * The http client used by all {@link RestService}s.
	 */
	static final CloseableHttpClient HTTP_CLIENT = init();
	
	private static CloseableHttpClient init() {
        final PoolingHttpClientConnectionManager conectionManager = new PoolingHttpClientConnectionManager();
        conectionManager.setMaxTotal(100);
        return HttpClients.custom()
                .setConnectionManager(conectionManager)
                .setDefaultRequestConfig(REQUEST_CONFIG)
                .build();
	}
	
}
