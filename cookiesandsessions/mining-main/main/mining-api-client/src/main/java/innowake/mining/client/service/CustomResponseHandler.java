/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import org.apache.http.client.methods.CloseableHttpResponse;

import innowake.lib.core.api.lang.Nullable;

/**
 * A custom response handler can be used to handle REST service responses that use a content type
 * that differs from JSON.
 * 
 * @param <T> the concrete response type of the body
 */
public interface CustomResponseHandler<T> {

	/**
	 * Handles the REST service response. This will only be called if the response content type differs from JSON.
	 * 
	 * @param response the {@link CloseableHttpResponse}
	 * @return the processed response body or {@code null} if the implementation doesn't return it
	 */
	@Nullable
	public T handleResponse(CloseableHttpResponse response);
}
