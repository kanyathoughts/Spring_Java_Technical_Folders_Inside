/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.UnsupportedCharsetException;
import java.util.Optional;

import org.apache.http.HttpEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.JsonProcessingException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.shared.PojoMapper;

/**
 * Generic REST service client implementation for tests.
 * @param <T> return type
 */
public class GenericRestService<T> extends RestService<T> {

	@Nullable
	private HttpUriRequest request = null;

	public GenericRestService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	@Nullable
	private HttpEntity createEntity(@Nullable final Object payload) throws UnsupportedEncodingException, UnsupportedCharsetException, JsonProcessingException {
		if (payload == null) {
			return null;
		} else if (payload instanceof String) {
			return new StringEntity((String) payload);
		} else {
			return new StringEntity(PojoMapper.jsonWriter().writeValueAsString(payload), ContentType.APPLICATION_JSON);
		}
	}

	public RestService<T> post(final String url, @Nullable final Object payload) throws UnsupportedEncodingException, UnsupportedCharsetException, JsonProcessingException {
		setServiceUrl(url);
		final HttpPost rqst = httpPost();
		rqst.setEntity(createEntity(payload));
		this.request = rqst;
		return this;
	}

	public RestService<T> get(final String url) throws UnsupportedCharsetException {
		setServiceUrl(url);
		this.request = httpGet();
		return this;
	}

	public RestService<T> put(final String url, @Nullable final Object payload) throws UnsupportedEncodingException, UnsupportedCharsetException, JsonProcessingException {
		setServiceUrl(url);
		final HttpPut rqst = httpPut();
		rqst.setEntity(createEntity(payload));
		this.request = rqst;
		return this;
	}

	public RestService<T> delete(final String url) throws UnsupportedCharsetException {
		setServiceUrl(url);
		this.request = httpDelete();
		return this;
	}

	@Override
	public Result<T> execute() throws IOException {
		return execute(Optional.of(this.request).get());
	}

}
