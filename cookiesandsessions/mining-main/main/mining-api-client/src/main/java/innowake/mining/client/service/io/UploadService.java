/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.io;

import java.io.IOException;
import java.io.InputStream;

import org.apache.http.HttpEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.mime.MultipartEntityBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.Result;

/**
 * Abstract base HTTP REST service for uploading an {@link InputStream}.
 * 
 * @param <S> the type of the actual service 
 * @param <T> the type if present MiningFileIndex.
 */
public abstract class UploadService<S extends ProjectIdService<S, T>, T> extends ProjectIdService<S, T> {

	@Nullable
	protected InputStream inputStream;
	
	@Nullable
	protected String inputStreamId;

	/**
	 * Constructor.
	 * 
	 * @param connectionInfo the {@link ConnectionInfo}
	 */
	protected UploadService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the input stream to upload.
	 *
	 * @param inputStream the input stream to upload
	 * @return {@code this}
	 */
	public S setInputStream(final InputStream inputStream) {
		this.inputStream = inputStream;
		return getThis();
	}
	
	/**
	 * Sets the ID of the {@link InputStream}.
	 *
	 * @param inputStreamId the ID
	 * @return {@code this}
	 */
	public S setInputStreamId(final String inputStreamId) {
		this.inputStreamId = inputStreamId;
		return getThis();
	}
	
	@Override
	protected HttpPost httpPost() {
		validate();
		setServiceUrl(String.format(getEndpoint(), encode(projectId)));
		final String apiUrl = buildUrl();
		LOG.debug(() -> "Calling POST " + apiUrl);
		final HttpPost httpPost = new HttpPost(apiUrl);
		httpPost.setHeader(getHeaderWithAuth());
		final HttpEntity entity = MultipartEntityBuilder
				.create()
				.addBinaryBody("file", inputStream, ContentType.APPLICATION_OCTET_STREAM, inputStreamId)
				.build();
		httpPost.setEntity(entity);
		return httpPost;
	}
	
	@Override
	protected HttpPut httpPut() {
		validate();
		setServiceUrl(String.format(getEndpoint(), encode(projectId)));
		final String apiUrl = buildUrl();
		LOG.debug(() -> "Calling PUT " + apiUrl);
		final HttpPut httpPut = new HttpPut(apiUrl);
		httpPut.setHeader(getHeaderWithAuth());
		final HttpEntity entity = MultipartEntityBuilder
				.create()
				.addBinaryBody("file", inputStream, ContentType.APPLICATION_OCTET_STREAM, inputStreamId)
				.build();
		httpPut.setEntity(entity);
		return httpPut;
	}
	/**
	 * Uploads an {@link InputStream} by sending a HTTP POST request to the specified end point.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>204</strong>: on success
	 * <li><strong>400</strong>: if the given {@link InputStream} is not valid
	 * <li><strong>404</strong>: if the given project does not exist
	 * <li><strong>501</strong>: if this feature is not enabled
	 * 
	 * @return a result holding only the status code of the response
	 * @throws IOException in case of an error
	 */
	@Override
	public Result<T> execute() throws IOException {
		return execute(httpPost());
	}

	/**
	 * Returns the specified end point.
	 *
	 * @return the specified end point
	 */
	protected abstract String getEndpoint();

	@Override
	protected void validate() {
		super.validate();
		if (inputStream == null) {
			throw new IllegalStateException("Input stream must be set.");
		}
		if (inputStreamId == null) {
			throw new IllegalStateException("Name must be set.");
		}
	}
}
