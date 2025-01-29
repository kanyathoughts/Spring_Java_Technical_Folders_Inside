/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import java.io.IOException;

import org.apache.http.client.utils.URIBuilder;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.shared.access.EntityId;

/**
 * Base class for all REST services that include a project id and a record id.
 * 
 * @param <T> the type of the service result 
 * @param <S> the type of the actual service 
 */
public class FindByIdWithProjectIdService<S extends FindByIdWithProjectIdService<S, T>, T> extends ProjectIdService<S, T> {

	@Nullable
	protected EntityId id;
	protected final String endpoint;
	protected final TypeReference<T> type;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 * @param endpoint the end point of the actual service
	 * @param type the type of the service result 
	 */
	protected FindByIdWithProjectIdService(ConnectionInfo connectionInfo, final String endpoint, final TypeReference<T> type) {
		super(connectionInfo);
		this.endpoint = endpoint;
		this.type = type;
	}
	
	/**
	 * Sets the record id to use.
	 *
	 * @param id the record id
	 * @return {@code this}
	 */
	public S setId(final EntityId id) {
		this.id = id;
		return getThis();
	}

	/**
	 * Finds by sending a HTTP GET request to the specified endpoint.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given record id does not exist
	 * 
	 * @return a result holding the found Annotation if the call was successful
	 */
	@Override
	public Result<T> execute() throws IOException {
		validate();
		final URIBuilder uri = new URIBuilder();
		uri.setPath(String.format(endpoint, encode(projectId), encode(id)));
		setServiceUrl(uri.toString());
		return execute(httpGet(), type);
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (id == null) {
			throw new IllegalStateException("Record id must be set.");
		}
	}
}