/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.client;

import java.io.IOException;
import java.util.UUID;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.ClientPojo;

/**
 * HTTP REST service for find single client.
 */
public class FindClientById extends RestService<ClientPojo> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/clients/%s";
	
	@Nullable
	private String clientId;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindClientById(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the numeric id of the Client to search for.
	 *
	 * @param clientId the client id
	 * @return {@code this}
	 */
	public FindClientById setClientId(final Long clientId) {
		this.clientId = String.valueOf(clientId);
		return this;
	}

	/**
	 * Sets the unique id of the Client to search for.
	 *
	 * @param clientId the client id
	 * @return {@code this}
	 */
	public FindClientById setClientId(final UUID clientId) {
		this.clientId = clientId.toString();
		return this;
	}

	/**
	 * Finds a client by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>404</strong>: if the given client id does not exist
	 * 
	 * @return a result holding the found {@link ClientPojo} if the call was successful
	 */
	@Override
	public Result<ClientPojo> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, clientId));
		return execute(httpGet(), new TypeReference<ClientPojo>() {});
	}

	@Override
	protected void validate() {
		if (clientId == null) {
			throw new IllegalStateException("Client id must be set.");
		}
	}
}
