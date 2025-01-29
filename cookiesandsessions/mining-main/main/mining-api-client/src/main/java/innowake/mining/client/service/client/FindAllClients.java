/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.client;

import java.io.IOException;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.entities.ClientPojo;

/**
 * HTTP REST service for find all clients.
 */
public class FindAllClients extends RestService<ClientPojo[]> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/clients";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	FindAllClients(final ConnectionInfo connectionInfo) {
		super(connectionInfo, ENDPOINT);
	}

	/**
	 * Finds all clients by sending a HTTP GET request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * 
	 * @return a result holding all found {@linkplain ClientPojo clients} if the call was successful
	 */
	@Override
	public Result<ClientPojo[]> execute() throws IOException {
		return execute(httpGet(), new TypeReference<ClientPojo[]>() {});
	}
}
