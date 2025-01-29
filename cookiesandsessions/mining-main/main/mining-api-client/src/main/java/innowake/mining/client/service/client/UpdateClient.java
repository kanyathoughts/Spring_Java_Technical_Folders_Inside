/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.client;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;

import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.ClientPojo;

/**
 * HTTP REST service for update existing client.
 */
public class UpdateClient extends ClientService {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/clients/%s";

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	UpdateClient(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Updates a client by sending a HTTP PUT request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>200</strong>: on success
	 * <li><strong>400</strong>: if the given client is not valid
	 * <li><strong>404</strong>: if the given client does not exist
	 * 
	 * @return a result holding the updated {@link ClientPojo} if the call was successful
	 */
	@Override
	public Result<ClientPojo> execute() throws IOException {
		validate();
		final Object clientId = assertNotNull(client).uid.isPresent() ? assertNotNull(client).uid.getNonNull() : assertNotNull(client).nid.getNonNull();
		setServiceUrl(String.format(ENDPOINT, clientId));
		final HttpPut put = httpPut();
		put.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(client), ContentType.APPLICATION_JSON));
		return execute(put, new TypeReference<ClientPojo>() {});
	}
}
