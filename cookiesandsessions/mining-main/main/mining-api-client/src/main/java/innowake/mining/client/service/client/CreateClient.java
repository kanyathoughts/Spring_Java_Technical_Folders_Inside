/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.client;

import java.io.IOException;

import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.ClientPojo;

/**
 * HTTP REST service for create new client.
 */
public class CreateClient extends ClientService {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/clients";
	
	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	CreateClient(final ConnectionInfo connectionInfo) {
		super(connectionInfo, ENDPOINT);
	}

	/**
	 * Creates a new module by sending a HTTP POST request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status codes:
	 * <li><strong>201</strong>: on success
	 * <li><strong>400</strong>: if the given client is not valid
	 * 
	 * @return a result holding the newly created {@link ClientPojo} if the call was successful
	 */
	@Override
	public Result<ClientPojo> execute() throws IOException {
		validate();
		final HttpPost post = httpPost();
		post.setEntity(new StringEntity(PojoMapper.jsonWriter().writeValueAsString(client), ContentType.APPLICATION_JSON));
		return execute(post, new TypeReference<ClientPojo>() {});
	}
}
