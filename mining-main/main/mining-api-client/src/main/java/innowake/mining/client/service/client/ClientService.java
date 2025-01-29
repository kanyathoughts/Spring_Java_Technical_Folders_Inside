/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.client;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ClientPojoPrototype;

/**
 * Base service for clients endpoints.
 */
public abstract class ClientService extends RestService<ClientPojo> {

	@Nullable
	protected ClientPojoPrototype client;

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	protected ClientService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 * @param serviceUrl the endpoint to use
	 */
	protected ClientService(final ConnectionInfo connectionInfo, final String serviceUrl) {
		super(connectionInfo, serviceUrl);
	}

	/**
	 * Sets the client.
	 *
	 * @param client the client
	 * @return {@code this}
	 */
	public ClientService setClient(final ClientPojoPrototype client) {
		this.client = client;
		return this;
	}
	
	@Override
	protected void validate() {
		if (client == null) {
			throw new IllegalStateException("Client must be set.");
		}
	}
}
