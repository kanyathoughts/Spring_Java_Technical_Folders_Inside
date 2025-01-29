/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.client;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to client services.
 */
public class ClientServiceProvider extends ServiceProvider<ClientServiceProvider> {

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public ClientServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllClients}.
	 *
	 * @return the service instance
	 */
	public FindAllClients findAllClients() {
		return new FindAllClients(connectionInfo);
	}
	
	/**
	 * Access to {@link FindClientById}.
	 *
	 * @return the service instance
	 */
	public FindClientById findClientById() {
		return new FindClientById(connectionInfo);
	}
	
	/**
	 * Access to {@link CreateClient}.
	 *
	 * @return the service instance
	 */
	public CreateClient createClient() {
		return new CreateClient(connectionInfo);
	}
	
	/**
	 * Access to {@link UpdateClient}.
	 *
	 * @return the service instance
	 */
	public UpdateClient updateClient() {
		return new UpdateClient(connectionInfo);
	}
}
