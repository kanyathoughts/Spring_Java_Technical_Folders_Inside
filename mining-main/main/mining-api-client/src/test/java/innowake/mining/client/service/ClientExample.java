/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import static java.lang.Long.valueOf;

import java.io.IOException;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.client.ClientServiceProvider;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ClientPojoPrototype;

/**
 * Command line runner to test client services.
 */
public class ClientExample {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);
	private static final String MARK = " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
	
	public static void main(final String[] args) throws IOException {
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", "");
		final ClientServiceProvider service = MiningApiClient.clientService(connectionInfo);

		LOG.info("find all clients" + MARK);
		LOG.info("Status message: " + service.findAllClients().execute().getStatusMessage());
		
		LOG.info("find client with id=1" + MARK);
		final Result<ClientPojo> foundClientResult = service.findClientById().setClientId(valueOf(1)).execute();
		final ClientPojo foundClient = foundClientResult.getValue().orElse(null);
		LOG.info("Status message: " + foundClientResult.getStatusMessage());
		
		LOG.info("find non existent client" + MARK);
		LOG.info("Status message: " + service.findClientById().setClientId(valueOf(-1)).execute().getStatusMessage());
		
		LOG.info("create new client" + MARK);
		final ClientPojoPrototype newClient = new ClientPojoPrototype().setName("A new client 3");
		LOG.info("Status message: " + service.createClient().setClient(newClient).execute().getStatusMessage());
		
		LOG.info("create new client with empty name" + MARK);
		final ClientPojoPrototype newClient2 = new ClientPojoPrototype();
		LOG.info("Status message: " + service.createClient().setClient(newClient2).execute().getStatusMessage());
		
		LOG.info("update an existing client with a new name" + MARK);
		LOG.info("Status message: " + service.updateClient().setClient(new ClientPojoPrototype()
				.setUid(foundClient.getUid())
				.setName("A new name for the client")
			).execute().getStatusMessage());
		
		LOG.info("update an existing client with an existing name" + MARK);
		LOG.info("Status message: " + service.updateClient().setClient(new ClientPojoPrototype()
				.setUid(foundClient.getUid())
				.setName("Demo Client 2")
			).execute().getStatusMessage());
		
		LOG.info("update an existing client with an empty name" + MARK);
		final ClientPojoPrototype newClient3 = new ClientPojoPrototype();
		LOG.info("Status message: " + service.updateClient().setClient(newClient3).execute().getStatusMessage());
		
		LOG.info("update a non existing client" + MARK);
		final ClientPojoPrototype newClient4 = new ClientPojoPrototype()
			.setNid(99l)
			.setName("Client does not exist");
		LOG.info("Status message: " + service.updateClient().setClient(newClient4).execute().getStatusMessage());
	}
}
