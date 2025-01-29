/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.time.Instant;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ClientPojo;
import junit.framework.AssertionFailedError;

/**
 * Test to verify Client service
 */
@TestMethodOrder(OrderAnnotation.class)
class ClientServiceTest extends DatabaseRelatedTest {

	@Autowired
	private ClientService clientService;
	
	@Nullable
	private ClientPojo createdClient;
	
	private static final Long ONE = Long.valueOf(1);
	private static final Long TWO = Long.valueOf(2);
	
	@Test
	@Order(1)
	void testCreateClient() {
		createdClient = clientService.create("Test Client " + Instant.now().toString());
		assertNotNull(createdClient);
	}
	
	@Test
	@Order(2)
	void testFindClient() {
		final ClientPojo client = clientService.find(assertNotNull(createdClient).getUid())
				.orElseThrow(() -> new AssertionFailedError("Client not found"));
		final ClientPojo clientById = clientService.find(EntityId.of(client.getId()), false)
				.orElseThrow(() -> new AssertionFailedError("Client not found by nID"));
		assertEquals(assertNotNull(createdClient).getUid(), clientById.getUid());
	}
	
	@Test
	@Order(3)
	void deleteById() {
		clientService.deleteDirectly(assertNotNull(createdClient).identity());
		assertFalse(clientService.find(assertNotNull(createdClient).identity(), false).isPresent());
	}
	
	@Test
	void testFindById() {
		assertTrue("Should return client for given Id", clientService.find(EntityId.of(ONE), false).isPresent());		
	}
	
	@Test
	void testFindClientAboveId() {
		final List<ClientPojo> clientList = clientService.find(q -> q
				.withIdAbove(ONE)
				.withMarkedForDeletion(false).sortNid(SortDirection.ASCENDING));
		assertFalse("Client list should not be empty", clientList.isEmpty());		
	}
	
	@Test
	void testMarkForDeletion() throws Exception {
		assertNotNull(clientService.markForDeletion(EntityId.of(ONE)));
		clientService.deleteClients();
		assertTrue("Should not return deleted client", clientService.find(EntityId.of(ONE), false).isEmpty());		
	}
	
	@Test
	void testFindAll() {
		final List<ClientPojo> clientList = clientService.find(q -> q.withIds(Arrays.asList(ONE,TWO))
																	 .withMarkedForDeletion(false)
																	 .sortNid(SortDirection.ASCENDING));
		assertEquals(TWO, Long.valueOf(clientList.size()));
	}

}
