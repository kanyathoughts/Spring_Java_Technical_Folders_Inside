/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.client.ClientServiceProvider;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ClientPojoPrototype;
import innowake.mining.shared.lang.NestedMap;

/**
 * Integration tests for the {@link ClientService} service.
 */
class ClientServiceTest extends IntegrationTest {
	
	private final ClientServiceProvider clientServiceProvider = MiningApiClient.clientService(getConnectionInfo());
	private static final ClientPojoPrototype TEST_CLIENT_1 = new ClientPojoPrototype();
	private static final ClientPojoPrototype TEST_CLIENT_2 = new ClientPojoPrototype();
	
	private static final Long NON_EXISTING_ID = Long.valueOf(Long.MAX_VALUE);
	
	@BeforeAll
	public static void init() {
		final Map<String, Object> props = Map.of(CustomPropertyClass.ClientCustomProperties.name(), Map.of("customClientMandatoryProperty", "A value for the mandatory property"));
		TEST_CLIENT_1.setName("TEST CLIENT 1");
		TEST_CLIENT_1.setCustomProperties(props);
		TEST_CLIENT_2.setName("TEST CLIENT 2");
		TEST_CLIENT_2.setCustomProperties(props);
	}

	@Test
	void testFindAllClients() throws IOException {
		final Result<ClientPojo[]> resultFindAll1 = clientServiceProvider.findAllClients().execute();
		assertEquals(200, resultFindAll1.getStatusCode());
		createTestClient(TEST_CLIENT_1);
		createTestClient(TEST_CLIENT_2);
		final Result<ClientPojo[]> resultFindAll2 = clientServiceProvider.findAllClients().execute();
		assertEquals(200, resultFindAll2.getStatusCode());
		final ClientPojo[] clientsBeforeCreate = resultFindAll1.getValue().get();
		final ClientPojo[] allClients = resultFindAll2.getValue().get();
		assertEquals(clientsBeforeCreate.length + 2, allClients.length);
		assertFalse(resultContains(clientsBeforeCreate, TEST_CLIENT_1.name.getNonNull()));
		assertFalse(resultContains(clientsBeforeCreate, TEST_CLIENT_2.name.getNonNull()));
		assertTrue(resultContains(allClients, TEST_CLIENT_1.name.getNonNull()));
		assertTrue(resultContains(allClients, TEST_CLIENT_2.name.getNonNull()));
	}

	@Test
	void testFindAllClientsWithDatabase() throws IOException {
		final Result<ClientPojo[]> resultFindAll = clientServiceProvider.findAllClients().execute();
		verifyFindAll(resultFindAll.getValue().get(), findAllByJDBC());
	}

	@Test
	void testFindAllClientsWithDatabaseAndNewClients() throws IOException {
		createTestClient(TEST_CLIENT_1);
		createTestClient(TEST_CLIENT_2);
		final Result<ClientPojo[]> resultFindAll = clientServiceProvider.findAllClients().execute();
		assertEquals(200, resultFindAll.getStatusCode());
		verifyFindAll(resultFindAll.getValue().get(), findAllByJDBC());
	}
	
	@Test
	void testFindById() throws IOException {
		final ClientPojo clientOne = createTestClient(TEST_CLIENT_1);
		final Long id = clientOne.getId();
		final Result<ClientPojo> resultFind = clientServiceProvider.findClientById().setClientId(id).execute();
		assertEquals(200, resultFind.getStatusCode());
		verifyClientWithoutIdandRid(TEST_CLIENT_1, resultFind.getValue().get());
	}
	
	@Test
	void testFindByRecordId() throws IOException {
		final ClientPojo clientOne = createTestClient(TEST_CLIENT_1);
		final UUID uid = clientOne.getUid();
		final Result<ClientPojo> resultFind = clientServiceProvider.findClientById().setClientId(uid).execute();
		assertEquals(200, resultFind.getStatusCode());
		verifyClientWithoutIdandRid(TEST_CLIENT_1, resultFind.getValue().get());
	}
	
	@Test
	void testFindByIdNotFound() throws IOException {
		final Result<ClientPojo> resultFind = clientServiceProvider.findClientById().setClientId(NON_EXISTING_ID).execute();
		assertEquals(404, resultFind.getStatusCode());
		assertFalse(resultFind.getValue().isPresent());
	}
	
	@Test
	void testCreateClient() throws IOException {
		verifyClientWithoutIdandRid(TEST_CLIENT_1, createTestClient(TEST_CLIENT_1));
	}
	
	@Test
	void testCreateClientWithCustomProperties() throws IOException {
		TEST_CLIENT_1.setCustomProperties(new HashMap<>(
				Map.of(CustomPropertyClass.ClientCustomProperties.name(),
						new HashMap<>(Map.of("customClientProperty", "a created value for the custom property")))));
		
		final ClientPojo createdClient = clientServiceProvider.createClient().setClient(TEST_CLIENT_1).execute().getValue().get();
		CustomProperties.verifyNumberOfCustomProperties(createdClient, 1);
		final Object createdCustomProperty = CustomProperties.getCustomPropertyByName("customClientProperty", createdClient);
		assertEquals("a created value for the custom property", createdCustomProperty);

		final ClientPojo clientById = clientServiceProvider.findClientById().setClientId(createdClient.getId()).execute().getValue().get();
		CustomProperties.verifyNumberOfCustomProperties(clientById, 1);
		final Object foundCustomProperty = CustomProperties.getCustomPropertyByName("customClientProperty", clientById);
		assertEquals("a created value for the custom property", foundCustomProperty);
	}
	
	/*
	TODO 
		Currently we have no checks on whether a custom property is defined. 
		We could reinstate this once the Orient classes have been migrated to Postgres.
	@Test
	void testCreateClientWithNonExistingCustomProperty() throws IOException {
		final ClientPojoPrototype newClient = new ClientPojoPrototype()
			.setName("New Client")
			.setCustomProperties(new HashMap<>(Map.of(CustomPropertyClass.ClientCustomProperties.name(),new HashMap<>(Map.of("NON_EXISTING_CUSTOM_PROPERTY", "a created value for the custom property")))));

		final Result<ClientPojo> result = clientServiceProvider.createClient().setClient(newClient).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}
	
	TODO 
		So far we have no read-only custom properties in Postgres and is unclear what they were ever meant to be useful for.
	@Test
	void testClientWithReadOnlyCustomProperty() throws IOException {
		final ClientPojoPrototype newClient = new ClientPojoPrototype()
			.setName("New Client")
			.setCustomProperties(new HashMap<>(Map.of(CustomPropertyClass.ClientCustomProperties.name(),
					new HashMap<>(Map.of("customClientReadOnlyProperty", "initial value of read only property",
							"customClientMandatoryProperty", "value for mandatory property")))));

		final Result<ClientPojo> result = clientServiceProvider.createClient().setClient(newClient).execute();
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		final ClientPojo createdClient = result.getValue().get();
		
		CustomPropertiesMap props = createdClient.getCustomProperties();
		ClientPojoPrototype proto = new ClientPojoPrototype()
					.withId(createdClient.identity())
					.setCustomProperties(props);
		props.put("customClientReadOnlyProperty", "Updated value which should be not allowed");
		
		final Result<ClientPojo> updateResult = clientServiceProvider.updateClient().setClient(proto).execute();
		assertEquals(400, updateResult.getStatusCode());
		assertFalse(updateResult.getValue().isPresent());
	}
	*/
	
	@Test
	void testCreateClientDuplicate() throws IOException {
		verifyClientWithoutIdandRid(TEST_CLIENT_1, createTestClient(TEST_CLIENT_1));
		final Result<ClientPojo> resultFail = clientServiceProvider.createClient().setClient(TEST_CLIENT_1).execute();
		assertEquals(400, resultFail.getStatusCode());
		assertFalse(resultFail.getValue().isPresent());
	}
	
	@Test
	void testCreateClientWithoutName() throws IOException {
		final Result<ClientPojo> result = clientServiceProvider.createClient().setClient(new ClientPojoPrototype()).execute();
		assertEquals(400, result.getStatusCode());
		assertFalse(result.getValue().isPresent());
	}

	@Test
	void testUpdate() throws IOException {
		final ClientPojoPrototype clientExpected = getUpdateClient(createTestClient(TEST_CLIENT_1).getId());
		final Result<ClientPojo> resultUpdate = clientServiceProvider.updateClient().setClient(clientExpected).execute();
		assertEquals(200, resultUpdate.getStatusCode());
		verifyClientWithId(clientExpected, resultUpdate.getValue().get());
	}
	
	@Test
	void testUpdateCustomProperties() throws IOException {
		final ClientPojo clientByName = findByName("Demo Client 1");
		ClientPojoPrototype proto = new ClientPojoPrototype().withId(clientByName.identity())
				.setCustomProperties(new NestedMap().set("customClientProperty", "An updated value for the custom property"));
		
		final ClientPojo updatedClient = clientServiceProvider.updateClient().setClient(proto).execute().getValue().get();
		final Object updatedCustomProperty = updatedClient.getCustomProperties().getValue("customClientProperty");
		assertEquals("An updated value for the custom property", updatedCustomProperty);
		
		final ClientPojo updatedClientByName = findByName("Demo Client 1");
		final Object updatedFoundCustomProperty = updatedClientByName.getCustomProperties().getValue("customClientProperty");
		assertEquals("An updated value for the custom property", updatedFoundCustomProperty);
	}

	@Test
	void testUpdateClientNotFound() throws IOException {
		final ClientPojoPrototype clientExpected = getUpdateClient(NON_EXISTING_ID);
		final Result<ClientPojo> resultUpdate = clientServiceProvider.updateClient().setClient(clientExpected).execute();
		assertEquals(404, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateUniqueName() throws IOException {
		final ClientPojoPrototype clientExpected = getUpdateClient(createTestClient(TEST_CLIENT_1).getId());
		final ClientPojoPrototype clientExpected2 = getUpdateClient(createTestClient(TEST_CLIENT_2).getId());
		final Result<ClientPojo> resultUpdate = clientServiceProvider.updateClient().setClient(clientExpected).execute();
		assertEquals(200, resultUpdate.getStatusCode());
		final Result<ClientPojo> resultUpdate3 = clientServiceProvider.updateClient().setClient(clientExpected2).execute();
		assertEquals(400, resultUpdate3.getStatusCode());
		verifyClientWithId(clientExpected, resultUpdate.getValue().get());
		assertFalse(resultUpdate3.getValue().isPresent());
	}

	@Test
	void testUpdateClientTwiceWithSameName() throws IOException {
		final ClientPojoPrototype clientExpected = getUpdateClient(createTestClient(TEST_CLIENT_1).getId());
		final Result<ClientPojo> resultUpdate = clientServiceProvider.updateClient().setClient(clientExpected).execute();
		assertEquals(200, resultUpdate.getStatusCode());
		final Result<ClientPojo> resultUpdate2 = clientServiceProvider.updateClient().setClient(clientExpected).execute();
		assertEquals(200, resultUpdate2.getStatusCode());
		verifyClientWithId(clientExpected, resultUpdate2.getValue().get());
	}

	@Test
	void testUpdateWithoutName() throws IOException {
		final ClientPojoPrototype clientExpected = new ClientPojoPrototype();
		clientExpected.setNid(createTestClient(TEST_CLIENT_1).getId());
		final Result<ClientPojo> resultUpdate = clientServiceProvider.updateClient().setClient(clientExpected).execute();
		assertEquals(400, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}

	@Test
	void testUpdateNonExistingWithoutName() throws IOException {
		final ClientPojoPrototype clientExpected = new ClientPojoPrototype();
		clientExpected.setNid(NON_EXISTING_ID);
		final Result<ClientPojo> resultUpdate = clientServiceProvider.updateClient().setClient(clientExpected).execute();
		assertEquals(404, resultUpdate.getStatusCode());
		assertFalse(resultUpdate.getValue().isPresent());
	}
	
	private ClientPojoPrototype getUpdateClient(final Long id) {
		final ClientPojoPrototype client = new ClientPojoPrototype();
		client.setNid(id);
		client.setName("UPDATE TEST CLIENT");
		return client;
	}
	
	private boolean resultContains(final ClientPojo[] result, final String clientName) {
		for (int i = 0; i < result.length; i++) {
			if (clientName.equals(result[i].getName())) {
				return true;
			}
		}
		return false;
	}
	
	private void verifyClientWithoutIdandRid(final ClientPojoPrototype expected, final ClientPojo actual) {
		assertEquals(expected.name.get(), actual.getName());
	}
	
	private void verifyClientWithId(final ClientPojoPrototype expected, final ClientPojo actual) {
		assertEquals(expected.nid.get(), actual.getId());
		assertEquals(expected.name.get(), actual.getName());
	}

	private Map<Long, String> findAllByJDBC() {
		final Map<Long, String> result = new HashMap<>();
		try {
			if (connectionPostgres != null) {
				try (final Statement stmt = connectionPostgres.createStatement()) {
					stmt.execute("SELECT nid, name FROM client where nid > 0");
					final ResultSet resultSet = stmt.getResultSet();
					while (resultSet.next()) {
						result.put((Long) resultSet.getObject("nid"), resultSet.getString("name"));
					}
				}
			} else {
				throw new IllegalStateException("Database connection was lost.");
			}
		} catch (final SQLException e) {
			throw new IllegalStateException(e);
		}
		return result;
	}
	
	private void verifyFindAll(final ClientPojo[] clients, final Map<Long,String> databaseResult) {
		for (final ClientPojo client : clients) {
			final Long id = client.getId();
			final String name = client.getName();
			final String expectedName = databaseResult.get(id);
			assertNotNull(expectedName);
			assertEquals(expectedName, name);
			databaseResult.remove(id);
		}
		assertTrue(databaseResult.isEmpty());
	}
	
	private ClientPojo createTestClient(final ClientPojoPrototype client) throws IOException {
		final Result<ClientPojo> result = clientServiceProvider.createClient().setClient(client).execute();
		assertEquals(201, result.getStatusCode());
		assertTrue(result.getValue().isPresent());
		return result.getValue().get();
	}

	private ClientPojo findByName(final String name) throws IOException {
		final Result<ClientPojo[]> resultFindAll = clientServiceProvider.findAllClients().execute();
		final ClientPojo[] clients = resultFindAll.getValue().get();
		for (final ClientPojo client : clients) {
			if (client.getName().equals(name)) {
				return client;
			}
		}
		final String message = String.format("A client with the name of %s does not exist", name);
		throw new IllegalArgumentException(message);
	}
}
