/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static org.junit.Assert.assertFalse;
import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.UUID;
import java.util.function.Function;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.HttpMethod;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ClientPojo;

/**
 * Abstract base class for testing the {@link ClientController} and {@link ClientControllerV2}.
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false)
abstract class BaseClientControllerTest extends DatabaseRelatedTest {

	@Autowired
	protected MockMvc mvc;

	@Autowired
	protected ClientService clientService;

	/* counter for createTestData() for unique client names */
	private static int testCount = 1;

	/**
	 * @return the URL pattern for collecting all clients
	 */
	abstract String getCollectionUrl();

	/**
	 * Generic test method that creates a test client and then performs the call. The call must return HTTP status code {@code 200}.
	 * <p>After the first call was successful the client is marked as to be deleted in the DB and the call is executed again. The call must
	 * return HTTP status code {@code 404}.
	 *
	 * @param url The URL of the call
	 * @param uriVarSupplier A {@link Function} that returns the URI variable for the call, id or rid of the test client
	 * @throws Exception when the mocked call was not successful
	 */
	void testClientMarkedAsDeleted(final HttpMethod method, final String url, final Function<UUID, ?> uriVarSupplier) throws Exception {
		final ClientPojo client = clientService.create("NotYetDeletedClient" + testCount++);
		final Object uriVar = uriVarSupplier.apply(client.getUid());

		/* Call for client which is not marked as deleted */
		mvc.perform(request(method, "/api" + url, uriVar)
				.contentType(APPLICATION_JSON))
				.andExpect(status().isOk());

		if (method != HttpMethod.DELETE) {
			clientService.markForDeletion(client.identity());
		}

		/* must always be 404 after client was marked as to be deleted */
		mvc.perform(request(method, "/api" + url, uriVar)
				.contentType(APPLICATION_JSON))
				.andExpect(status().isNotFound());
	}

	/**
	 * Tests that both {@link ClientController#findAll(HttpServletRequest, HttpServletResponse)} and
	 * {@link ClientControllerV2#findAll(HttpServletRequest, HttpServletResponse)} return only clients that are NOT marked as to be deleted.
	 * 
	 * @throws Exception if request failed
	 */
	@Test
	void testFindAllExceptToBeDeletedClients() throws Exception {
		final EntityId clientId = clientService.markForDeletion(clientService.create("ToBeDeletedClient" + testCount++).identity());

		final MvcResult result = mvc.perform(get("/api" + getCollectionUrl())
				.contentType(APPLICATION_JSON))
				.andExpect(status().isOk())
				.andReturn();

		assertFalse(result.getResponse().getContentAsString().contains(clientId.toString()));
	}

	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}

}
