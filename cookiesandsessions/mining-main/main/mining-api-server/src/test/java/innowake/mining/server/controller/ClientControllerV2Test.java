/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.server.controller.ClientControllerV2.CLIENT_BY_ID_URL;
import static innowake.mining.server.controller.ClientControllerV2.CLIENT_COLLECTION_URL;
import static org.springframework.http.HttpMethod.DELETE;

import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link ClientControllerV2}.
 */
class ClientControllerV2Test extends BaseClientControllerTest {

	@Override
	String getCollectionUrl() {
		return CLIENT_COLLECTION_URL;
	}

	/**
	 * Tests {@link ClientControllerV2#deleteClient(HttpServletRequest, HttpServletResponse, Long)} for a client that was marked as to be deleted.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testDeleteClientForToBeDeletedClient() throws Exception {
		testClientMarkedAsDeleted(DELETE, CLIENT_BY_ID_URL, UUID::toString);
	}
}
