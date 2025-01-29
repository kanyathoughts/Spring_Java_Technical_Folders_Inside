/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.server.controller.ClientController.CLIENT_BY_ID_URL;
import static innowake.mining.server.controller.ClientController.CLIENT_COLLECTION_URL;
import static org.springframework.http.HttpMethod.GET;

import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link ClientController}.
 */
class ClientControllerTest extends BaseClientControllerTest {

	@Override
	String getCollectionUrl() {
		return CLIENT_COLLECTION_URL;
	}

	/**
	 * Tests {@link ClientController#findById(HttpServletRequest, HttpServletResponse, String)} for a client that was marked as to be deleted.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testFindByIdForToBeDeletedClient() throws Exception {
		testClientMarkedAsDeleted(GET, CLIENT_BY_ID_URL, UUID::toString);
	}
}
