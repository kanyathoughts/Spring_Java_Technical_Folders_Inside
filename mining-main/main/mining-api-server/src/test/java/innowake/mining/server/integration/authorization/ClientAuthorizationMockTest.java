/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import innowake.mining.server.config.Profiles;

import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ClientPojo;

/**
 * Tests for checking the filtered collections of the /clients end points.
 */
@ActiveProfiles(value = Profiles.AUTH_TEST, inheritProfiles = false)
@AutoConfigureMockMvc
class ClientAuthorizationMockTest extends AuthorizationTests {
	
	@Autowired
	private MockMvc mockMvc;
	
	@Autowired
	private ClientService clientService;

	/**
	 * Check empty result on /v2/clients without any mining roles.
	 *
	 * @throws Exception when something with the call fails
	 */
	@Test
	@WithMockUser
	public void userWithoutMiningRolesGetsAnEmptyResponse() throws Exception {
		mockMvc.perform(get("/api/v2/clients"))
		.andDo(print())
		.andExpect(status().isOk())
		.andExpect(jsonPath("$.size").value(Long.valueOf(0)))
		.andExpect(jsonPath("$.content").isEmpty());
	}

	/**
	 * Check non-empty result on /v2/clients with 1 client-related role.
	 *
	 * @throws Exception when something with the call fails
	 */
	@Test
	void userWithMiningRolesGetsTheCorrespondingClients() throws Exception {
		mockMvc.perform(get("/api/v2/clients").with(miningUser(miningRoles("client-1-project-1-manager", "client-1-project-1-mining"))))
		.andDo(print())
		.andExpect(status().isOk())
		.andExpect(jsonPath("$.size").value(Long.valueOf(1)))
		.andExpect(jsonPath("$.content").isNotEmpty())
		.andExpect(jsonPath("$.content[0].id").value(Long.valueOf(1)));
	}
	
	/**
	 * Check result with multiple objects on /v2/clients with 2 client-related roles.
	 *
	 * @throws Exception when something with the call fails
	 */
	@Test
	void userWithTwoMiningRolesGetsTheCorrespondingClients() throws Exception {
		mockMvc.perform(get("/api/v2/clients").with(miningUser(miningRoles("client-1-project-1-manager", "client-2-project-3-mining"))))
		.andDo(print())
		.andExpect(status().isOk())
		.andExpect(jsonPath("$.size").value(Long.valueOf(2)))
		.andExpect(jsonPath("$.content").isNotEmpty());
	}

	/**
	 * Check empty result on /v1/clients without any mining roles.
	 *
	 * @throws Exception when something with the call fails
	 */
	@Test
	@WithMockUser
	void userWithoutMiningRolesGetsAnEmptyResponseV1() throws Exception {
		mockMvc.perform(get("/api/v1/clients"))
		.andDo(print())
		.andExpect(status().isOk())
		.andExpect(jsonPath("$").isEmpty());
	}

	/**
	 * Check non-empty result on /v1/clients with 1 client-related role.
	 *
	 * @throws Exception when something with the call fails
	 */
	@Test
	void userWithMiningRolesGetsTheCorrespondingClientsV1() throws Exception {
		mockMvc.perform(get("/api/v1/clients").with(miningUser(miningRoles("client-1-project-1-manager", "client-1-project-1-mining"))))
		.andDo(print())
		.andExpect(status().isOk())
		.andExpect(jsonPath("$", hasSize(1)))
		.andExpect(jsonPath("$[0].id").value(Long.valueOf(1)));
	}
	
	/**
	 * Check result with multiple objects on /v1/clients with 2 client-related roles.
	 *
	 * @throws Exception when something with the call fails
	 */
	@Test
	void userWithTwoMiningRolesGetsTheCorrespondingClientsV1() throws Exception {
		mockMvc.perform(get("/api/v1/clients").with(miningUser(miningRoles("client-1-project-1-manager", "client-2-project-3-mining"))))
		.andDo(print())
		.andExpect(status().isOk())
		.andExpect(jsonPath("$", hasSize(2)))
		.andExpect(jsonPath("$[0].id").value(Long.valueOf(1)))
		.andExpect(jsonPath("$[1].id").value(Long.valueOf(2)));
	}
	
	/**
	 * Check retrieval of clients by record ID fails if the ID of the associated record ID does not match.
	 *
	 * @throws Exception when something with the call fails
	 */
	@Test
	void retrieveClient1ByRecordIdFailsWithoutAppropriateRights() throws Exception {
		final ClientPojo client = clientService.find(EntityId.of(Long.valueOf(1)), false)
			.orElseThrow(() -> new AssertionError("Client with ID 1 should be in the database"));
		mockMvc.perform(get("/api/v1/clients/{recordId}", client.getUid())
							.with(miningUser(miningRoles("client-2-project-3-mining", "client-2-project-3-editor"))))
				.andDo(print())
				.andExpect(status().isForbidden());
	}

	/**
	 * Check retrieval of clients by record ID succeeds if the ID of the associated record ID matches.
	 *
	 * @throws Exception when something with the call fails
	 */
	@Test
	void retrieveClient1ByRecordIdSucceedsWithAppropriateRights() throws Exception {
		final ClientPojo client = clientService.find(EntityId.of(Long.valueOf(1)), false)
				.orElseThrow(() -> new AssertionError("Client with ID 1 should be in the database"));
		mockMvc.perform(get("/api/v1/clients/{recordId}", client.getId())
							.with(miningUser(miningRoles("client-1-project-1-mining", "client-1-project-1-viewer"))))
				.andDo(print())
				.andExpect(status().isOk());
	}
}
