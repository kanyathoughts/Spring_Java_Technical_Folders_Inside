/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.integration.authorization;

import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpStatus.CREATED;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.entities.ClientPojoPrototype;
import innowake.mining.tags.AuthorizationTest;

/**
 * Tests validating the behavior of csrf with different user-agent
 */
@AuthorizationTest
abstract class CsrfAuthorizationTest extends DatabaseRelatedTest {

	@Autowired
	private MockMvc mockMvc;

	@ParameterizedTest
	@ValueSource(strings = {"Chrome", "Edge", "Firefox", "Safari", "Opera", "Trident", "MSIE"})
	void csrfWithBrowserBasedClientWithoutToken(final String userAgent) throws Exception {
		final ClientPojoPrototype client = new ClientPojoPrototype().setName("ClientWith" + userAgent);
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(client);

		mockMvc.perform(
				request(POST, "/api/v2/clients/").contentType(MediaType.APPLICATION_JSON).content(jsonForCreation).header("user-agent", userAgent))
				.andExpect(status().isForbidden());
	}

	@ParameterizedTest
	@ValueSource(strings = {"Chrome", "Edge", "Firefox", "Safari", "Opera", "Trident", "MSIE"})
	void testShouldCreateClient(final String userAgent) throws Exception {
		final ClientPojoPrototype client = new ClientPojoPrototype().setName("ClientWith" + userAgent);
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(client);

		mockMvc.perform(
				request(POST, "/api/v2/clients/").with(csrf()).contentType(MediaType.APPLICATION_JSON).content(jsonForCreation).header("user-agent", userAgent))
				.andExpect(status().is(CREATED.value()));
	}
	
	@Test
	void csrfWithNonBrowserBasedClientWithOutToken() throws Exception {
		final ClientPojoPrototype client = new ClientPojoPrototype().setName("ClientWithOutToken");
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(client);

		mockMvc.perform(
				request(POST, "/api/v2/clients/").contentType(MediaType.APPLICATION_JSON).content(jsonForCreation).header("user-agent", "abc"))
				.andExpect(status().is(CREATED.value()));
	}

	@Test
	void csrfWithNonBrowserBasedClientWithOutTokenWithOutUserAgent() throws Exception {
		final ClientPojoPrototype client = new ClientPojoPrototype().setName("ClientWith");
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(client);

		mockMvc.perform(
				request(POST, "/api/v2/clients/").contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
				.andExpect(status().is(CREATED.value()));
	}

}
