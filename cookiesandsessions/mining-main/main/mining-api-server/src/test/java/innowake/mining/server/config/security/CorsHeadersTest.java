/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.config.security;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.HttpHeaders;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import innowake.mining.server.integration.DatabaseRelatedTest;

/**
 * Tests for verifying the CORS headers in the application.
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = "dev")
class CorsHeadersTest extends DatabaseRelatedTest {
	
	private static final String ACCESS_TOKEN = "5COgpjv7HdUiRQrj0LwEsyiJnhE";
	private static final String CLIENT_COLLECTION = "/api/v1/clients";
	
	@Autowired
	private MockMvc mockMvc;
	
	@Test
	void corsConfigurationTest() throws Exception {
		final String corsOrigin = "http://localhost:4200";
		this.mockMvc.perform(MockMvcRequestBuilders.get(CLIENT_COLLECTION)
				.header(HttpHeaders.ORIGIN, corsOrigin)
				.header(HttpHeaders.AUTHORIZATION, "Bearer " + ACCESS_TOKEN))
				.andExpect(MockMvcResultMatchers.status().isOk())
				.andExpect(MockMvcResultMatchers.header().string(HttpHeaders.ACCESS_CONTROL_ALLOW_ORIGIN, corsOrigin))
				.andExpect(MockMvcResultMatchers.header().string(HttpHeaders.ACCESS_CONTROL_ALLOW_CREDENTIALS, "true"));
	}
	
	@Test
	void incorrectOriginConfigurationTest() throws Exception {
		this.mockMvc.perform(MockMvcRequestBuilders.get(CLIENT_COLLECTION)
				.header(HttpHeaders.ORIGIN, "http://localhost:9000")
				.header(HttpHeaders.AUTHORIZATION, "Bearer " + ACCESS_TOKEN))
				.andExpect(MockMvcResultMatchers.status().isForbidden())
				.andExpect(MockMvcResultMatchers.header().doesNotExist(HttpHeaders.ACCESS_CONTROL_ALLOW_ORIGIN));
	}
}
