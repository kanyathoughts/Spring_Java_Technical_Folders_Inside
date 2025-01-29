/*
 * Copyright (c) 2024 Deloitte. All rights reserved. 
 */
package innowake.mining.server.filter;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.MiningUiExtensionsController;
import innowake.mining.server.integration.DatabaseRelatedTest;

/**
 * Test for {@link ContentSecurityPolicyFilter} to ensure that the Content-Security-Policy header is added.
 */
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@AutoConfigureMockMvc()
class ContentSecurityPolicyFilterTests extends DatabaseRelatedTest {

	@Autowired
	private MockMvc mvc;
	
	/**
	 * Test when starting the {@link MiningApiApplication} without setting ui.base-url that 'self' is used as the origin.
	 * @throws Exception
	 */
	@Test
	void testCspHeaderBaseUrlNotSetDefaultsToSelf() throws Exception {
		final String expected = 
				"default-src 'self' https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/; " + 
				"script-src 'self' https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline' 'wasm-unsafe-eval'; " +
				"style-src 'self' https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline'; " +
				"img-src 'self' https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ data:; " +
				"font-src 'self' https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ data:; " +
				"worker-src 'self' https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline' * blob:;";
		
		mvc.perform(get("/api" + MiningUiExtensionsController.EXTENSION_LIST_URL).contentType(MediaType.APPLICATION_JSON_VALUE))
				.andExpect(status().isOk())
				.andExpect(header().exists("Content-Security-Policy"))
				.andExpect(header().string("Content-Security-Policy", expected))
				.andReturn();
	}
	
	/**
	 * Test when starting the {@link MiningApiApplication} and setting ui.base-url that the value of ui.base-url is used as the origin.
	 * @throws Exception
	 */
	@Test
	void testCspHeaderBaseUrlSetUsesBaseUrl() throws Exception {
		final String oldValue = System.getProperty("ui.base-url");
		try {
			System.setProperty("ui.base-url", "http://localhost:4200");
			final String expected = 
					"default-src http://localhost:4200 https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/; " + 
					"script-src http://localhost:4200 https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline' 'wasm-unsafe-eval'; " + 
					"style-src http://localhost:4200 https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline'; " + 
					"img-src http://localhost:4200 https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ data:; " +
					"font-src http://localhost:4200 https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ data:; " +
					"worker-src http://localhost:4200 https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline' * blob:;";
			
			mvc.perform(get("/api" + MiningUiExtensionsController.EXTENSION_LIST_URL).contentType(MediaType.APPLICATION_JSON_VALUE))
					.andExpect(status().isOk())
					.andExpect(header().exists("Content-Security-Policy"))
					.andExpect(header().string("Content-Security-Policy", expected))
					.andReturn();
		} finally {
			if (oldValue != null) {
				System.setProperty("ui.base-url", oldValue);
			} else {
				System.clearProperty("ui.base-url");
			}
		}
	}
}
