/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.extensions;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.MiningUiExtensionsController;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.extensions.MiningWebUiExtension;

/**
 * Test for the {@link MiningWebUiExtension} to verify the behavior when multiple extensions are installed.
 */
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@AutoConfigureMockMvc
@Import(MultipleMiningWebUiExtensionTests.TestConfig.class)
class MultipleMiningWebUiExtensionTests extends DatabaseRelatedTest {
	
	@Autowired
	private MockMvc mvc;
	
	/*
	 * That that Content-Security-Policy is correctly added for multiple origins.
	 */
	@Test
	void testWebUiExtensionsCsp() throws Exception {
		final String expectedCsp =
				"default-src 'self' https://my-subdomain.deloitte.com http://client.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/; " + 
				"script-src 'self' https://my-subdomain.deloitte.com http://client.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline' 'wasm-unsafe-eval'; " +
				"style-src 'self' https://my-subdomain.deloitte.com http://client.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline'; " +
				"img-src 'self' https://my-subdomain.deloitte.com http://client.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ data:; " +
				"font-src 'self' https://my-subdomain.deloitte.com http://client.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ data:; " +
				"worker-src 'self' https://my-subdomain.deloitte.com http://client.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline' * blob:;";
	
		mvc.perform(get("/api" + MiningUiExtensionsController.EXTENSION_LIST_URL).contentType(MediaType.APPLICATION_JSON_VALUE))
				.andDo(print())
				.andExpect(status().isOk())
				.andExpect(header().exists("Content-Security-Policy"))
				.andExpect(header().string("Content-Security-Policy", expectedCsp))
				.andReturn();
	}
	
	/**
	 * Configuration for the test. 
	 */
	@TestConfiguration
	static class TestConfig {

		/**
		 * Test extensions for performing the tests.
		 * 
		 * @return MiningWebUiExtension instances.
		 */
		@Bean
		public MiningWebUiExtension[] miningWebUiExtensions() {
			final List<MiningWebUiExtension> extensions = List.of(
					
				/* A {@link MiningWebUiExtension} that uses https and a subdomain. */
				new MiningWebUiExtension() {
					@Override
					public String getName() {
						return "Test Extension 1";
					}
					
					@Override
					public String getPageIdentifier() {
						return "testIdentifier1";
					}				

					@Override
					public Kind getKind() {
						return Kind.IFRAME;
					}

					@Override
					public Map<Property, String> getProperties() {
						final Map<Property, String> props = new HashMap<>();
						props.put(
								Property.IFRAME_SRC, 
								"https://my-subdomain.deloitte.com/my/extension-url/extension-ui.html");
						return props;
					}

					
				},
				
				/* A {@link MiningWebUiExtension} that uses http and no subdomain. */
				new MiningWebUiExtension() {

					@Override
					public String getName() {
						return "Test Extension 2";
					}

					@Override
					public String getPageIdentifier() {
						return "testIdentifier2";
					}

					@Override
					public Kind getKind() {
						return Kind.IFRAME;
					}

					@Override
					public Map<Property, String> getProperties() {
						final Map<Property, String> props = new HashMap<>();
						props.put(
								Property.IFRAME_SRC, 
								"http://client.com/my/extension-url/extension-ui.html");
						return props;
					}
					
				});
			
			return extensions.stream().toArray(MiningWebUiExtension[]::new);
		}
	}
}
