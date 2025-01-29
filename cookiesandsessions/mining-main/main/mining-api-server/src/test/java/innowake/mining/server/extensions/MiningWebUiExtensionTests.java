/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.extensions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;

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
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.MiningUiExtensionsController;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.extensions.MiningWebUiExtension;
import innowake.mining.shared.extensions.WebUiExtensionDescription;

/**
 * Test for the {@link MiningUiExtensionsController} to verify the behavior in default Spring profile.
 */
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
@AutoConfigureMockMvc
@Import(MiningWebUiExtensionTests.TestConfig.class)
class MiningWebUiExtensionTests extends DatabaseRelatedTest {

	@Autowired
	private MockMvc mvc;
	
	/**
	 * Make sure that webUIextensions are present and contains the correct value.
	 *
	 * @throws Exception when the call was not successful 
	 */
	@Test
	void testWebUIExtension() throws Exception {
		final String expectedCsp =
				"default-src 'self' https://my-subdomain.deloitte.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/; " + 
				"script-src 'self' https://my-subdomain.deloitte.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline' 'wasm-unsafe-eval'; " +
				"style-src 'self' https://my-subdomain.deloitte.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline'; " +
				"img-src 'self' https://my-subdomain.deloitte.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ data:; " +
				"font-src 'self' https://my-subdomain.deloitte.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ data:; " +
				"worker-src 'self' https://my-subdomain.deloitte.com https://unpkg.com/graphiql/ https://unpkg.com/react@16/ https://unpkg.com/react-dom@16/ 'unsafe-inline' * blob:;";
		
		final MvcResult result = mvc.perform(get("/api" + MiningUiExtensionsController.EXTENSION_LIST_URL).contentType(MediaType.APPLICATION_JSON_VALUE))
				.andDo(print())
				.andExpect(status().isOk())
				.andExpect(header().exists("Content-Security-Policy"))
				.andExpect(header().string("Content-Security-Policy", expectedCsp))
				.andReturn();
		
		final String extensionInfoJson = result.getResponse().getContentAsString();
		final List<WebUiExtensionDescription> extensionInfo = PojoMapper
				.jsonReaderFor(new TypeReference<List<WebUiExtensionDescription>>() {}).readValue(extensionInfoJson);
		assertEquals(1, extensionInfo.size());
		assertEquals("Test Name", extensionInfo.get(0).getName());
	}
	
	/**
	 * Configuration for the test. 
	 */
	@TestConfiguration
	static class TestConfig {

		/**
		 * Test extension for performing the test.
		 * 
		 * @return MiningWebUiExtension instance.
		 */
		@Bean
		public MiningWebUiExtension miningWebUiExtension() {
			return new MiningWebUiExtension() {

				@Override
				public String getName() {
					return "Test Name";
				}

				@Override
				public Kind getKind() {
					return Kind.IFRAME;
				}

				@Override
				public Map<Property, String> getProperties() {
					final Map<Property, String> props = new HashMap<>();
					props.put(Property.IFRAME_SRC, "https://my-subdomain.deloitte.com/my/extension-url/extension-ui.html?param1=${value1}&param2=${value2}");
					return props;
				}

				@Override
				public String getPageIdentifier() {
					return "testIdentifier";
				}
			};
		}
	}
}
