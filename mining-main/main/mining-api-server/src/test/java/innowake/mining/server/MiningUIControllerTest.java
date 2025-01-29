/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server;

import static org.junit.Assert.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.NoAuthSecurityConfig;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.MiningUiController;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.CookieIdVerifier;
import software.amazon.awssdk.utils.StringUtils;

/**
 * Mocked tests verifying the working of the {@link MiningUiController} with the authorized access.
 */
@WebMvcTest(MiningUiController.class)
@Import({ CookieIdVerifier.class, BuildProperties.class, NoAuthSecurityConfig.class })
@ActiveProfiles(Profiles.NO_AUTH)
@Tag("mocked")
@TestPropertySource(properties="mining.cookieId=d09526d5-4e1f-47b1-8035-cdd8e267112a-test")
class MiningUIControllerTest extends MockedBaseTest {

	@Autowired
	private MockMvc mvc;

	@Autowired
	private MiningUiController miningUIController;
	
	@Nullable
	@MockBean
	private GenericConfigProperties genericConfigProperties;
	
	@Nullable
	@MockBean
	private DiscoveryPersistenceImpl orientDiscoveryPersistence;

	/**
	 * Tests to check whether index.html contains cookie consent script or not.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void checkCookieConsent() throws Exception {

		final String cookieId = miningUIController.getCookieId();
		assertTrue( ! StringUtils.isBlank(cookieId));
		final MvcResult result = mvc.perform(get("/index.html"))
										.andDo(print())
										.andExpect(status().isOk())
										.andReturn();

		assertTrue(result.getResponse().getContentAsString().contains(cookieId));
	}
}
