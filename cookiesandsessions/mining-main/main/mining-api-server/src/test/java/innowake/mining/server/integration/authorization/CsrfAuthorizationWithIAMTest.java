/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.integration.authorization;

import org.junit.jupiter.api.Disabled;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.server.service.KeycloakAuthorizationManagementService;

/**
 * Tests validating the behavior of csrf with different user-agent
 */
@ActiveProfiles(value = Profiles.AUTH_TEST, inheritProfiles = false )
@AutoConfigureMockMvc
@WithMockUser(authorities = {"admin"})
@Disabled("CSRF protection temporarily disabled - WMIN-5382 will re-enable")
class CsrfAuthorizationWithIAMTest extends CsrfAuthorizationTest {
	
	@Nullable
	@MockBean
	private RestTemplate keycloakRestTemplate;

	@MockBean
	@Nullable
	private KeycloakApplicationConfiguration config;

	@Nullable
	@SpyBean
	private KeycloakAuthorizationManagementService authorizationManagementService;
}
