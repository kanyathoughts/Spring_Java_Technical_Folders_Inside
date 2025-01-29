package innowake.mining.server;

import static org.junit.Assert.fail;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.AuthTestSecurityConfig;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.KeycloakConfigurationController;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.server.service.CookieIdVerifier;

/**
 * Mocked tests verifying the behavior of the {@link KeycloakConfigurationController}.
 */
@WebMvcTest(KeycloakConfigurationController.class)
@Import({ CookieIdVerifier.class, BuildProperties.class, AuthTestSecurityConfig.class})
@ActiveProfiles(Profiles.AUTH_TEST)
@Tag("mocked")
class KeycloakConfigurationMockedControllerTests extends MockedBaseTest {

	@Autowired
	private MockMvc mvc;
	
	@MockBean
	@Nullable
	private KeycloakApplicationConfiguration config;
	
	@Nullable
	@MockBean
	private GenericConfigProperties genericConfigProperties;
	
	@Nullable
	@MockBean
	private DiscoveryPersistenceImpl orientDiscoveryPersistence;
	
	/**
	 * Setup the Keycloak application configuration mock.
	 */
	@Override
	@BeforeEach
	public void setup() {
		super.setup();
		
		final KeycloakApplicationConfiguration config2 = config;
		if (config2 != null) {
			given(config2.getAuthServerUrl()).willReturn("mock-host");
			given(config2.getRealm()).willReturn("mock-realm");
			given(config2.getSslRequired()).willReturn("mock-ssl");
			given(config2.getClientId()).willReturn("mock-client-id");
		} else {
			fail("KeycloakApplicationConfiguration was not properly injected.");
		}
	}
	
	/**
	 * Make sure that the Keycloak configuration for the eclipse client is served and contains the correct values.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void eclipseConfigurationExists() throws Exception {
		mvc.perform(get("/keycloak-eclipse.json").contentType(MediaType.APPLICATION_JSON))
		   .andDo(print())
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$.resource").value("eclipse"))
		   .andExpect(jsonPath("$.public-client").value(Boolean.TRUE))
		   .andExpect(jsonPath("$.enable-pkce").value(Boolean.TRUE))
		   .andExpect(jsonPath("$.principal-attribute").value("preferred_username"))
		   .andExpect(jsonPath("$.realm").value("mock-realm"))
		   .andExpect(jsonPath("$.auth-server-url").value("mock-host"))
		   .andExpect(jsonPath("$.ssl-required").value("mock-ssl"));
	}

	/**
	 * Make sure that the Keycloak configuration for the web client is served and contains the correct values.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void webConfigurationExists() throws Exception {
		mvc.perform(get("/keycloak-web.json").contentType(MediaType.APPLICATION_JSON))
		   .andDo(print())
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$.resource").value("web"))
		   .andExpect(jsonPath("$.public-client").value(Boolean.TRUE))
		   .andExpect(jsonPath("$.enable-pkce").value(Boolean.TRUE))
		   .andExpect(jsonPath("$.principal-attribute").value("preferred_username"))
		   .andExpect(jsonPath("$.realm").value("mock-realm"))
		   .andExpect(jsonPath("$.auth-server-url").value("mock-host"))
		   .andExpect(jsonPath("$.client-id").value("mock-client-id"))
		   .andExpect(jsonPath("$.ssl-required").value("mock-ssl"));
	}
}
