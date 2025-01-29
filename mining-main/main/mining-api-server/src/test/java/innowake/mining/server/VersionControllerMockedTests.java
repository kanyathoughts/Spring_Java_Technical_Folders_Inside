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
import innowake.mining.server.controller.VersionController;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.server.service.CookieIdVerifier;

/**
 * Mocked tests verifying the behavior of the {@link VersionController} with the authorized access.
 */
@WebMvcTest(VersionController.class)
@Import({ CookieIdVerifier.class, BuildProperties.class, AuthTestSecurityConfig.class })
@ActiveProfiles(Profiles.AUTH_TEST)
@Tag("mocked")
class VersionControllerMockedTests extends MockedBaseTest {

	@Autowired
	private MockMvc mvc;
	
	@MockBean
	@Nullable
	private BuildProperties buildProperties;
	
	@Nullable
	@MockBean
	private GenericConfigProperties genericConfigProperties;
	
	@Nullable
	@MockBean
	private DiscoveryPersistenceImpl orientDiscoveryPersistence;

	private static final String TEST_VERSION = "19.5.00";
	
	/**
	 * Setup the build properties mock.
	 */
	@Override
	@BeforeEach
	public void setup() {
		super.setup();
		final BuildProperties buildProperties2 = buildProperties;
		if (buildProperties2 != null) {
			given(buildProperties2.getVersion()).willReturn(TEST_VERSION);
		} else {
			fail("BuildProperties was not properly injected.");
		}
	}

	/**
	 * Tests version retrieval.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void versionRetrieval() throws Exception {
		mvc.perform(get("/api" + VersionController.API_SERVER_VERSION_URL).contentType(MediaType.APPLICATION_JSON))
		   .andDo(print())
		   .andExpect(status().isOk())
		   .andExpect(jsonPath("$.version").value(TEST_VERSION));
	}

}