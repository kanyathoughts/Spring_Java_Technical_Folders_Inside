package innowake.mining.server.controller;

import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.info.BuildProperties;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.integration.DatabaseRelatedTest;

/**
 * Tests verifying the behavior of {@link VersionController} in the default Spring profile.
 * <p>
 * This is not a mocked test, as the legacy auth seems to not easily be mockable, so we just went with a full-fledged integration test instead.
 */
@AutoConfigureMockMvc
class VersionControllerTests extends DatabaseRelatedTest {

	@Autowired
	private MockMvc mvc;
	
	@MockBean
	@Nullable
	private BuildProperties buildProperties;

	private static final String TEST_VERSION = "19.5.00";
	
	/**
	 * Setup the build properties mock.
	 */
	@BeforeEach
	public void init() {
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