/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;
/**
 * FIXME: description
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.tags.AuthorizationTest;

/**
 * Tests verifying that the Keycloak configuration files do not require any authentication.
 */
@SpringBootTest(webEnvironment=WebEnvironment.RANDOM_PORT)
@ActiveProfiles(value = Profiles.AUTH_TEST, inheritProfiles = false )
@AuthorizationTest
public class KeycloakConfigFilesUnsecuredTest extends DatabaseRelatedTest {

	@MockBean
	@Nullable
	RestTemplate oauthRestTemplate;
	
	@Autowired
	private TestRestTemplate restTemplate;
	
	/**
	 * Makes sure that the Eclipse configuration is available without any authentication.
	 */
	@Test
	public void eclipseConfigIsAvailable() {
		final ParameterizedTypeReference<Map<String,Object>> typeReference = new ParameterizedTypeReference<Map<String, Object>>() {};
		final ResponseEntity<Map<String,Object>> response = restTemplate.exchange("/keycloak-eclipse.json", HttpMethod.GET, null, typeReference);
		assertEquals(HttpStatus.OK, response.getStatusCode()); 
		final Map<String, Object> result = response.getBody();
		assertNotNull(result);
		assertFalse("Result must not be empty", result.isEmpty());
	}

	/**
	 * Makes sure that the Web configuration is available without any authentication.
	 */
	@Test
	public void webConfigIsAvailable() {
		final ParameterizedTypeReference<Map<String,Object>> typeReference = new ParameterizedTypeReference<Map<String, Object>>() {};
		final ResponseEntity<Map<String,Object>> response = restTemplate.exchange("/keycloak-web.json", HttpMethod.GET, null, typeReference);
		assertEquals(HttpStatus.OK, response.getStatusCode()); 
		final Map<String, Object> result = response.getBody();
		assertNotNull(result);
		assertFalse("Result must not be empty", result.isEmpty());
	}
}
