/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.BDDMockito.given;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import innowake.mining.server.cache.MiningCacheConfig;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.server.service.KeycloakUserNameService;
import innowake.mining.shared.model.Member;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.ProjectRole;
import innowake.mining.shared.model.UserRole;

/**
 * Abstract class for accessing the keycloak-extension end points for {@code Member} info access.
 */
@AutoConfigureMockMvc
public abstract class AbstractUserNameTest extends AuthorizationTests {
	
	@MockBean
	@Nullable
	private KeycloakApplicationConfiguration config;

	@Autowired
	protected KeycloakUserNameService userNameService;
	@Autowired
	private CacheManager cacheManager;
	
	protected static final String MOCK_HOST = "mock-host";
	protected static final String MOCK_REALM = "mock-realm";

	@BeforeAll
	void beforeAll() {
		Optional.ofNullable(cacheManager.getCache(MiningCacheConfig.KEYCLOAK_USER_NAME_LOOKUP)).ifPresent(Cache::invalidate);
	}
	
	@BeforeEach
	void init() {
		assertNotNull(keycloakRestTemplate);
		final KeycloakApplicationConfiguration config2 = config;
		if (config2 != null) {
			given(config2.getAuthServerUrl()).willReturn(MOCK_HOST);
			given(config2.getRealm()).willReturn(MOCK_REALM);
			userNameService.createKeycloakURI();
		} else {
			fail("Keycloak was not initialized properly.");
		}
	}
	
	/**
	 * Test to validate that the beans have been autowired/injected appropriately.
	 */
	@Test
	void testAutowiredNotNull() {
		assertNotNull(config);
		assertNotNull(keycloakRestTemplate);
		assertNotNull(assertNotNull(config).getAuthServerUrl());
	}
	
	/**
	 * Create a mock response for the given number of users with {@code HttpStatus#OK}.
	 * User ids mocked starts from 1 and goes until {@code numberOfUsers}.
	 *
	 * @param numberOfUsers number of users for which mock response is made
	 */
	protected void mockResponseExpectOk(final int numberOfUsers) {
		final List<ProjectNature> projectNatures = Collections.singletonList(ProjectNature.MINING);
		final List<ProjectRole> projectRoles = Collections.singletonList(new ProjectRole(Long.valueOf(3), UserRole.EDITOR, projectNatures));
		for (int i = 1; i <= numberOfUsers; i++) {
			final Member member = new Member(String.valueOf(i), "FName-" + i, "LName-" + i, "email" + i + "@email.com", projectRoles);
			final ResponseEntity<Member> mockedResponse = new ResponseEntity<>(member, HttpStatus.OK);
			given(assertNotNull(keycloakRestTemplate)
					.getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/users/" + i, Member.class))
					.willReturn(mockedResponse);
		}
	}

	/**
	 * Create a mock response returning {@code HttpStatus#OK}.
	 *
	 * @param userId id for which mock response is made
	 */
	protected void mockResponseExpectOk(final String userId) {
		final ResponseEntity<Member> mockedResponse = new ResponseEntity<>(HttpStatus.OK);
		given(assertNotNull(keycloakRestTemplate)
				.getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/users/" + userId, Member.class))
				.willReturn(mockedResponse);
	}

	/**
	 * Create a mock response returning {@code HttpStatus#FORBIDDEN}.
	 *
	 * @param userId id for which mock response is made
	 */
	protected void mockResponseExpectForbidden(final String userId) {
		final ResponseEntity<Member> mockedResponse = new ResponseEntity<>(HttpStatus.FORBIDDEN);
		given(assertNotNull(keycloakRestTemplate)
				.getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/users/" + userId, Member.class))
				.willReturn(mockedResponse);
	}

	/**
	 * Create a mock response returning {@code HttpStatus#NOT_FOUND}.
	 *
	 * @param userId id for which mock response is made
	 */
	protected void mockResponseExpectNotFound(final String userId) {
		final ResponseEntity<Member> mockedResponse = new ResponseEntity<>(HttpStatus.NOT_FOUND);
		given(assertNotNull(keycloakRestTemplate)
				.getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/users/" + userId, Member.class))
				.willReturn(mockedResponse);
	}
}
