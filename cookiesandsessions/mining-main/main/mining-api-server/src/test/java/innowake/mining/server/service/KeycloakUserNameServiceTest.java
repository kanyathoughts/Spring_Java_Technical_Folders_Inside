/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

import innowake.mining.server.integration.authorization.AbstractUserNameTest;
import innowake.mining.server.service.UserNameService.LookupResult;
import innowake.mining.shared.model.Member;

/**
 * Tests for the {@link KeycloakUserNameService}.
 */
public class KeycloakUserNameServiceTest extends AbstractUserNameTest {
	
	/**
	 * Test to validate user id is returned, if user is not found.
	 * This also tests if the service is gracefully handled if the mining-keycloak-extension is not updated with the endpoint to retrieve username information.
	 */
	@Test
	public void testLookUpUserNameInvalidId() {
		mockResponseExpectNotFound("4");
		final LookupResult lookedUpUser = userNameService.lookupUserName("4");
		assertEquals("4", lookedUpUser.getUserName());
	}
	
	/**
	 * Test to validate user name service with valid user id.
	 */
	@Test
	public void testLookUpUserNameValidId() {
		mockResponseExpectOk(1);
		final LookupResult lookedUpUser = userNameService.lookupUserName("1");
		verify(keycloakRestTemplate, times(1)).getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/users/1", Member.class);
		assertEquals("FName-1 LName-1", lookedUpUser.getUserName());
	}
	
	/**
	 * Test to validate user name service with system_user as user id.
	 */
	@Test
	public void testLookUpUserNameWithSystemUserAsId() {
		final String user = "system_user";
		mockResponseExpectOk(user);
		final LookupResult lookedUpUser = userNameService.lookupUserName(user);
		assertEquals("SYSTEM", lookedUpUser.getUserName());
	}
	
	/**
	 * Test to validate user name service where user belongs to different projects.
	 */
	@Test
	public void testLookUpUserNameDifferentProject() {
		final String user = "user1";
		mockResponseExpectForbidden(user);
		final LookupResult lookedUpUser = userNameService.lookupUserName(user);
		assertEquals(user, lookedUpUser.getUserName());
	}
	
	/**
	 * Test to validate user name service for multiple users lookup and also validates the cache implementation added to avoid redundant user information
	 * requests.
	 */
	@Test
	public void testLookUpUserNames() {		
		mockResponseExpectOk(3);
		
		final List<LookupResult> lookedUpUsers = userNameService.lookupUserNames(Arrays.asList("2", "1", "2"));
		assertEquals(3, lookedUpUsers.size());
		assertEquals("FName-2 LName-2", lookedUpUsers.get(0).getUserName());
		assertEquals("FName-1 LName-1", lookedUpUsers.get(1).getUserName());
		assertEquals("FName-2 LName-2", lookedUpUsers.get(2).getUserName());
		verify(keycloakRestTemplate, times(1)).getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/users/2", Member.class);
		verify(keycloakRestTemplate, times(1)).getForEntity(anyString(), eq(Member.class));
	}
}
