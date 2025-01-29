/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

import innowake.mining.server.service.NoAuthUserNameService;
import innowake.mining.server.service.UserNameService;
import innowake.mining.server.service.UserNameService.LookupResult;

/**
 * Tests for the {@link NoAuthUserNameService}.
 */
class UserNameServiceNoAuthTest {
	
	private UserNameService userNameService = new NoAuthUserNameService();
	
	/**
	 * Test to validate user id is returned, in no auth profile.
	 */
	@Test
	void testLookUpUserName() {
		final LookupResult lookedUpUser = userNameService.lookupUserName("4");
		assertEquals("4", lookedUpUser.getUserName());
		assertEquals(Boolean.FALSE, Boolean.valueOf(lookedUpUser.isMissing()));
		assertEquals(Boolean.FALSE, Boolean.valueOf(lookedUpUser.isDeleted()));
	}
	
	
	/**
	 * Test to validate user name service for multiple users lookup in no auth profile.
	 */
	@Test
	void testLookUpUserNames() {
		final List<LookupResult> lookedUpUsers = userNameService.lookupUserNames(Arrays.asList("1", "2", "1"));
		assertEquals(3, lookedUpUsers.size());
		assertEquals("1", lookedUpUsers.get(0).getUserName());
		assertEquals("2", lookedUpUsers.get(1).getUserName());
		assertEquals("1", lookedUpUsers.get(2).getUserName());
	}
	
}
