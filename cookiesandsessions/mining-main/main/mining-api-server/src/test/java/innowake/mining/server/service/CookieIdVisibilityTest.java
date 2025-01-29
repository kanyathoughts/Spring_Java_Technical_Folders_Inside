/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.Assert.assertFalse;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.TestPropertySource;

import innowake.mining.server.integration.DatabaseRelatedTest;

/**
 * Tests for {@link CookieIdVerifier} to test methods for cookieIdVerificationIsEnabled
 * to check whether CookieID verification is enable
 */
@TestPropertySource(properties="mining.cookieId=DISABLED")
class CookieIdVisibilityTest extends DatabaseRelatedTest {

	@Autowired
	private CookieIdVerifier cookieIdVerifier;

	/**
	 * Tests that properly checks that whether CookieID verification is enable
	 */
	@Test
	void testVisibilityOfCookieId() {
		assertFalse(cookieIdVerifier.cookieIdVerificationIsEnabled());
	}
}
