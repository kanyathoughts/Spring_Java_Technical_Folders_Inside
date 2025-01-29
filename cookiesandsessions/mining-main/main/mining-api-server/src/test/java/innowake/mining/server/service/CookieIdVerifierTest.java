/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.springframework.test.context.TestPropertySource;

/**
 * Tests for {@link CookieIdVerifier} to test methods for cookieIdVerificationIsEnabled
 * and method to test verifyCookieId is working or not.
 */
@TestPropertySource(properties="mining.cookieId=")
class CookieIdVerifierTest {

	private static final String ARTIFACT_NAME = "mining-api-server-dist";
	
	/**
	 * Verifies CookieID is properly set
	 */
	@Test
	void testVerifyCookieId() {	
		final CookieIdVerifier cookieIdVerifier = new CookieIdVerifier();
		assertThrows(IllegalArgumentException.class, () ->
			cookieIdVerifier.verifyArtifactCookieId(ARTIFACT_NAME)
		);
	}
}