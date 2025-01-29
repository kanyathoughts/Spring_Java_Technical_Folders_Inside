/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.test;

import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Map;

import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;

import innowake.mining.client.service.Result;
import innowake.mining.client.service.info.Info;
import innowake.mining.client.service.info.InfoServiceProvider;

/**
 * Integration tests for the {@link Info} service.
 * 
 * @see InfoServiceProvider
 */
class InfoServiceTest extends IntegrationTest {

	private static final String[] EXPECTED_KEYS = {
			"api-version", "userId"
	};

	private final InfoServiceProvider InfoServiceProvider = new InfoServiceProvider(getConnectionInfo());

	@Test
	void testInfo() throws IOException {
		final Result<Map<String, String>> infoResult = InfoServiceProvider.info().execute();
		assertEquals(HttpStatus.SC_OK, infoResult.getStatusCode());
		assertTrue(infoResult.getValue().isPresent());

		final Map<String, String> map = infoResult.getValue().get();
		assertEquals(EXPECTED_KEYS.length, map.keySet().size());
		for (final String key : EXPECTED_KEYS) {
			assertTrue(map.containsKey(key));
		}
	}

}
