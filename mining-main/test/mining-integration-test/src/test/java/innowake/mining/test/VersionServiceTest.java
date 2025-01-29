/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.test;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.text.IsBlankString.blankString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.Map;
import java.util.Set;

import org.apache.http.HttpStatus;
import org.junit.jupiter.api.Test;

import innowake.mining.client.service.Result;
import innowake.mining.client.service.version.Version;
import innowake.mining.client.service.version.VersionServiceProvider;

/**
 * Integration tests for the {@link Version} service.
 * 
 * @see VersionServiceProvider
 */
class VersionServiceTest extends IntegrationTest {

	private static final String[] EXPECTED_KEYS = { "version" };

	private final VersionServiceProvider versionServiceProvider = new VersionServiceProvider(getConnectionInfo());

	@Test
	void testVersion() throws IOException {
		final Result<Map<String, String>> infoResult = versionServiceProvider.version().execute();
		assertEquals(HttpStatus.SC_OK, infoResult.getStatusCode());
		assertTrue(infoResult.getValue().isPresent());

		final Map<String, String> map = infoResult.getValue().get();
		final Set<String> keySet = map.keySet();
		assertEquals(EXPECTED_KEYS.length, keySet.size());
		for (final String key : EXPECTED_KEYS) {
			assertThat(keySet, hasItem(key));
			assertThat(map.get(key), is(not(blankString())));
		}
	}

}
