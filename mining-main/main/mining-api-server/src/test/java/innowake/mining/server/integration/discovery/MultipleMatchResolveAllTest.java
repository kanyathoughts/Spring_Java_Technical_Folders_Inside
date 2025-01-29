/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import innowake.mining.server.discovery.dawn.metrics.test.contributors.multiplematch.MultipleMatchResolveAllContributorsConfiguration;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.TestPropertySource;

import java.io.IOException;

/**
 * This test validates that dependencies declared with {@code MULTIPLE_MATCH_RESOLVE_ALL} flag are resolved exactly once to each valid target,
 * even if new targets are added by a deferred action.
 */
@TestPropertySource(properties ="configuration.discovery-enable-dawn-contributors=false") /* disable auto-registration of Dawn contributors ... */
@TestPropertySource(properties ="configuration.discovery-enable-legacy-contributors=false") /* disable execution of legacy contributors ... */
@Import(MultipleMatchResolveAllContributorsConfiguration.class) /* ... and only load the test contributors instead */
@WithMockUser
public class MultipleMatchResolveAllTest extends BaseDiscoveryTest {

	private static final String TEST_FOLDER = "resolve-all";

	public MultipleMatchResolveAllTest() {
		/* our test contributor will not match the Discovery feature matrix, so feature validation must be skipped */
		super(true);
	}

	@Test
	void testMultipleMatchResolveAll() throws IOException, InvalidFormatException {
		execute();
	}

	@Override
	protected String getTestFolder() {
		return TEST_FOLDER;
	}
}
