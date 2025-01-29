/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.discovery.dna;

import java.nio.file.Paths;

import org.junit.jupiter.api.Test;
import org.springframework.test.context.TestPropertySource;

/**
 * Tests the DNA optimal persistance. 
 */
@TestPropertySource(properties="configuration.discovery-dna-optimal-persist=true")
class DiscoveryDnaOptimalPersistTest extends DiscoverDnaJobTest {

	@Override
	protected String getTestFolder() {
		return Paths.get("DNA/optimal_persist").toString();
	}

	/**
	 * Tests the new discovery DNA run.
	 */
	@Test
	void testDiscoveryDnaForNew() {
		assertCount(1, 8, 204, 1, 2); /* In case if the optimal persist is turned off, the DNA similarity count would be 12 */
	}

}
