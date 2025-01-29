/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration;

import java.io.IOException;

import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.shared.access.SourceCachingService;

/**
 * Base class for integration tests needing to reset the test data after every test-run.
 */
public abstract class DatabaseResettingTest extends DatabaseRelatedTest {

	@Autowired
	private SourceCachingService sourceService;

	/**
	 * Reset the test data before each test method.
	 * @throws IOException If an IO related error occurs.
	 */
	@BeforeEach
	public void resetData() throws IOException {
		resetTestData();
	}
	
	@Override
	protected void resetTestData() throws IOException {
		super.resetTestData();
		/* need to call the cache reset also before each test. DatabaseRelatedTest already does that */
		sourceService.resetCaches();
	}
}
