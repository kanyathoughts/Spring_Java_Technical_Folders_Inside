/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server;

import static org.junit.Assert.assertTrue;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import innowake.mining.server.integration.DatabaseRelatedTest;

/**
 * Run the test-data-complete script.
 */
@Disabled
class DatabaseCompleteTest extends DatabaseRelatedTest {

	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}
	
	@Test
	void dummy() {
		assertTrue(true);
	}

}
