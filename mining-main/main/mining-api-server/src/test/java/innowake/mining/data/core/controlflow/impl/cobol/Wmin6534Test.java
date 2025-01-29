/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

/**
 * Test for verifying CFG of GOTO Statements with single and Multiple Targets.
 */
public class Wmin6534Test extends CobolCalculateControlFlowTest {
	
	/**
	 * Tests a GOTO statement with single target.
	 */
	@Test
	void testGoTowithSingleTarget() {
		final String errorMsg = "Target could not be resolved.";
		final Throwable exception = assertThrows(IllegalArgumentException.class, () -> {
			doTest("MIN6534A.cbl");
		});
		assertEquals(errorMsg, exception.getMessage());
	}
	
	/**
	 * Tests a GOTO statement with multiple targets.
	 */
	@Test
	void testGoTowithMultipleTarget() {
		doTest("MIN6534B.cbl");
	}

	private void doTest(final String fileName) {
		doTest("wmin6534", fileName);
	}
}
