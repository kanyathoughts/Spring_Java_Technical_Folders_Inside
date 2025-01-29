/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for ENTRY points
 */
public class Wmin519Test extends CobolCalculateControlFlowTest {
	
	@Test
	public void testExitProgramAndStopRun() {
		doTest("MIN519A.cbl");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin519", fileName);
	}
}
