/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for GOBACK, STOP RUN and EXIT PROGRAM.
 */
public class Wmin520Test extends CobolCalculateControlFlowTest {
	
	@Test
	public void testExitProgramAndStopRun() {
		doTest("MIN520A.cbl");
	}
	
	@Test
	public void testImplicitGoBack() {
		doTest("MIN520B.cbl");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin520", fileName);
	}
}
