/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for GO TO
 */
public class Wmin516Test extends CobolCalculateControlFlowTest {

	@Test
	public void testGoToNestedInPerfom() {
		doTest("MIN516A.cbl");
	}
	
	@Test
	public void testGoToNestedInPerfom2() {
		doTest("MIN516B.cbl");
	}
	
	@Test
	public void testGoToEndlessLoopWithDeadCode() {
		doTest("MIN516C-DO-NOT-RUN-THIS-ON-MAINFRAME.cbl");
	}
	
	@Test
	public void testGoToDependingOn() {
		doTest("MIN516D.cbl");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin516", fileName);
	}
}
