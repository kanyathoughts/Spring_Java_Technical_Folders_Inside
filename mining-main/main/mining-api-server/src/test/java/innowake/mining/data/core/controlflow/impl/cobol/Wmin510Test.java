/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for PERFORM
 */
public class Wmin510Test extends CobolCalculateControlFlowTest {

	@Test
	public void testNestedPerform1() {
		doTest("MIN510A.cbl");
	}
	
	@Test
	public void testNestedPerform2() {
		doTest("MIN510B.cbl");
	}
	
	@Test
	public void testNestedPerformWithIf() {
		doTest("MIN510C.cbl");
	}
	
	@Test
	public void testNestedPerformWithIf2() {
		doTest("MIN510G.cbl");
	}
	
	@Test
	public void testNestedPerformLoop1() {
		doTest("MIN510D.cbl");
	}

	@Test
	public void testNestedPerformLoop2() {
		doTest("MIN510E.cbl");
	}

	@Test
	public void testInlinePerformLoop() {
		doTest("MIN510F.cbl");
	}
	
	@Test
	public void testNestedEndlessLoopPerform() {
		doTest("MIN510H.cbl");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin510", fileName);
	}
}
