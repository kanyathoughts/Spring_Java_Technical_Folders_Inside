/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for statements that call other programs
 */
public class Wmin517Test extends CobolCalculateControlFlowTest {
	
	@Test
	public void testCall() {
		doTest("MIN517A.cbl");
	}
	
	@Test
	public void testCicsLink() {
		doTest("MIN517B.cbl");
	}
	
	@Test
	public void testCicsReturn() {
		doTest("MIN517C.cbl");
	}
	
	@Test
	public void testCicsXctl() {
		doTest("MIN517D.cbl");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin517", fileName);
	}
}
