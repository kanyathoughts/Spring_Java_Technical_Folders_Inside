/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import org.junit.Test;

/**
 * Contains tests to verify implementation for Goto Statements CFG in C
 */
public class Wmin4797Test extends CCalculateControlFlowTest {
	
	/**
	 * Test to verify CFG for a C goto statement present inside if else block.
	 */
	@Test
	public void testCGoToStatement1() {
		doTest("CGoToStmt1.c");
	}
	
	/**
	 * Test to verify CFG for a C goto statement not having target present.
	 */
	@Test
	public void testCGoToStatement2() {
		doTest("CGoToStmt2.c");
	}
	
	/**
	 * Test to verify CFG for a C goto statement having a function call as target
	 */
	@Test
	public void testCGoToStatement3() {
		doTest("CGoToStmt3.c");
	}
	
	/**
	 * Test to verify CFG for a C goto statement having a block of code as target
	 */
	@Test
	public void testCGoToStatement4() {
		doTest("CGoToStmt4.c");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin4797", fileName);
	}
}
