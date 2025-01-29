/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import org.junit.Test;

/**
 * Contains tests to verify implementation for Function Call CFG in C 
 */
public class Wmin4796Test extends CCalculateControlFlowTest {

	/**
	 * Test to verify CFG for a simple function call
	 */
	@Test
	public void testCFunctionCall1() {
		doTest("CFunctionCall1.c");
	}

	/**
	 * Test to verify CFG for multiple simple function call
	 */
	@Test
	public void testCFunctionCall2() {
		doTest("CFunctionCall2.c");
	}

	/**
	 * Test to verify CFG for function as argument to another function 
	 */
	@Test
	public void testCFunctionCall3() {
		doTest("CFunctionCall3.c");
	}

	/**
	 * Test to verify CFG for multiple functions as argument to another function 
	 */
	@Test
	public void testCFunctionCall4() {
		doTest("CFunctionCall4.c");
	}

	/**
	 * Test to verify CFG for nested function calls
	 */
	@Test
	public void testCFunctionCall5() {
		doTest("CFunctionCall5.c");
	}

	/**
	 * Test to verify CFG for nested function calls variation
	 */
	@Test
	public void testCFunctionCall6() {
		doTest("CFunctionCall6.c");
	}

	private void doTest(final String fileName) {
		doTest("wmin4796", fileName);
	}

}
