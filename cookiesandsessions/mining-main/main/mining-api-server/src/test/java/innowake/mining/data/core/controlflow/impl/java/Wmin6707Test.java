/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.java;

import org.junit.Test;

/**
 * Test to verify CFG for linking of Entry Super Type in simple Java program .
 */
public class Wmin6707Test extends JavaCalculateControlFlowTest {

	/**
	 * Test to verify CFGF of simple Java program containing main function and non static public functions.
	 */
	@Test
	public void testEntryType1() {
		doTest("JavaEntry1.java");
	}

	/**
	 * Test to verify CFG of simple Java program containing main function and static functions.
	 */
	@Test
	public void testEntryType2() {
		doTest("JavaEntry2.java");
	}

	/**
	 * Test to verify CFG of simple Java program containing main function, private and protected functions.
	 */
	@Test
	public void testEntryType3() {
		doTest("JavaEntry3.java");
	}

	/**
	 * Test to verify CFG of simple Java program containing main function with no body.
	 */
	@Test
	public void testEntryType4() {
		doTest("JavaEntry4.java");
	}

	private void doTest(final String fileName) {
		doTest("wmin6707", fileName);
	}
}
