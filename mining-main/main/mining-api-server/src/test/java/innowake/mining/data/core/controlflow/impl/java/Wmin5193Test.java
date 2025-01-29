/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.java;

import org.junit.Test;

/**
 * Base Implementation to check CFG generation for simple Java program.
 */
public class Wmin5193Test extends JavaCalculateControlFlowTest {

	/**
	 * Test to verify CFG base facility of simple Java program ( without branch/loops)
	 */
	@Test
	public void testSimpleStatementLinking() {
		doTest("WMIN5193A.java");
	}
	
	/**
	 * Test to verify CFG base facility of simple Java program containing declaration statement
	 */
	@Test
	public void testSimpleStatementLinking2() {
		doTest("WMIN5193B.java");
	}

	/**
	 * Test to verify CFG base facility of simple Java program containing user defined method call 
	 */
	@Test
	public void testSimpleStatementLinking3() {
		doTest("WMIN5193C.java");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin5193", fileName);
	}
}
