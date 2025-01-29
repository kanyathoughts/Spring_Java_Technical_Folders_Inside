/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import org.junit.Test;

/**
 * Base Implementation to check CGF generation for C program for the Entry.
 */
public class Wmin5194Test extends CCalculateControlFlowTest {

	/**
	 * The Test case checks if the a non static main function is of Type entry
	 */
	@Test
	public void testmainFunction() {
		doTest("mainFunction.c");
	}

	/**
	 * The Test case has many function checks how many are of Type entry
	 */
	@Test
	public void testmultipleFunction() {
		doTest("multipleFunction.c");
	}

	/**
	 * The Test case checks if the a non static function is of Type entry
	 */
	@Test
	public void testnonStaticFunction1() {
		doTest("nonStaticFunction1.c");
	}

	/**
	 * The Test case checks if the a non static main function is of Type entry and has a static variable
	 */
	@Test
	public void testnonStaticFunction2() {
		doTest("nonStaticFunction2.c");
	}

	/**
	 * The Test case checks if the a non static main function is of Type entry and has a register variable
	 */
	@Test
	public void testnonStaticFunction3() {
		doTest("nonStaticFunction3.c");
	}

	/**
	 * The Test case checks if the a static main function is of Type entry
	 */
	@Test
	public void teststaticFunction1() {
		doTest("staticFunction1.c");
	}

	/**
	 * The Test case checks if the a static main function is of Type entry and has a static variable
	 */
	@Test
	public void teststaticFunction2() {
		doTest("staticFunction2.c");
	}

	private void doTest(final String fileName) {
		doTest("wmin5194", fileName);
	}
}
