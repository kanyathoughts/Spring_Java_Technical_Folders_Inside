/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import org.junit.Test;

/**
 * Test case to check the CGF generation for C program Halt statement.
 */
public class Wmin4794Test extends CCalculateControlFlowTest {

	/**
	 * Abort statement with the  Library stdlib included
	 */
	@Test
	public void testCAbort() {
		doTest("wmin4794", "CAbort.c");
	}

	/**
	 * Abort statement without the  Library stdlib included
	 */
	@Test
	public void testCAbortWithoutLib() {
		doTest("wmin4794", "CAbortWithoutLib.c");
	}

	/**
	 * Exit Library function with if
	 */
	@Test
	public void testCExit1() {
		doTest("wmin4794", "CExit1.c");
	}

	/**
	 * _Exit library function
	 */
	@Test
	public void testCExit2() {
		doTest("wmin4794", "CExit2.c");
	}

	/**
	 * Exit Library function without if
	 */
	@Test
	public void testCExit3() {
		doTest("wmin4794", "CExit3.c");
	}

	/**
	 * _Exit Library function without if
	 */
	@Test
	public void testCExit4() {
		doTest("wmin4794", "CExit4.c");
	}

	/**
	 * Mix of all the halt function together
	 */
	@Test
	public void testCExit5() {
		doTest("wmin4794", "CExit5.c");
	}

	/**
	 * Mix of all the halt function together without defining the library
	 */
	@Test
	public void testCExitWithoutLib1() {
		doTest("wmin4794", "CExitWithoutLib1.c");
	}

	/**
	 * Exit function without defining the library
	 */
	@Test
	public void testCExitWithoutLib2() {
		doTest("wmin4794", "CExitWithoutLib2.c");
	}

	/**
	 * Return from a method
	 */
	@Test
	public void testCReturn1() {
		doTest("wmin4794", "CReturn1.c");
	}

	/**
	 * Return from a method with if
	 */
	@Test
	public void testCReturn2() {
		doTest("wmin4794", "CReturn2.c");
	}

	/**
	 * Return from a method with if and a printf statement
	 */
	@Test
	public void testCReturn3() {
		doTest("wmin4794", "CReturn3.c");
	}
}
