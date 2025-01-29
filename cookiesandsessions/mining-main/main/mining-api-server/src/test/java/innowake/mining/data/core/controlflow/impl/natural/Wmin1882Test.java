/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import org.junit.Test;

/**
 * Tests call statements.
 */
public class Wmin1882Test extends NaturalCalculateControlFlowTest {
	
	@Test
	public void testPerformSubroutine() {
		doTest("MIN1882A.nsp");
	}
	
	@Test
	public void testFetch() {
		doTest("MIN1882B.nsp");
	}	
	
	@Test
	public void testFetchReturn() {
		doTest("MIN1882C.nsp");
	}
	
	@Test
	public void testReturningCallStatements() {
		doTest("MIN1882D.nsp");
	}

	@Test
	public void testPerformSubroutineWithRecursiveCall() {
		/* due to the recursive call this tests that the control flow calculation does not end in an endless loop */
		doTest("MIN1882E.nsp");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin1882", fileName, fileName);
	}
}
