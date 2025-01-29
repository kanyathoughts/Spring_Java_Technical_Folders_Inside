/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import org.junit.Test;

/**
 * Tests linking of IF ELSE statements.
 */
public class Wmin1856Test extends NaturalCalculateControlFlowTest {
	
	@Test
	public void testSimpleIfElse() {
		doTest("MIN1856A.nsp");
	}

	@Test
	public void testNestedIfElse() {
		doTest("MIN1856B.nsp");
	}
	
	@Test
	public void testIfSelection() {
		doTest("MIN1856C.nsp");
	}
	
	@Test
	public void testDecideOn() {
		doTest("MIN1856D.nsp");
	}
	
	@Test
	public void testDecideFor() {
		doTest("MIN1856E.nsp");
	}
	
	@Test
	public void testDecideForEvery() {
		doTest("MIN1856F.nsp");
	}
	
	@Test
	public void testDecideOnEvery() {
		doTest("MIN1856G.nsp");
	}
	
	@Test
	public void testDecideOnEveryWithNestedIf() {
		doTest("MIN1856H.nsp");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin1856", fileName, fileName);
	}
}
