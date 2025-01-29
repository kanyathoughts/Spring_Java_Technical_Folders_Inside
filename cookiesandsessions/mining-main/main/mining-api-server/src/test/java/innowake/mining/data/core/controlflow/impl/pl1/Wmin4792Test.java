/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import org.junit.Test;

/**
 * Base Implementation to check CGF generation for PL/I program with Return statements.
 */
public class Wmin4792Test extends Pl1CalculateControlFlowTest {

	/**
	 * Test to verify Return Statement
	 */
	@Test
	public void testPl1ReturnStmt1() {
		doTest("Pl1ReturnStatement.pl1");
	}
	
	/**
	 * Test to verify Return Statement within IF ELSE BLOCK
	 */
	@Test
	public void testPl1ReturnStmt2() {
		doTest("Pl1ReturnStatement2.pl1");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin4792", fileName);
	}
}
