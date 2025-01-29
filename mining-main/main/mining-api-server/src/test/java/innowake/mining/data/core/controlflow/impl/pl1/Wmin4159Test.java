/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import org.junit.Test;

/**
 * Contains test cases to verify Base Implementation to check CGF generation for PL/I program with GOTO statements.
 */
public class Wmin4159Test extends Pl1CalculateControlFlowTest {
	
	/**
	 * Test to verify CFG for GoTo Statements in Pl/I
	 */
	@Test
	public void testPl1GoToStmt1() {
		doTest("Pl1GotoStatement1.pl1");
	}

	/**
	 * Test to verify CFG for GoTo Statements within If eLse block
	 */
	@Test
	public void testPl1GoToStmt2() {
		doTest("Pl1GotoStatement2.pl1");
	}

	/**
	 * Test to verify CFG for GoTo Statements with no target label
	 */
	@Test
	public void testPl1GoToStmt3() {
		doTest("Pl1GotoStatement3.pl1");
	}

	/**
	 * Test to verify CFG for GoTo Statements with target label in a procedure outside the main procedure
	 */
	@Test
	public void testPl1GoToStmt4() {
		doTest("Pl1GotoStatement4.pl1");
	}
	
	/**
	 * Test to verify CFG for GoTo Statements with target statement containing function calls.
	 */
	@Test
	public void testPl1GoToStmt5() {
		doTest("Pl1GotoStatement5.pl1");
	}

	private void doTest(final String fileName) {
		doTest("wmin4159", fileName);
	}
}
