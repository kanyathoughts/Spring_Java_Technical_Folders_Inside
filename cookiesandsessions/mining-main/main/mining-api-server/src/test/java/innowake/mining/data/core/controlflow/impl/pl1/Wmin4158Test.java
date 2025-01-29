/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import org.junit.Test;

/**
 * Tests Pl1 CFG for Loop statements
 */
public class Wmin4158Test extends Pl1CalculateControlFlowTest {

	/**
	 * Test to verify CFG for Leave and Iterate statements
	 */
	@Test
	public void testLoopControlStatements() {
		doTest("Pl1DoStatementLoopControlStmt.pl1");
	}

	/**
	 * Test to verify CFG for Do Statements of type 2
	 */
	@Test
	public void testDoStatementsType2() {
		doTest("Pl1DoStatementType2.pl1");
	}

	/**
	 * Test to verify CFG for Do Statements of type 3
	 */
	@Test
	public void testDoStatementsType3() {
		doTest("Pl1DoStatementType3.pl1");
	}

	/**
	 * Test to verify CFG for Do Statements of type 4
	 */
	@Test
	public void testDoStatementsType4() {
		doTest("Pl1DoStatementType4.pl1");
	}

	private void doTest(final String fileName) {
		doTest("wmin4158", fileName);
	}

}
