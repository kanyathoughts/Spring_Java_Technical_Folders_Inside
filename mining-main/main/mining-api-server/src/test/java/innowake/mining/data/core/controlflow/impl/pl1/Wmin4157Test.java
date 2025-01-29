/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import org.junit.Test;

/**
 * Base Implementation to check CGF generation for PL/I program with simple statements.
 */
public class Wmin4157Test extends Pl1CalculateControlFlowTest {

	/**
	 * Test to verify CFG for CALL Statements PL/I Statements within IF Block
	 */
	@Test
	public void testPl1CallStmt1() {
		doTest("Pl1CallStatement1.pl1");
	}
	
	/**
	 * Test to verify CFG for CALL Statements PL/I Statements 
	 */
	@Test
	public void testPl1CallStmt2() {
		doTest("Pl1CallStatement2.pl1");
	}
	
	/**
	 * Test to verify CFG for CALL Statements PL/I Statements within target Procedure outside the call statement Procedure.
	 */
	@Test
	public void testPl1CallStmt3() {
		doTest("Pl1CallStatements3.pl1");
	}

	private void doTest(final String fileName) {
		doTest("wmin4157", fileName);
	}
}
