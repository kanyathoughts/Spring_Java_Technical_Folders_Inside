/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import org.junit.Test;

/**
 * Contains test cases for PL/I specific handling of procedure blocks.
 */
public class Wmin6142Test extends Pl1CalculateControlFlowTest {
	
	/**
	 * Test to verify CFG for procedure block in PL/I
	 */
	@Test
	public void testPl1ProcedureBlock() {
		doTest("Pl1ProcedureBlock.pl1");
	}
	
	/**
	 * Test to verify CFG for procedure block containing in line nested procedure block in PL/I
	 */
	@Test
	public void testPl1InlineProcedureBlock() {
		doTest("Pl1InlineProcedureBlockpl1");
	}
	
	/**
	 * Test to verify CFG for procedure block containing in Begin block in PL/I
	 */
	@Test
	public void testPl1ProcedureContainingBeginBlock() {
		doTest("Pl1ProcedureWithBeginBlock.pl1");
	}
	
	/**
	 * Test to verify CFG for procedure block with return statement in PL/I
	 */
	@Test
	public void testPl1ProcedureBlockWithReturn() {
		doTest("Pl1ProcedureBlockWithReturn.pl1");
	}

	/**
	 * Test to verify CFG for procedure block with call statement in PL/I
	 */
	@Test
	public void testPl1CallStatement() {
		doTest("Pl1CallStmnt.pl1");
	}
	
	/**
	 * Test to verify CFG for procedure block with GoTo statement in PL/I
	 */
	@Test
	public void testPl1GotoStatement() {
		doTest("Pl1GotoStmnt.pl1");
	}

	private void doTest(final String fileName) {
		doTest("wmin6142", fileName);
	}
}
