/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import org.junit.Test;

/**
 * Tests halt statements.
 */
public class Wmin4160Test extends Pl1CalculateControlFlowTest {

	/**
	 * Test to verify CFG when end statement belongs to main procedure block.
	 */
	@Test
	public void testHaltStatementsA() {
		doTest("Pl1MainProcedureEndStatement.pl1");
	}

	/**
	 * Test to verify halt statements CFG in case of stop statement.
	 */
	@Test
	public void testHaltStatementsB() {
		doTest("Pl1StopStatement.pl1");
	}

	/**
	 * Test to verify halt statements CFG in case of exit statements.
	 */
	@Test
	public void testHaltStatementsC() {
		doTest("Pl1ExitStatements.pl1");
	}

	private void doTest(final String fileName) {
		doTest("wmin4160", fileName);
	}

}
