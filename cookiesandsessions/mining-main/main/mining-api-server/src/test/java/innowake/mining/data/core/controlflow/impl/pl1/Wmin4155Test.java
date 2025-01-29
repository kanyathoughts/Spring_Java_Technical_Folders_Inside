/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import org.junit.Test;

/**
 * Base Implementation to check CGF generation for PL/I program with simple statements.
 */
public class Wmin4155Test extends Pl1CalculateControlFlowTest {

	/**
	 * Test to check verify CFG base facility incase of simple PL/1 program ( without branch/loops)
	 */
	@Test
	public void testSimpleStatementLinking() {
		doTest("Wmin3801.pl1");
	}

	private void doTest(final String fileName) {
		doTest("wmin3801", fileName);
	}

}
