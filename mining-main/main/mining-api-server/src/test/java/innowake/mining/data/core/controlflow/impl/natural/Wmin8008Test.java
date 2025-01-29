/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import org.junit.Test;

/**
 * Tests CFG for Include Statement for Natural language
 */
public class Wmin8008Test extends NaturalCalculateControlFlowTest {

	/**
	 * Contains one include node
	 */
	@Test
	public void testIncludeStmt1() {
		doTest("MIN8008A.nsp");
	}

	/**
	 * Contains two include node
	 */
	@Test
	public void testIncludeStmt2() {
		doTest("MIN8008B.nsp");
	}

	private void doTest(final String fileName) {
		doTest("wmin8008", fileName, fileName);
	}
}
