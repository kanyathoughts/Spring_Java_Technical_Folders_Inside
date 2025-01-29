/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;
import innowake.ndt.cobol.parser.ast.statement.CobolCopyStmt;

/**
 * Tests CFG for source having {@link CobolCopyStmt}}.
 */
public class Wmin2070Test extends CobolCalculateControlFlowTest {

	/**
	 * Test CFG for source having single Copy Statement.
	 */
	@Test
	public void testCopyStatement1() {
		doTest("SingleCopy.cbl");
	}

	/**
	 * Test CFG for source having multiple Copy Statements.
	 */
	@Test
	public void testCopyStatements2() {
		doTest("MultipleCopies.cbl");
	}

	/**
	 * Test CFG for source having nested Copy Statement inside another Copy.
	 */
	@Test
	public void testCopyStatement3() {
		doTest("NestedCopy.cbl");
	}

	private void doTest(final String fileName) {
		doTest("wmin2070", fileName, fileName);
	}
}
