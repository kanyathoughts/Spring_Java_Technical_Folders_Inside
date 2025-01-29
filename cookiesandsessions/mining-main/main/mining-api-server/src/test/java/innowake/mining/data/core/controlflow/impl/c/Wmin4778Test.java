/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import org.junit.Test;

/**
 * Base Implementation to check CGF generation for C program with simple statements.
 */
public class Wmin4778Test extends CCalculateControlFlowTest {

	/**
	 * This test contains all the valid statement type for the C language.
	 */
	@Test
	public void testAllStatement() {
		doTest("wmin4778a.c");
	}
	
	/**
	 * This test contains only the expression statement type for the C language.
	 */
	@Test
	public void testExpressionStatement() {
		doTest("wmin4778b.c");
	}

	private void doTest(final String fileName) {
		doTest("wmin4778", fileName);
	}
}
