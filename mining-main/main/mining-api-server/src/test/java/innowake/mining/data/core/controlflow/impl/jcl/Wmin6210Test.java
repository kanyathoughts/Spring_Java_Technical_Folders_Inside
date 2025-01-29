/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import org.junit.Test;

/**
 * Tests CFG for COND in Job and Step Exec in JCL
 */
public class Wmin6210Test extends JclCalculateControlFlowTest {

	/**
	 * Contains JCL StepExec with condition.
	 */
	@Test
	public void testJclCondition1() {
		doTest("WMIN6210A.job");
	}

	/**
	 * Contains JCL Job with condition.
	 */
	@Test
	public void testJclCondition2() {
		doTest("WMIN6210B.job");
	}

	/**
	 * Contains JCL Job and StepExec with condition.
	 */
	@Test
	public void testJclCondition3() {
		doTest("WMIN6210C.job");
	}

	private void doTest(final String fileName) {
		doTest("wmin6210", fileName);
	}
}
