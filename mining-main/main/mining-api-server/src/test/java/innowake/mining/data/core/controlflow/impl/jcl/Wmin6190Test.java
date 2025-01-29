/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import org.junit.Test;

/**
 * Contains test cases to verify implementation for CallExternalStatement in JCL
 */
public class Wmin6190Test extends JclCalculateControlFlowTest {

	/**
	 * This test contains StepExec
	 */
	@Test
	public void testCallExternalStatementCfg1() {
		doTest("WMIN6190A.job");
	}

	/**
	 * This test contains multiple StepExec
	 */
	@Test
	public void testCallExternalStatementCfg2() {
		doTest("WMIN6190B.job");
	}

	/**
	 * This test contains StepExec inside StepIf
	 */
	@Test
	public void testCallExternalStatementCfg3() {
		doTest("WMIN6190C.job");
	}

	private void doTest(final String fileName) {
		doTest("wmin6190", fileName);
	}
}
