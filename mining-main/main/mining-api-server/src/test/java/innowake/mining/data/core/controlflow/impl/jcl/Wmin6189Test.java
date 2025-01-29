/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import org.junit.Test;

/**
 * Contains test cases to check CFG generation for JCL
 */
public class Wmin6189Test extends JclCalculateControlFlowTest {

	/**
	 * This test contains JCL Job with StepExec
	 */
	@Test
	public void testSimpleCfg1() {
		doTest("WMIN6189A.job");
	}

	/**
	 * This test contains two JCL Jobs 
	 */
	@Test
	public void testSimpleCfg2() {
		doTest("WMIN6189B.job");
	}

	/**
	 * This test contains JCL Job with StepIf
	 */
	@Test
	public void testSimpleCfg3() {
		doTest("WMIN6189C.job");
	}

	private void doTest(final String fileName) {
		doTest("wmin6189", fileName);
	}
}
