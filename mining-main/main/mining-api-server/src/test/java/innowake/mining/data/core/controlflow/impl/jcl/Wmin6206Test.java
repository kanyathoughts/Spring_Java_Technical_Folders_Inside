/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import org.junit.Test;

/**
 * Contains test cases to check Labels for JCL program.
 */
public class Wmin6206Test extends JclCalculateControlFlowTest {

	/**
	 * This test contains JCL StepIf for the JCL language.
	 */
	@Test
	public void testLabelProvider1() {
		doTest("WMIN6206A.job");
	}

	/**
	 * This test contains JCL StepIf with on If node for the JCL language.
	 */
	@Test
	public void testLabelProvider2() {
		doTest("WMIN6206B.job");
	}

	/**
	 * This test contains JCL StepExec after JCL StepIf with on If node for the JCL language.
	 */
	@Test
	public void testLabelProvider3() {
		doTest("WMIN6206C.job");
	}

	private void doTest(final String fileName) {
		doTest("wmin6206", fileName);
	}
}
