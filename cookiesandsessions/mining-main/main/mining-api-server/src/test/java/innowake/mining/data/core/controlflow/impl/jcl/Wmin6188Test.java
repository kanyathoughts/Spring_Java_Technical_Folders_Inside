/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import org.junit.Test;

/**
 * This class contains test cases for verifying CFG of JCL Branch Statement.
 */
public class Wmin6188Test extends JclCalculateControlFlowTest {

	/**
	 * This test contains JCL StepIf Statement with no else branch.
	 */
	@Test
	public void testBranchStatement1() {
		doTest("WMIN6188B.job");
	}

	/**
	 * This test contains JCL StepIf Statement with both then and else branch.
	 */
	@Test
	public void testBranchStatement2() {
		doTest("WMIN6188C.job");
	}

	/**
	 * This test contains JCL StepIf Statement with nested StepIf statements in the then branch.
	 */
	@Test
	public void testBranchStatement3() {
		doTest("WMIN6188D.job");
	}

	/**
	 * This test contains JCL StepIf Statement with nested StepIf statements in the else branch.
	 */
	@Test
	public void testBranchStatement4() {
		doTest("WMIN6188E.job");
	}

	/**
	 * This test contains multiple JCL StepIf statements.
	 */
	@Test
	public void testBranchStatement5() {
		doTest("WMIN6188A.job");
	}

	private void doTest(final String fileName) {
		doTest("wmin6188", fileName);
	}
}
