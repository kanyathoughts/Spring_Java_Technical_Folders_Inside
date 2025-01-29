/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import org.junit.Test;

/**
 * Base Implementation to check CGF generation for C program with simple statements.
 */
public class Wmin4791Test extends CCalculateControlFlowTest {

	/**
	 * This test contains do while with break and continue together C language.
	 */
	@Test
	public void testCDoWhileBreakContinueStatement1() {
		doTest("wmin4791", "CDoWhileBreakContinueStatement1.c");
	}

	/**
	 * This test contains do while with break for C language.
	 */
	@Test
	public void testCDoWhileBreakStatement1() {
		doTest("wmin4791", "CDoWhileBreakStatement1.c");
	}

	/**
	 * This test contains do while with break and a printf at last for C language.
	 */
	@Test
	public void testCDoWhileBreakStatement2() {
		doTest("wmin4791", "CDoWhileBreakStatement2.c");
	}

	/**
	 * This test contains do while continue for C language.
	 */
	@Test
	public void testCDoWhileContinueStatement1() {
		doTest("wmin4791", "CDoWhileContinueStatement1.c");
	}

	/**
	 * This test contains do while for C language.
	 */
	@Test
	public void testCDoWhileStatement1() {
		doTest("wmin4791", "CDoWhileStatement1.c");
	}

	/**
	 * This test contains do while and printf at last for C language.
	 */
	@Test
	public void testCDoWhileStatement2() {
		doTest("wmin4791", "CDoWhileStatement2.c");
	}

	/**
	 * This test contains 2 do while for C language.
	 */
	@Test
	public void testCDoWhileStatement3() {
		doTest("wmin4791", "CDoWhileStatement3.c");
	}

	/**
	 * This test contains 2 do while and printf at last for C language.
	 */
	@Test
	public void testCDoWhileStatement4() {
		doTest("wmin4791", "CDoWhileStatement4.c");
	}

	/**
	 * This test contains 2 do while and both contains printf at last for C language.
	 */
	@Test
	public void testCDoWhileStatement5() {
		doTest("wmin4791", "CDoWhileStatement5.c");
	}

	/**
	 * This test contains for statement with break and continue together for C language.
	 */
	@Test
	public void testCForBreakContinueStatement1() {
		doTest("wmin4791", "CForBreakContinueStatement1.c");
	}

	/**
	 * This test contains For statement with break inside If statement for the C language.
	 */
	@Test
	public void testCForBreakStatement1() {
		doTest("wmin4791", "CForBreakStatement1.c");
	}

	/**
	 * This test contains 2 For statement with break inside If statement for the C language.
	 */
	@Test
	public void testCForBreakStatement2() {
		doTest("wmin4791", "CForBreakStatement2.c");
	}

	/**
	 * This test contains For statement with break and a printf for the C language.
	 */
	@Test
	public void testCForBreakStatement3() {
		doTest("wmin4791", "CForBreakStatement3.c");
	}

	/**
	 * This test contains 2 For statement with break and 2 printf for the C language.
	 */
	@Test
	public void testCForBreakStatement4() {
		doTest("wmin4791", "CForBreakStatement4.c");
	}

	/**
	 * This test contains For statement with continue for the C language.
	 */
	@Test
	public void CForContinueStatement1() {
		doTest("wmin4791", "CForContinueStatement1.c");
	}

	/**
	 * This test contains for statement for C language.
	 */
	@Test
	public void testCForStatement1() {
		doTest("wmin4791", "CForStatement1.c");
	}

	/**
	 * This test contains for statement with printf for C language.
	 */
	@Test
	public void testCForStatement2() {
		doTest("wmin4791", "CForStatement2.c");
	}

	/**
	 * This test contains 2 for statement for C language.
	 */
	@Test
	public void testCForStatement3() {
		doTest("wmin4791", "CForStatement3.c");
	}

	/**
	 * This test contains 2 for statement with printf for C language.
	 */
	@Test
	public void testCForStatement4() {
		doTest("wmin4791", "CForStatement4.c");
	}

	/**
	 * This test contains while with break and continue for C language.
	 */
	@Test
	public void testCWhileBreakContinueStatement1() {
		doTest("wmin4791", "CWhileBreakContinueStatement1.c");
	}

	/**
	 * This test contains while with break with printf for C language.
	 */
	@Test
	public void testCWhileBreakStatement1() {
		doTest("wmin4791", "CWhileBreakStatement1.c");
	}

	/**
	 * This test contains while with break for C language.
	 */
	@Test
	public void testCWhileBreakStatement2() {
		doTest("wmin4791", "CWhileBreakStatement2.c");
	}

	/**
	 * This test contains 2 while with break with printf for C language.
	 */
	@Test
	public void testCWhileBreakStatement3() {
		doTest("wmin4791", "CWhileBreakStatement3.c");
	}

	/**
	 * This test contains while with continue with for C language.
	 */
	@Test
	public void testCWhileContinueStatement1() {
		doTest("wmin4791", "CWhileContinueStatement1.c");
	}

	/**
	 * This test contains for statement with continue for the C language.
	 */
	@Test
	public void testCForContinueStatement1() {
		doTest("wmin4791", "CForContinueStatement1.c");
	}

	/**
	 * This test will check the single while statement for the C language.
	 */
	@Test
	public void testCWhileStatement1() {
		doTest("wmin4791", "CWhileStatement1.c");
	}

	/**
	 * This test will check the single while statement with printf for the C language.
	 */
	@Test
	public void testCWhileStatement2() {
		doTest("wmin4791", "CWhileStatement2.c");
	}

	/**
	 * This test will check the 2 while statement for the C language.
	 */
	@Test
	public void testCWhileStatement3() {
		doTest("wmin4791", "CWhileStatement3.c");
	}

	/**
	 * This test will check the 2 while statement withs 1 printf for the C language.
	 */
	@Test
	public void testCWhileStatement4() {
		doTest("wmin4791", "CWhileStatement4.c");
	}

	/**
	 * This test will check the 2 while statement with 2 printf for the C language.
	 */
	@Test
	public void testCWhileStatement5() {
		doTest("wmin4791", "CWhileStatement5.c");
	}
}
