/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import org.junit.Test;

/**
 * Base Implementation to check CGF generation for PL/I program with simple statements.
 */
public class Wmin4156Test extends Pl1CalculateControlFlowTest {

	/**
	 * Test to verify simple if else statements
	 */
	@Test
	public void testSimpleIfElse() {
		doTest("Wmin4156A.pl1");
	}
	
	
	/**
	 * Test to verify mutliple if else statements
	 */
	@Test
	public void testMultipleIfElse() {
		doTest("Wmin4156B.pl1");
	}
	
	/**
	 * Test to verify nested if else statements
	 */
	@Test
	public void testNestedIfElse() {
		doTest("Wmin4156C.pl1");
	}
	
	/**
	 * Test to verify miscellaneous if else statements
	 */
	@Test
	public void miscIfElse() {
		doTest("Wmin4156D.pl1");
	}
	
	/**
	 * Test to verify simple Select Group
	 */
	@Test
	public void SimpleSelectGroup() {
		doTest("Wmin4156E.pl1");
	}
	
	/**
	 * Test to verify Select Group with when statements
	 */
	@Test
	public void SelectGroupWithWhenStatements() {
		doTest("Wmin4156F.pl1");
	}
	
	/**
	 * Test to verify Select Group with only otherwise statements
	 */
	@Test
	public void SelectGroupWithOnlyOtherWiseStatements() {
		doTest("Wmin4156G.pl1");
	}
	
	/**
	 * Test to verify miscellaneous Select Group statements
	 */
	@Test
	public void MiscSelectGroup() {
		doTest("Wmin4156H.pl1");
	}
	
	/**
	 * Test to verify miscellaneous branch statemens ( if else , select group)
	 */
	@Test
	public void MiscBranchStatements() {
		doTest("Wmin4156I.pl1");
	}

	private void doTest(final String fileName) {
		doTest("wmin4156", fileName);
	}
}
