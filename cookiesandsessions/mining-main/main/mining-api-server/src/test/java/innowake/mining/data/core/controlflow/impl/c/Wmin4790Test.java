/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import org.junit.Test;

/**
 * Base Implementation to check CGF generation for C program for the Branch statement.
 */
public class Wmin4790Test extends CCalculateControlFlowTest {

	/**
	 * This test contains if statement for C language.
	 */
	@Test
	public void testCifStatement() {
		doTest("wmin4790", "CifStatement.c");
	}
	
	/**
	 * This test contains if statement with else for C language.
	 */
	@Test
	public void testCifElseStatement() {
		doTest("wmin4790", "CifElseStatement.c");
	}
	
	/**
	 * This test contains only the expression statement as char for the C language.
	 */
	@Test
	public void testCSwitchStatementA() {
		doTest("wmin4790", "CSwitchStatementA.c");
	}
	
	/**
	 * This test contains the expression statement as operation for the C language.
	 */
	@Test
	public void testCSwitchStatementB() {
		doTest("wmin4790", "CSwitchStatementB.c");
	}
	
	/**
	 * This test contains the expression statement as a number for the C language.
	 */
	@Test
	public void testCSwitchStatementC() {
		doTest("wmin4790", "CSwitchStatementC.c");
	}
	
	/**
	 * This test contains the if else and switch all together for the C language.
	 */
	@Test
	public void testAllStatementWithBranch() {
		doTest("wmin4790", "AllStatementWithBranch.c");
	}
}
