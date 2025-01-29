/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import org.junit.Test;

/**
 * Tests to verify support for multiple entry points.
 */
public class Wmin4767Test extends Pl1CalculateControlFlowTest {

	/**
	 * Test to verify CFG for a simple Function Reference within a statement
	 */
	@Test
	public void testPl1ProgramForSimpleFunctionCall() {
		doTest("Pl1FunctionCall.pl1");
	}
	
	/**
	 * Test to verify CFG for nested Function References within a statement
	 */
	@Test
	public void testPl1ProgramForNestedFunctionCall() {
		doTest("Pl1FunctionCall2.pl1");
	}
	
	/**
	 * Test to verify CFG for nested Function References in a single function reference within a statement
	 */
	@Test
	public void testPl1ProgramForNestedFunctionCall2() {
		doTest("Pl1FunctionCall3.pl1");
	}
	
	/**
	 * Test to verify CFG for nested Function References preceded by call keyword within a statement
	 */
	@Test
	public void testPl1ProgramForNestedFunctionCall3() {
		doTest("Pl1FunctionCall4.pl1");
	}
	/**
	 * Test to verify CFG for Function References to a procedure outside main procedure.
	 */
	@Test
	public void testPl1ProgramForFunctionCall3() {
		doTest("Pl1ExternalFunctionCall.pl1");
	}
	
	/**
	 * Test to verify CFG for Function References to a procedure not present in same file.
	 */
	@Test
	public void testPl1ProgramForFunctionCall2() {
		doTest("Pl1ExternalFunctionCall2.pl1");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin4767", fileName);
	}

}
