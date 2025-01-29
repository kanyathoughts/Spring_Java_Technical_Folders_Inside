/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for IF and EVALUATE
 */
public class Wmin306Test extends CobolCalculateControlFlowTest {

	@Test
	public void testSimple() {
		doTest("simple", "SIMPLE.cbl");
	}
	
	@Test
	public void testFalltrough() {
		doTest("falltrough", "FALLTHROUGH.cbl");
	}
	
	@Test
	public void testIf() {
		doTest("if", "IF.cbl");
	}
	
	@Test
	public void testIfElse() {
		doTest("if", "IFELSE.cbl");
	}
	
	@Test
	public void testIfNested() {
		doTest("if", "IFNESTED.cbl");
	}
	
	@Test
	public void testIfNested2() {
		doTest("if", "IFNESTED2.cbl");
	}
	
	@Test
	public void testElseNested() {
		doTest("if", "ELSENESTED.cbl");
	}
	
	@Test
	public void testEvaluate() {
		doTest("evaluate", "EVAL.cbl");
	}
	
	@Test
	public void testEvaluateWithFallThrough() {
		doTest("evaluate", "EVALFALLTHROUGH.cbl");
	}
	
	@Test
	public void testEvaluateOther() {
		doTest("evaluate", "EVALOTHER.cbl");
	}
}
