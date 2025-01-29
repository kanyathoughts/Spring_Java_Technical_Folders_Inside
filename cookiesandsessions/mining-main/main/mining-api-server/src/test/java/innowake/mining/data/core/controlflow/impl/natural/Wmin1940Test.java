/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import org.junit.Test;

/**
 * Tests of loop event statements.
 */
public class Wmin1940Test extends NaturalCalculateControlFlowTest {

	@Test
	public void testSortWithBreak() {
		doTest("MIN1940A.nsp");
	}
	
	@Test
	public void testRepeatWithBreakProcessing() {
		doTest("MIN1940B.nsp");
	}
	
	@Test
	public void testReadWithBreak() {
		doTest("MIN1940C.nsp");
	}
	
	@Test
	public void testNestedReadWithBreak() {
		doTest("MIN1940D.nsp");
	}

	@Test
	public void testNestedReadWithLabeledAtStartEndOfData() {
		doTest("MIN1940E.nsp");
	}

	@Test
	public void testFindWithNoRecordsFound() {
		doTest("MIN1940F.nsp");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin1940", fileName, fileName);
	}
}
