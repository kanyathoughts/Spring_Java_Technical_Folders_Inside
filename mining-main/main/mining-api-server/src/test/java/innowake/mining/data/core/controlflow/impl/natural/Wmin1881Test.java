/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import org.junit.Test;

/**
 * Tests Loops and ESCAPE statements
 */
public class Wmin1881Test extends NaturalCalculateControlFlowTest {
	
	@Test
	public void testNestedFindWithEscapeBottomLabel() {
		doTest("MIN1881A.nsp");
	}

	@Test
	public void testForStatement() {
		doTest("MIN1881B.nsp");
	}
	
	@Test
	public void testRepeateStatement() {
		doTest("MIN1881C.nsp");
	}
	
	@Test
	public void testReadAndSortStatement() {
		doTest("MIN1881D.nsp");
	}
	
	@Test
	public void testFindStatementThatIsNotALoop() {
		doTest("MIN1881E.nsp");
	}

	@Test
	public void testHistogramStatement() {
		doTest("MIN1881F.nsp");
	}
	
	@Test
	public void testReadWorkFileAndReadWorkFileOnce() {
		doTest("MIN1881G.nsp");
	}
	
	@Test
	public void testSelectStatement() {
		doTest("MIN1881H.nsp");
	}
	
	@Test
	public void testUploadPcFileStatement() {
		doTest("MIN1881I.nsp");
	}
	
	@Test
	public void testNestedRepeateLoopsWithEscapes() {
		doTest("MIN1881I.nsp");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin1881", fileName, fileName);
	}
}
