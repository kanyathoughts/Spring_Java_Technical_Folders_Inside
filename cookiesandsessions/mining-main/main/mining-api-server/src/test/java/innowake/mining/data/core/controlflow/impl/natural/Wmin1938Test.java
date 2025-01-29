/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import org.junit.Test;

/**
 * Tests reinput and escape statements.
 */
public class Wmin1938Test extends NaturalCalculateControlFlowTest {

	@Test
	public void testReinputWithDifferentInputStatement() {
		doTest("MIN1938A.nsp");
	}
	
	@Test
	public void testReinputWithSingleInputStatement() {
		doTest("MIN1938B.nsp");
	}
	
	@Test
	public void testEscapeRoutineAndEscapeModule() {
		doTest("MIN1938C.nsp");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin1938", fileName, fileName);
	}
}
