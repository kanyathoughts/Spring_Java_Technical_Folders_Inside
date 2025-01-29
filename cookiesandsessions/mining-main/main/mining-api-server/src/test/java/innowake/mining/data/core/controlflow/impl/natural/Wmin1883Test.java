/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import org.junit.Test;

/**
 * Tests halt statements.
 */
public class Wmin1883Test extends NaturalCalculateControlFlowTest {
	
	@Test
	public void testHaltStatements() {
		doTest("MIN1883A.nsp");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin1883", fileName, fileName);
	}
}
