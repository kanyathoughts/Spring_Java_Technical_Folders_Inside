/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import org.junit.Test;

/**
 * Tests linking of simple statements.
 */
public class Wmin1822Test extends NaturalCalculateControlFlowTest {

	@Test
	public void testSimpleStatementLinking() {
		doTest("MIN1822A.nsp");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin1822", fileName, fileName);
	}
}
