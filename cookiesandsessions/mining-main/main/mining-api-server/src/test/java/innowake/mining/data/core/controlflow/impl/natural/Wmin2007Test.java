/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import org.junit.Test;

/**
 * Tests DECIDE FOR WHEN ANY statements.
 */
public class Wmin2007Test extends NaturalCalculateControlFlowTest {

	@Test
	public void testWhenAnyLabel() {
		doTest("MIN2007A.nsp");
	}
	
	
	private void doTest(final String fileName) {
		doTest("wmin2007", fileName, fileName);
	}
}
