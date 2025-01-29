/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for marking nodes as branch statements.
 */
public class Wmin827Test extends CobolCalculateControlFlowTest {

	/**
	 * Tests RETURN with and without AT END and NOT AT END
	 * If no END statement is present it is not a branch statement otherwise it is.
	 */
	@Test
	public void testRETURN() {
		doTest("WMIN827A.cbl");
	}
	
	/**
	 * Tests ADD with and without ON SIZE ERROR and NOT ON SIZE ERROR
	 * If no SIZE ERROOR statement is present it is not a branch statement otherwise it is.
	 */
	@Test
	public void testADD() {
		doTest("WMIN827B.cbl");
	}
	
	
	private void doTest(final String fileName) {
		doTest("wmin827", fileName);
	}
}
