/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for labels on edges.
 */
public class Wmin807Test extends CobolCalculateControlFlowTest {
	
	/**
	 * Tests ADD without ON SIZE ERROR and NOT ON SIZE ERROR
	 */
	@Test
	public void testADD() {
		doTest("WMIN807A.cbl");
	}
	
	/**
	 * Tests START without INVALID KEY and NOT INVALID KEY
	 */
	@Test
	public void testStart() {
		doTest("WMIN807B.cbl");
	}
	
	
	private void doTest(final String fileName) {
		doTest("wmin807", fileName);
	}
}
