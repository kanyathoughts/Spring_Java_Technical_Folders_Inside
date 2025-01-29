/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests whether
 */
public class ExpandableCfgTest extends CobolCalculateControlFlowTest {
	
	@Test
	public void testBranchStatements() {
		doTest("expandableBranchStatements.cbl");
	}

	private void doTest(final String fileName) {
		doTest("expandable", fileName, fileName);
	}
}
