/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for ADD and SUBTRACT statements that are wrapped in CobolSizeGuardedStmt.
 */
public class CollapsibleCobolSizeGuardedStatementTest extends CobolCalculateControlFlowTest {

	/**
	 * Tests ADD without ON SIZE ERROR
	 */
	@Test
	public void testAddWithoutOnSizeError() {
		doTest("AddWithoutOnSizeError.cbl");
	}
	
	/**
	 * Tests ADD with ON SIZE ERROR and NOT ON SIZE ERROR
	 */
	@Test
	public void testAddWithOnSizeError() {
		doTest("AddWithOnSizeError.cbl");
	}
	
	/**
	 * Tests SUBTRACT without ON SIZE ERROR
	 */
	@Test
	public void testSubtractWithoutOnSizeError() {
		doTest("SubtractWithoutOnSizeError.cbl");
	}
	
	/**
	 * Tests SUBTRACT with ON SIZE ERROR and NOT ON SIZE ERROR
	 */
	@Test
	public void testSubtractWithOnSizeError() {
		doTest("SubtractWithOnSizeError.cbl");
	}
	
	private void doTest(final String fileName) {
		doTest("add", fileName);
	}
}
