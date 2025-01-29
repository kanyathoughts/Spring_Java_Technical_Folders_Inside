/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.Test;

/**
 * Tests for PROCEDURE DIVISION with REPORT SECTION, existing and non-existing procedures.
 */
public class Wmin2688Test extends CobolCalculateControlFlowTest {

	/**
	 * Tests with REPORT SECTION
	 */
	@Test
	public void testProcedureWithReportSection() {
		doTest("wmin2688", "WMIN2688A.cbl");
	}
	
	/**
	 * Tests with existing procedure
	 */
	@Test
	public void testProcedureWithExistingProcedure() {
		doTest("wmin2688", "WMIN2688B.cbl");
	}
	
	/**
	 * Tests with non-existing procedure
	 */
	@Test
	public void testProcedureNonExistingProcedure() {
		final String errorMsg = "Target could not be resolved.";
		final Throwable exception = assertThrows(IllegalArgumentException.class, () -> {
			doTest("wmin2688", "WMIN2688C.cbl");
		});
		assertEquals(errorMsg, exception.getMessage());
	}
}
