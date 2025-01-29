/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests that errors in the parser get handled correctly on the mining side and do not cause the execution to crash
 */
public class ParserErrorHandlingTest extends CobolCalculateControlFlowTest{
	
	
	/* this module used to crash out the server due to errors in the {@link AdvancedLocationProvider} */
	@Test
	public void testErrorHandlingInStoreAst() {
		doTest("MI11406.cbl");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin11406", fileName);
	}

}
