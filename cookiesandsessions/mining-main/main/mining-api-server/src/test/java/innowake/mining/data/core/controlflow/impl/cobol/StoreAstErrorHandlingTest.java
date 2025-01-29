/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

import innowake.ndt.core.parsing.ast.AdvancedLocationProvider;

/**
 * Tests that errors in {@link AdvancedLocationProvider} get handled correctly on the mining side
 */
public class StoreAstErrorHandlingTest extends CobolCalculateControlFlowTest{
	
	
	/* this module used to crash out the server due to errors in the {@link AdvancedLocationProvider} */
	@Test
	public void testErrorHandlingInStoreAst() {
		doTest("MI11406.cbl");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin11406", fileName);
	}

}
