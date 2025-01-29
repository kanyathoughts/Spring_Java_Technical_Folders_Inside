/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

import innowake.ndt.core.parsing.ast.model.BranchStatement;
import innowake.ndt.core.parsing.ast.model.LoopStatement;

/**
 * Tests CFG labels of Cobol {@link BranchStatement} and {@link LoopStatement}.
 */
public class Wmin2194Test extends CobolCalculateControlFlowTest {

	@Test
	public void testIfStatements() {
		doTest("if.cbl");
	}

	@Test
	public void testEvaluateStatements() {
		doTest("evaluate.cbl");
	}

	private void doTest(final String fileName) {
		doTest("wmin2194", fileName, fileName);
	}
}
