/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import org.junit.Test;

import innowake.ndt.core.parsing.ast.model.BranchStatement;
import innowake.ndt.core.parsing.ast.model.LoopStatement;

/**
 * Tests CFG labels of Natural {@link BranchStatement} and {@link LoopStatement}.
 */
public class Wmin2194Test extends NaturalCalculateControlFlowTest {

	@Test
	public void testIfStatements() {
		doTest("if.nsp");
	}

	@Test
	public void testIfSelectionStatements() {
		doTest("ifSelection.nsp");
	}

	@Test
	public void testDecideOnStatements() {
		doTest("decideOn.nsp");
	}

	@Test
	public void testDecideForStatements() {
		doTest("decideFor.nsp");
	}

	@Test
	public void testForStatements() {
		doTest("for.nsp");
	}

	@Test
	public void testRepeatStatements() {
		doTest("repeat.nsp");
	}

	@Test
	public void testReadWorkfileStatements() {
		doTest("readWorkfile.nsp");
	}

	@Test
	public void testFindStatements() {
		doTest("find.nsp");
	}

	@Test
	public void testReadStatements() {
		doTest("read.nsp");
	}

	@Test
	public void testHistogramStatements() {
		doTest("histogram.nsp");
	}

	@Test
	public void testDataLoopEventStatements() {
		doTest("dataLoopEvents.nsp");
	}

	private void doTest(final String fileName) {
		doTest("wmin2194", fileName, fileName);
	}
}
