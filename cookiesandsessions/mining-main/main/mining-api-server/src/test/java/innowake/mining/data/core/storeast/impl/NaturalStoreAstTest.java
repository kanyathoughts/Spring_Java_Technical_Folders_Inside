/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import org.junit.Test;

import innowake.mining.data.core.AstToString;
import innowake.mining.shared.entities.ast.StoreAstPrototype;

/**
 * Natural specific tests for storeAst.
 */
public class NaturalStoreAstTest extends AbstractNaturalTest {
	
	@Test
	public void testSimple() {
		doTest("wmin1204", "WMIN1204A.nsp");
	}
	
	@Test
	public void testCopycode() {
		doTest("wmin1204", "WMIN1204B.nsp", "WMIN1204C.nsc");
	}
	
	@Test
	public void testLda() {
		doTest("wmin1204", "WMIN1204D.nsp", "WMIN1204E.nsiwl");
	}
	
	@Test
	public void testWorkfileAccess() {
		doTest("wmin1205", "WMIN1205A.nsp");
	}
	
	@Test
	public void testDbAccess() {
		doTest("wmin1205", "WMIN1205B.nsp");
	}
	
	@Test
	public void testUi() {
		doTest("wmin1206", "WMIN1206A.nsp");
	}
	
	@Test
	public void testMultipleInclusions() {
		doTest("wmin1204", "WMIN1204F.nsp", "WMIN1204E.nsiwl", "WMIN1204C.nsc", "WMIN1204G.nsiwa", "WMIN1204H.nsc");
	}

	/**
	 * Tests the {@ode AstNodeToMiningNodeFunctionImpl.handleBranchStatement(AstNode, StoreAstNode)}, if the condition paths are set correctly for Natural
	 * {@ode IF} and {@ode IF SELECTION} statements.
	 */
	@Test
	public void testWmin3603IfStatement() {
		doTest("wmin3603", "IF1.nsp");
		doTest("wmin3603", "IF2.nsp");
		/* nested branch statements */
		doTest("wmin3603", "IF3.nsp");
	}

	/**
	 * Tests the {@ode AstNodeToMiningNodeFunctionImpl.handleBranchStatement(AstNode, StoreAstNode)}, if the condition paths are set correctly for Natural
	 * Cobol {@ode DECIDE FOR} statements.
	 */
	@Test
	public void testWmin3603DecideFor() {
		doTest("wmin3603", "DecideFor1.nsp");
		doTest("wmin3603", "DecideFor2.nsp");
		/* nested branch statements */
		doTest("wmin3603", "DecideFor3.nsp");
	}

	/**
	 * Tests the {@ode AstNodeToMiningNodeFunctionImpl.handleBranchStatement(AstNode, StoreAstNode)}, if the condition paths are set correctly for Natural
	 * Cobol {@ode DECIDE FOR} statements.
	 */
	@Test
	public void testWmin3603DecideOn() {
		doTest("wmin3603", "DecideOn1.nsp");
		doTest("wmin3603", "DecideOn2.nsp");
		/* nested branch statements */
		doTest("wmin3603", "DecideOn3.nsp");
	}

	private void doTest(final String folderName, final String... moduleNames) {
		final StoreAstPrototype node = storeAst(folderName, moduleNames);
		final String output = new AstToString(this::nodeToString).traverse(node).toString();
		assertOutput(folderName, moduleNames[0], output);
	}

}
