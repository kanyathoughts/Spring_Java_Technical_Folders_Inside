/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import java.util.function.Predicate;
import org.junit.Test;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.AstToString;
import innowake.mining.data.core.api.AbstractTraverser;
import innowake.mining.shared.entities.ast.StoreAstPrototype;
import innowake.ndt.cobol.parser.ast.statement.CobolLabelStmt;

public class CobolStoreAstTest extends AbstractCobolTest {
	private static final String COBOL_IF_STATEMENT = "CobolIfStmt";
	private static final String COBOL_THEN_BLOCK = "CobolThenBlock";
	private static final String IF_CONDITION_CHECK_1 = "IF TESTFIELD EQ 1";
	private static final String IF_CONDITION_CHECK_2 = "IF CICS-RESP EQ DFHRESP(NORMAL)";
	
	@Test
	public void test2NestedCopies() {
		doTest("test2NestedCopies", "A.cbl");
	}

	@Test
	public void testCopy() {
		doTest("testCopy", "A.cbl");
	}
	
	@Test
	public void test2Copies() {
		doTest("test2Copies", "A.cbl");
	}
	
	@Test
	public void testNestedCopies() {
		doTest("testNestedCopies", "A.cbl");
	}
	
	@Test
	public void testIfConditionStatement() {
		doTest("testIfConditionStatement", "A.cbl");
		doIfLabelTest("testIfConditionStatement", "A.cbl");
	}

	@Test
	public void testWmin494() {
		doTest("wmin494", "WMIN494.cbl");
	}
	
	@Test
	public void testWNDT3654() {
		doTest("WNDT3654", "WNDT3654.cbl");
	}

	/**
	 * Tests the {@ode AstNodeToMiningNodeFunctionImpl.handleBranchStatement(AstNode, StoreAstNode)}, if the condition paths are set correctly for Cobol
	 * {@ode IF} statements with and without comments.
	 */
	@Test
	public void testWmin3603IfStatement1() {
		doTest("wmin3603", "IF1.cbl");
	}
	
	@Test
	public void testWmin3603IfStatement2() {
		doTest("wmin3603", "IF2.cbl");
	}
	
	@Test
	public void testWmin3603IfStatement3() {
		/* nested branch statements */
		doTest("wmin3603", "IF3.cbl");
	}

	/**
	 * Tests the that the {@link AbstractTraverser} stops traversing when the {@ode reproductionPredicate} returns {@code true}.
	 */
	@Test
	public void testLabelStatement() {
		doTest("wmin3603", "ExitParagraph.cbl", astNode ->
			/* Stop if the current node is a CobolLabelStatement whose name ends with "-EXIT" */
			! (astNode instanceof CobolLabelStmt) || ! ((CobolLabelStmt) astNode).getName().endsWith("-EXIT")
		);
	}

	/**
	 * Tests the {@ode AstNodeToMiningNodeFunctionImpl.handleBranchStatement(AstNode, StoreAstNode)}, if the condition paths are set correctly for Cobol
	 * Cobol {@ode EVALUATE} statements with and without comments.
	 */
	@Test
	public void testWmin3603EvaluateStatement() {
		doTest("wmin3603", "Evaluate1.cbl");
		doTest("wmin3603", "Evaluate2.cbl");
		/* nested branch statements */
		doTest("wmin3603", "Evaluate3.cbl");
	}
	
	@Test
	public void testWMIN5241() {
		doTest("WMIN5241", "PRG1.cbl");
	}

	private void doTest(final String folderName, final String fileName) {
		doTest(folderName, fileName, null);
	}

	private void doTest(final String folderName, final String fileName, @Nullable final Predicate<innowake.ndt.core.parsing.ast.AstNode> processChildren) {
		final StoreAstPrototype node = storeAst(folderName, processChildren, fileName);
		final String output = new AstToString(this::nodeToString).traverse(node).toString();
		assertOutput(folderName, fileName, output);
	}
	
	private void doIfLabelTest(final String folderName, final String fileName) {
		final StoreAstPrototype node = storeAst(folderName, fileName);
		final StoreAstPrototype procedureDiv = node.children().stream().filter(checkNode -> "ProcedureDivision".equals(checkNode.type.orElse(null))).findFirst().get();
		final StoreAstPrototype ifNodes = procedureDiv.children().stream().filter(checkNode -> COBOL_IF_STATEMENT.equals(checkNode.type.orElse(null))).findFirst().get();
		assertEquals(IF_CONDITION_CHECK_1, ifNodes.label.orElse(null));
		final StoreAstPrototype ifThen = ifNodes.children().stream().filter(checkNode -> COBOL_THEN_BLOCK.equals(checkNode.type.orElse(null))).findFirst().get();
		final StoreAstPrototype ifThenIfStm = ifThen.children().stream().filter(checkNode -> COBOL_IF_STATEMENT.equals(checkNode.type.orElse(null))).findFirst().get();
		assertEquals(IF_CONDITION_CHECK_2, ifThenIfStm.label.orElse(null));
	}

}
