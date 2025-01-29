/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import org.junit.Test;
import innowake.mining.data.core.AstToString;
import innowake.mining.shared.entities.ast.StoreAstPrototype;

/**
 * Specific tests for BASIC storeAst nodes.
 */
public class BasicStoreAstTest extends AbstractBasicStoreAstTest {
	
	/**
	 * AST Test to verify storeAst Nodes for basic program
	 */
	@Test
	public void testBasicStoreAstNodes() {
		doTest("basic", "BasicTest1.bas");
	}
	
	private void doTest(final String folderName, final String... moduleNames) {
		final StoreAstPrototype node = storeAst(folderName, moduleNames);
		final String output = new AstToString(this::nodeToString).traverse(node).toString();
		assertOutput(folderName, moduleNames[0], output);
	}
}
