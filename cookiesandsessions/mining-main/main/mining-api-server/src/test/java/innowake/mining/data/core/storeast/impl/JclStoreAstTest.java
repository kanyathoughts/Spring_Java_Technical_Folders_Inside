/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import org.junit.Test;
import innowake.mining.data.core.AstToString;
import innowake.mining.shared.entities.ast.StoreAstPrototype;

/**
 * JCL specific tests for storeAst.
 */
public class JclStoreAstTest extends AbstractJclTest {

	/**
	 * Ast Test to verify JCL with single job
	 */
	@Test
	public void testJclStoreAst1() {
		doTest("wmin5864", "WMIN5864A.job");
	}

	/**
	 * Ast Test to verify JCL with single job with condition
	 */
	@Test
	public void testJclStoreAst2() {
		doTest("wmin5864", "WMIN5864B.job");
	}

	/**
	 * Ast Test to verify JCL with multiple jobs
	 */
	@Test
	public void testJclStoreAst3() {
		doTest("wmin5864", "WMIN5864C.job");
	}

	/**
	 * Ast Test to verify JCL with Step Exec
	 */
	@Test
	public void testJclStoreAst4() {
		doTest("wmin5864", "WMIN5864D.job");
	}

	/**
	 * Ast Test to verify JCL with Step Exec with condition
	 */
	@Test
	public void testJclStoreAst5() {
		doTest("wmin5864", "WMIN5864E.job");
	}

	/**
	 * Ast Test to verify Jcl Step If without else part
	 */
	@Test
	public void testJclStoreAst6() {
		doTest("wmin5864", "WMIN5864F.job");
	}

	/**
	 * Ast Test to verify Jcl Step If
	 */
	@Test
	public void testJclStoreAst7() {
		doTest("wmin5864", "WMIN5864G.job");
	}

	/**
	 * Ast Test to verify Jcl Step Exec with Proc
	 */
	@Test
	public void testJclStoreAst8() {
		doTest("wmin5864", "STEPEXECPROC.job");
	}

	private void doTest(final String folderName, final String... moduleNames) {
		final StoreAstPrototype node = storeAst(folderName, moduleNames);
		final String output = new AstToString(this::nodeToString).traverse(node).toString();
		assertOutput(folderName, moduleNames[0], output);
	}

}
