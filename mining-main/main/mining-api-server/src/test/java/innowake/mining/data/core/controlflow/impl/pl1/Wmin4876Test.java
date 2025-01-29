/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import org.junit.Test;

/**
 * Tests to verify support for multiple entry points.
 */
public class Wmin4876Test extends Pl1CalculateControlFlowTest {

	/**
	 * Test to verify multiple entry points without Package Statement
	 */
	@Test
	public void testPl1ProgramWithoutPackageStatement() {
		doTest("Pl1PackageStatements.pl1");
	}

	/**
	 * Test to verify multiple entry points with Package Statement but without exports attribute
	 */
	@Test
	public void testPl1ProgramWithPackageStatementButWithoutExportsAttribute() {
		doTest("Pl1PackageStatementsA.pl1");
	}

	/**
	 * Test to verify multiple entry points with Package Statement & exports attribute
	 */
	@Test
	public void testPl1ProgramWithPackageStatementAndExportsAttribute() {
		doTest("Pl1PackageStatementsB.pl1");
	}

	/**
	 * Test to verify multiple entry points with Package Statement & export attributes but with all procedures allowed (exports(*))
	 */
	@Test
	public void testPl1ProgramWithExportsAttributeAllowAllProcedures() {
		doTest("Pl1PackageStatementsC.pl1");
	}

	/**
	 * Test to verify multiple entry points with Package Statement & exports attribute with valid procedure names list.
	 */
	@Test
	public void testPl1ProgramWithExportsAttributeWithProcedureNamesList() {
		doTest("Pl1PackageStatementsD.pl1");
	}

	private void doTest(final String fileName) {
		doTest("wmin4876", fileName);
	}

}
