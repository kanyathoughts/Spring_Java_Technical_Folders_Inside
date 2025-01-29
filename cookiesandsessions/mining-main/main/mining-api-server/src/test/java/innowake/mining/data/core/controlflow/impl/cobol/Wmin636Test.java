/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for SECTION 
 */
public class Wmin636Test extends CobolCalculateControlFlowTest {

	/**
	 * Tests PERFORM paragraph statements where the paragraph names are only unique in their respective section.
	 */
	@Test
	public void testSections1() {
		doTest("MIN636A.cbl");
	}
	
	/**
	 * Tests a PERFORM section1 combined with PERFORM paragraph statements.
	 */
	@Test
	public void testSections2() {
		doTest("MIN636B.cbl");
	}
	
	/**
	 * Tests a PERFORM section1 THRU section2 statement
	 */
	@Test
	public void testSections3() {
		doTest("MIN636C.cbl");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin636", fileName);
	}
}
