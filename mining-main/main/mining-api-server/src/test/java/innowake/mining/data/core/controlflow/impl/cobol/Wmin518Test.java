/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import org.junit.Test;

/**
 * Tests for statements that can contain conditional options.
 */
public class Wmin518Test extends CobolCalculateControlFlowTest {
	
	/**
	 * Tests ADD with ON SIZE ERROR and NOT ON SIZE ERROR
	 */
	@Test
	public void testADD() {
		doTest("WMIN518A.cbl");
	}
	
	/**
	 * Tests RETURN with AT END and NOT AT END
	 */
	@Test
	public void testReturn() {
		doTest("WMIN518B.cbl");
	}
	
	/**
	 * Tests WRITE with INVALID KEY and NOT INVALID KEY
	 */
	@Test
	public void testWrite() {
		doTest("WMIN518C.cbl");
	}
	
	/**
	 * Tests REWRITE with INVALID KEY and NOT INVALID KEY
	 */
	@Test
	public void testRewrite() {
		doTest("WMIN518D.cbl");
	}
	
	/**
	 * Tests START with INVALID KEY and NOT INVALID KEY
	 */
	@Test
	public void testStart() {
		doTest("WMIN518E.cbl");
	}
	
	/**
	 * Tests SEARCH with AT END
	 */
	@Test
	public void testSearch() {
		doTest("WMIN518F.cbl");
	}
	
	/**
	 * Tests READ with INVALID KEY and NOT INVALID KEY
	 */
	@Test
	public void testRead1() {
		doTest("WMIN518G.cbl");
	}
	
	/**
	 * Tests READ with AT END and NOT AT END
	 */
	@Test
	public void testRead() {
		doTest("WMIN518H.cbl");
	}
	
	/**
	 * Tests SEARCH without AT END
	 */
	@Test
	public void testSearch2() {
		doTest("WMIN518I.cbl");
	}

	/**
	 * Tests ADD with only ON SIZE ERROR
	 */
	@Test
	public void testADD2() {
		doTest("WMIN518J.cbl");
	}
	
	/**
	 * Tests START with only INVALID KEY
	 */
	@Test
	public void testStart2() {
		doTest("WMIN518K.cbl");
	}

	/**
	 * Tests ADD as first statement. EntryPoint is supposed to link to the ADD statement and not the wrapper statement.
	 */
	@Test
	public void testADD3() {
		doTest("WMIN518L.cbl");
	}
	
	private void doTest(final String fileName) {
		doTest("wmin518", fileName);
	}
}
