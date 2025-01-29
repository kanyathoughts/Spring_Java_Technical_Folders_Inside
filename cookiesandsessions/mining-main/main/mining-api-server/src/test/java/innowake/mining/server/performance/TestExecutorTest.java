/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.performance;

class TestExecutorTest {
	
//	Is there some kind annotation for manual test exclusion?
//	@Test 
	void test() {
		new TestExecutor.Builder()
			.test(this::testSleep)
			.warmup(5)
			.testCount(5)
			.sysout(true)
			.build()
			.runTest();
	}

	public void testSleep() {
		try {
			Thread.sleep(100);
		} catch (InterruptedException e) {
			throw new IllegalStateException(e);
		}
	}
}
