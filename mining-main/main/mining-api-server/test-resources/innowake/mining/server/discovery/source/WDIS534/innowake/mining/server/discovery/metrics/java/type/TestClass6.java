/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.java.type;

import java.math.BigDecimal;

import innowake.mining.server.discovery.metrics.java.type.test.Class1;

/**
 * Tests the correct processing of dependencies if a type references itself. 
 */
@SuppressWarnings("unused")
public class TestClass6 {

	private static final String REF1 = Class1.CONST1;
	private static final String REF2 = innowake.mining.server.discovery.metrics.java.type.test.Class1.CONST1;
	
	private static String sep1 = "The separator string 1";

	public static final TestClass6 INSTANCE1 = new TestClass6();
	public TestClass6 create1() {
		return new TestClass6();
	}

	private static String sep2 = "The separator string 2";
	private static String sep3 = "The separator string 3";

	public TestInnerInterface1 create2() {
		return new TestInnerInterface1() {};
	}

	public TestInnerClass1 create3() {
		return new TestInnerClass1();
	}


	public static interface TestInnerInterface1 {

		public static Boolean sep4 = Boolean.TRUE;
		public static Boolean sep5 = Boolean.TRUE;

		public static TestInnerInterface1 INSTANCE2 = new TestInnerInterface1() {};

		public static Boolean sep6 = Boolean.TRUE;

		public static TestInnerClass1 INSTANCE3 = new TestInnerClass1() {};

		default TestClass6 create4() {
			return new TestClass6();
		}
	}

	public static class TestInnerClass1 {

		public static Integer sep7 = Integer.valueOf(1111111111);

		public static TestInnerClass1 INSTANCE5 = new TestInnerClass1();

		public static Integer sep8 = Integer.valueOf(1111111111);
		public static Integer sep9 = Integer.valueOf(1111111111);

		public static TestInnerInterface1 INSTANCE4 = new TestInnerInterface1() {};

		public TestClass6 create2() {
			return new TestClass6();
		}
	}

	private static String sep10 = "The separator string 11";

	public enum TestEnum1 {
		A;

		public TestEnum1 create5(final String str) {
			switch (str) {
				case "A":
					return TestEnum1.A;
			}

			return null;
		}
	}

	private static String sep11 = "The separator string 11";
}
