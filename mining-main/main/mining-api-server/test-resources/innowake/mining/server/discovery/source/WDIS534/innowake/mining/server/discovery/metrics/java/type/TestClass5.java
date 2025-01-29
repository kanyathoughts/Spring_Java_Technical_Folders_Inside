/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.java.type;

import innowake.mining.server.discovery.metrics.java.type.test.*;

@SuppressWarnings("all")
public class TestClass5 {

	private final TestInnerClass1 tic_1 = new TestInnerClass1(null, Enum1.A, new Class1());
	private final TestInnerClass2 tic_2 = new TestInnerClass2(new Class8());

	private class TestInnerClass1 implements Interface1, Interface9<Interface7> {

		private final Interface4 ti_4 = new Interface4() {};

		public TestInnerClass1(final Interface1 ti_1, final Enum1 te_1, final Class1 tc_1) {
			final Interface2 ti_2 = null;
			final Enum2 te_2 = null;
			final Class2 tc_2 = null;
		}
	}

	public static class TestInnerClass2 extends Class1 {

		private final Interface4 ti_4 = new Interface4() {};

		TestInnerClass2(final Class8<Interface8> tc_8) {
			innowake.mining.server.discovery.metrics.java.type.test.Interface8 ti_8_1 = new innowake.mining.server.discovery.metrics.java.type.test.Class8<innowake.mining.server.discovery.metrics.java.type.test.Interface8>();
		}

		@Override
		public String toString() {
			return super.toString();
		}
	}

	public enum TestInnerEnum1 {
		A;

		@Override
		public String toString() {
			return super.toString();
		}
	}
}
