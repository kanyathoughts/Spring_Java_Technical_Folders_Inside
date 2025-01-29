/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package wdis534b.test.impl;

import wdis534b.test.types.*;

/**
 * Tests that no dependencies get collected when package belongs to another project in class.
 */
@SuppressWarnings("all")
public class TestClass3 {

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
			/* package + types from other project */
			WDIS534C.test3.types.Interface8 ti_8_1 = new WDIS534C.test3.types.Class8<Interface8>();
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
