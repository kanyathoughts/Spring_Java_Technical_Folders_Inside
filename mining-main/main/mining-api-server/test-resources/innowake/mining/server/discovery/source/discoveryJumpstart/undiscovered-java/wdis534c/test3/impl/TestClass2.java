/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package wdis534c.test3.impl;

import wdis534c.test3.types.*;

/**
 * Tests if dependencies get collected for method parameters & return types and for local variables in class.
 */
@SuppressWarnings("all")
public class TestClass2 {

	private static Interface1 cm1(final Interface2 ti_2, final Enum1 te_1, final Class1 tc_1) {
		final Class2 tc_2 = null;

		return new Interface1() { };
	}

	private static Enum3 cm2() {
		final Interface4 ti_4 = new Interface4() {};
		final Enum3 te_3 = Enum3.A;
		final Class2 tc_2 = new Class2();

		return Enum3.A;
	}

	private static Class3 cm3() {
		return new Class3();
	}

	private Interface1 cm4(final Interface5 ti_5, final Enum4 te_4, final Class3 tc_3) {
		final Interface6 ti_6 = null;
		final Enum5 te_5 = null;
		final Class4 tc_4 = null;

		return null;
	}

	private Enum5 cm5() {
		return Enum5.A;
	}

	private Interface7 cm6() {
		return (Interface7) new Class7();
	}

	private wdis534c.test3.types.Class8<wdis534c.test3.types.Interface8> get() {
		return new Class8<>();
	}
}
