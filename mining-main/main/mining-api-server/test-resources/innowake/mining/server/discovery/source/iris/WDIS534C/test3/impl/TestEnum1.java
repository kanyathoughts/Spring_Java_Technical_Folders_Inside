/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WDIS534C.test3.impl;

import WDIS534C.test3.types.*;

/**
 * Tests if dependencies get collected for implemented (parameterized) interfaces, method return types and local variables in enum.
 */
public enum TestEnum1 implements Interface1, Interface2, Interface9<Interface7> {

	A;

	@Override
	public String toString() {
		return super.toString();
	}

	public Interface1 get() {
		final TestClass1 tc_1 = new WDIS534C.test3.impl.TestClass1();
		if (tc_1.equals(new Class1())) {
			final String msg = "OMG";
			System.out.println(msg);
		}
		return null;
	}
}
