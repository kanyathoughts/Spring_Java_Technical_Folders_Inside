/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WDIS534C.test3.impl;

import WDIS534C.test3.types.*;

/**
 * Tests if dependencies get collected for class variables, constants and fields in class.
 */
@SuppressWarnings("unused")
public class TestClass1 extends Class1 {

	/* Test constants */
	private static final Interface3 TI_3 = null;
	private static final Interface4 TI_4 = new Interface4() {};
	private static Enum1 TE_1 = null;
	private static final Enum2 TE_2 = Enum2.A;
	private static final Class1 TC_1 = null;
	private static Class2 TC_2 = new Class2();

	/* Test members */
	private WDIS534C.test3.types.Interface7 ti_7;
	private WDIS534C.test3.types.Interface1 ti_8 = new WDIS534C.test3.types.Interface1() { };
	private WDIS534C.test3.types.Interface9<WDIS534C.test3.types.Interface7> ti_9 = new WDIS534C.test3.types.Interface9<WDIS534C.test3.types.Interface7>() { };
	private WDIS534C.test3.types.Enum3 te_3 = null;
	private WDIS534C.test3.types.Enum4 te_4 = WDIS534C.test3.types.Enum4.A;
	private WDIS534C.test3.types.Class5 tc_5 = null;
	private WDIS534C.test3.types.Class6 tc_6 = new WDIS534C.test3.types.Class6();
	private WDIS534C.test3.types.Interface7 tic_7 = new WDIS534C.test3.types.Class7();

	private WDIS534C.test3.types.Interface8 ti_8_1 = new WDIS534C.test3.types.Class8<WDIS534C.test3.types.Interface8>();
	private WDIS534C.test3.types.Class8<Interface8> tc_8 = new Class8<>();
}
