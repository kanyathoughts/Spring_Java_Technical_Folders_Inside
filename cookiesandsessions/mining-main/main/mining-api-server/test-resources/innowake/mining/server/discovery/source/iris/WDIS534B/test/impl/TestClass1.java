/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WDIS534B.test.impl;

import WDIS534B.test.types.*;
import innowake.lib.core.api.lang.NonNull;

/**
 * Tests if dependencies get collected for parameterized super class and super interface types in class.
 * Also tests that the java built-in annotation and types don't get reported as dependencies.
 */
public class TestClass1 extends Class8<Interface8> implements Interface1, Interface9<Interface7> {

	@Override
	@NonNull
	public String toString() {
		return super.toString();
	}
}
