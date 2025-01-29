/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WDIS534C.test3;

import java.util.Collection;

import WDIS534C.test3.types.*;

/**
 * Tests if dependencies get collected for extended (parameterized) interfaces and parameters of default methods in interface.
 */
@SuppressWarnings("all")
public interface TestInterface1 extends Interface1, Interface9<Interface7> {

	default String get(final Interface1 ti_1) throws Exception1 {
		if (ti_1 == null) {
			throw new Exception1(new NullPointerException());
		}

		Collection<Interface2> collection = (Collection) ti_1;
		collection.stream()
					.filter(var -> var instanceof Interface8)
					.forEach(var -> new Class1());

		for (int i = 0; i < 10; i++) {
			switch (i) {
				case -1:
					return new Interface1() { }.toString();
				default:
					break;
			}
		}

		return "";
	}

	default String switchToName(final Enum1 enum1) {
		switch (enum1) {
			case A:
				return enum1.toString();
			default:
				break;
		}

		return "";
	}
}
