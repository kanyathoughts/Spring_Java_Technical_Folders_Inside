/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

import java.util.Arrays;
import java.util.List;

public class MyClass1 {

	/* Method call */
	private static final List<String> CONSTANTS = Arrays.asList("C", "DE", "EFG", "FGHI");

	static {
		/* Method call */
		CONSTANTS.clear();
	}
	
	public static String staticMethod11() {
		/* Method call */
		final String staticMethod2 = staticMethod12();
		/* Method call */
		if ("".equals(staticMethod2)) {
			/* Method call */
			return staticMethod13(staticMethod2);
		}

		final MyClass2 myClass4 = new MyClass2();
		/* Method call */
		return myClass4.method21();
	}
	
	private static String staticMethod12() {
		return "";
	}
	
	public static String staticMethod13(final String value) {
		/* Method call */  									/* Method call */
		return value == null || value.length() == 0 ? "" : (value.isEmpty() ? " " : value);
	}
}
