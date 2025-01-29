/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

public interface MyInterface1 {

	default void defaultMethod1() {
		defaultMethod1();
	}

	default void defaultMethod2(final String text) {
		text.length();
	}

	String method1();

	void method2();
}
