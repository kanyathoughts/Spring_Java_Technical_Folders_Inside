/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

public interface MyInterface2 {

	default void defaultMethod1() {
		/* Method call */
		defaultMethod2(null);
	}
	
	default void defaultMethod2(final MyInterface1 parameter1) {
		/* Method call */
		parameter1.defaultMethod1();
	}
}
