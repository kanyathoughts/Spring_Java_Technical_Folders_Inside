/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

import java.util.ArrayList;
import java.util.List;

public class MyClass6 extends MyClass5 {

	private static final List<String> CONSTANTS = new ArrayList<>();

	public void method61() {
		/* Method call */
		CONSTANTS.forEach(super::defaultMethod2);
		/* Method call */
		CONSTANTS.forEach(super::method53);
		/* Method call */
		CONSTANTS.forEach(MyClass6.super::method54);
	}
	
	public void method62() {
		/* Method call */
		CONSTANTS.forEach(this::method42);
		/* Method call */
		CONSTANTS.forEach(this::method64);
		/* Method call */
		CONSTANTS.forEach(MyClass6.this::method65);
	}
	
	public void method63() {

		/* Method call */
		CONSTANTS.forEach(MyClass1::staticMethod13);
	}

	@SuppressWarnings("unused")
	private void method64(final String value) { }

	@SuppressWarnings("unused")
	private void method65(final String value) { }
}
