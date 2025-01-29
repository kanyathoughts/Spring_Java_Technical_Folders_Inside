/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

import java.util.ArrayList;
import java.util.List;

public class MyClass5 extends MyClass4 {

	private static final List<String> CONSTANTS = new ArrayList<>();

	public void method51() {
		/* Method call */
		CONSTANTS.forEach(t -> super.defaultMethod1());
		/* Method call */
		CONSTANTS.forEach(t -> super.method1());
		/* Method call */
		CONSTANTS.forEach(t -> MyClass5.super.method2());
	}

	public void method52() {
		/* Method call */
		CONSTANTS.forEach(t -> this.method42(t));
		/* Method call */
		CONSTANTS.forEach(t -> this.method53(t));
		/* Method call */
		CONSTANTS.forEach(t -> MyClass5.this.method53(t));

		/* Method call */
		CONSTANTS.forEach(t -> MyClass1.staticMethod13(t));
	}

	public void method53(final String value) {
		final Runnable runnable1 = () -> {
			/* Method call */
			method54(value);
		};
		/* Method call */
		runnable1.run();
	}
	
	public void method54(final String value) {
		/* Method call */
		final Runnable runnable2 = () -> method54(value);
		/* Method call */
		runnable2.run();
	}
	
	public void method55(final String value) {
		/* Method call */
		CONSTANTS.forEach(c -> new Runnable() {

				@Override
				public void run() {
					/* Method call */
					MyClass5.this.method56(value, 0, Boolean.FALSE);
				}
			/* Method call */
			}.run()
		);
	}
		
	@SuppressWarnings("unused")
	private void method56(final String value, final int number, final Boolean flag) { }
}
