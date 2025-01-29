/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

public abstract class MyAbstractClass1 implements MyInterface1 {

	@Override
	public void methodA() {
	}

	public void methodC() {
	}

	@SuppressWarnings("unused")
	public void methodC(final boolean methodOverloading) {
	}

	public void methodD() {
	}
}
