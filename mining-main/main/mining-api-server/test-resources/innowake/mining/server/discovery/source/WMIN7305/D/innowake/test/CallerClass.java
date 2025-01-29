/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

public class CallerClass {

	@SuppressWarnings("unused")
	public static void callerA(final String[] args) {
		final MyInterface1 myInt = new MyClass1();
		myInt.methodA();
	}

	public void callerB() {
		final MyAbstractClass1 myAbst = new MyClass1();
		myAbst.methodA();
		myAbst.methodB();
		myAbst.methodC();
	}

	static void callerC() {
		final MyClass1 myClass = new MyClass1();
		myClass.methodA();
		myClass.methodB();
		myClass.methodC();
	}

	void callerD() {
		final MyClass1 myClass = new MyClass1();
		myClass.methodC();
		myClass.methodC(true);
		myClass.methodD();
	}
}
