/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

public class MyClass3 implements MyInterface1 {
	
	private final String val;
	
	protected MyClass3() {
		/* Method call */
		defaultMethod1();
		/* Method call */
		val = method1();
	}

	@Override
	public String method1() {
		/* Method call */
		method2();
		final MyClass2 myClass4 = new MyClass2();
		/* Method call */
		return myClass4.method21();
	}

	@Override
	public void method2() {
		/* Method call */
		"abc".length();
		/* Method call */
		new MyClass2().method21();
		/* Method call */
		new MyClass1().staticMethod11();
		/* Method call */
		MyClass1.staticMethod13("");
	}
}
