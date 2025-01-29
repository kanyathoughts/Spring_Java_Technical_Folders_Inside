/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

import java.util.Arrays;
import java.util.List;

public class MyClass2 {
	
	/* Method call */
	private final List<String> list = Arrays.asList("C", "DE", "EFG", "FGHI");

	{
		MyInterface1 m1 = new MyClass3();
		/* Method call */
		m1.method1();

		/* Method call */
		new MyClass3().method2();
	}
	
	public MyClass2() {
		/* Method call */
		new MyClass3().method1();

		MyInterface1 m1 = new MyClass3();
		/* Method call */
		m1.method1();

		MyClass3 m2 = new MyClass3();
		/* Method call */
		m2.method2();
	}

	public String method21() {
		/* Method call */
		new MyClass3().method1();

		MyClass3 m2 = new MyClass3();
		/* Method call */
		m2.method2();

		MyInterface1 m1 = new MyClass3();
		/* Method call */
		return m1.method1();
	}
}
