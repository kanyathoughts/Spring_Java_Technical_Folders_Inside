/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

public class MyClass4 extends MyClass3 {
	
	private class MyInnerClass4 {
		public void inner1(final String value) {
			method42(value);
		}
		
		public void inner2(final String value) {
			method42(value);
		}
	}
	
	public MyClass4() {
		/* Method call */
		defaultMethod1();
		/* Method call */
		method1();
	}

	@Override
	public String method1() {
		/* Method call */
		method2();
		/* Method call */
		return super.method1();
	}

	public void method42(final String value) {
		final MyClass2 c2 = new MyClass2();
		/* Method call */
		c2.method21();

		/* Method call */
		MyClass1.staticMethod13(value);
	}
	
	public void method43(final String value) {
		final MyClass2 c3 = new MyClass2() {
			
			@Override
			public String method21() {
				/* Method call */
				method47(value, 0, Boolean.FALSE);
				/* Method call */
				return super.method21();
			}
		};
		/* Method call */
		c3.method21();
	}
	
	public void method44() {
		final MyInterface1 i1 = new MyInterface1() {
			
			@Override
			public void method2() {
				/* Method call */
				this.defaultMethod1();
			}
			
			@Override
			public String method1() {
				/* Method call */
				MyClass4.this.method1();
				/* Method call */
				MyClass4.super.method2();
				/* Method call */
				method42("");
				/* Method call */
				this.defaultMethod2("");
				return null;
			}
		};
		/* Method call */
		i1.method2();
	}

	public void method45(final String value) {
		final MyInnerClass4 innerClass1 = new MyInnerClass4();
		/* Method call */
		innerClass1.inner1(value);
	}

	public void method46(final String value) {
		final MyInnerClass4 innerClass2 = new MyInnerClass4() {
			
			@Override
			public void inner1(final String value) {
				/* Method call */
				method47(value, 0, Boolean.TRUE);
				/* Method call */
				another(value);
				/* Method call */
				inner2(value);
			}

			private void another(final String value) {
				/* Method call */
				method42(value);
				/* Method call */
				inner2(value);
			}
		};
		/* Method call */
		innerClass2.inner1(value);
	}
	
	@SuppressWarnings("unused")
	private void method47(final String value, final int number, final Boolean flag) {
	}
}
