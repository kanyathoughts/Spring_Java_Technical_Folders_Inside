/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.test;

public class MyClass1 {

	private static class InnerClass1 {

		private void innerMethodA() {
		}
	}

	private static class InnerClass2 {

		private void innerMethodA() {
		}

		void innerMethodB() {
		}
	}

	private class InnerClass3 extends InnerClass2 {

		private void innerMethodA() {
		}

		@Override
		void innerMethodB() {
			super.innerMethodA();
			super.innerMethodB();
		}
	}

	public void methodA() {
		final InnerClass1 ic1 = new InnerClass1();
		ic1.innerMethodA();

		final InnerClass2 ic2 = new InnerClass2();
		ic2.innerMethodA();
	}

	public void methodB() {
		final InnerClass3 ic3 = new InnerClass3();
		ic3.innerMethodA();
		ic3.innerMethodB();
	}
}
