/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.ndt.parsing.parser.source.java;
/**
 * Test input file for verifying total condition count
 */
public class MiscFile {

	public void ifMethod1() {
		int a = 5;
		if (a > 1 && a != 3 ? true : false) {
			System.out.println("pass");
		}
	}

	public void ifMethod2() {
		int a = 5;
		if (test(a > 1 ? true : false)) {
			System.out.println("pass");
		}
	}

	public void forMethod1() {
		for (int i = 1; i < 5 ? true : false; i++) {
			System.out.println(i);
		}
	}

	public void forMethod2() {
		for (int i = 1; test(i != 3 && i < 5 ? true : false); i++) {
			System.out.println(i);
		}
	}

	public void whileMethod1() {
		int i = 5;
		while (i != 7 && i != 8 && i < 10 ? true : false) {
			i++;
		}
	}

	public void whileMethod2() {
		int i = 5;
		while (test(i < 8 && i != 6 ? true : false)) {
			i++;
		}
	}

	@SuppressWarnings("unused")
	public void conditionalExpressionMethod() {
		int x = 0;
		int y;
		y = test(x > 1 && x != 2 ? true : false) ? 61 : 90;
	}

	@SuppressWarnings("unused")
	public boolean test(boolean a) {
		return true;
	}
}
