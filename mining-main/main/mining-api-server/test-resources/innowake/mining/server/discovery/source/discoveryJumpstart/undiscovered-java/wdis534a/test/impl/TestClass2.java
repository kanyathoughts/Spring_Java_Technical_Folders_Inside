/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package wdis534a.test.impl;

import java.util.Arrays;
import java.util.List;

import wdis534a.test.TestInterface;
import wdis534a.test.marker.MarkerAnnotation1;
import wdis534a.test.marker.MarkerAnnotation2;
import wdis534a.test.single.member.SingleMemberAnnotation1;
import wdis534a.test.single.member.SingleMemberAnnotation2;

/**
 * Tests if dependencies get collected for {@code MarkerAnnotation}s and {@code SingleMemberAnnotation}s on type, constant,
 * class variable, member, local variables, methods and method parameters in class.
 * Also tests that the java built-in annotation and types don't get reported as dependencies.
 */
@MarkerAnnotation2
@SingleMemberAnnotation2("test1")
@SuppressWarnings("all")
public class TestClass2 implements TestInterface {

	@SuppressWarnings("unused")
	@MarkerAnnotation1
	@SingleMemberAnnotation1("test2")
	@MarkerAnnotation2
	@SingleMemberAnnotation2("test3")
	private static final String MY_CONST1 = "";

	private static final String MY_CONST2 = "";

	@Deprecated
	private static final String MY_CONST3 = "";

	@MarkerAnnotation1
	@SingleMemberAnnotation1("test4")
	private final String name;

	@MarkerAnnotation1
	@Deprecated
	@SingleMemberAnnotation1("test5")
	public TestClass2(@MarkerAnnotation1 @MarkerAnnotation2 @SingleMemberAnnotation1("test6") final String name) {
		this.name = name;
	}
	
	/**
	 * method1.
	 *
	 * @return always {@code true}
	 */
	public boolean method1() {
		return "Today could be a good day".length() > "Not in this reality my friend :p".length();
	}

	/**
	 * method2.
	 * @param evilMode be evil or evil
	 * @param name your name; not {@code null}
	 * @return always {@code true}
	 */
	@MarkerAnnotation1
	@SingleMemberAnnotation1("test7")
	@MarkerAnnotation2
	@SingleMemberAnnotation2("test8")
	public boolean method2(@SuppressWarnings("unused") final boolean evilMode, @Deprecated final String name) {
		return evilMode || "Lucifer".equals(name);
	}

	/**
	 * method3.
	 * @param evilMode be evil or evil
	 * @param name your name; not {@code null}
	 * @return always {@code true}
	 */
	@MarkerAnnotation1
	@SingleMemberAnnotation1("test9")
	@MarkerAnnotation2
	@SingleMemberAnnotation2("test10")
	public <X> boolean method3(@SuppressWarnings("unused") @MarkerAnnotation1 @SingleMemberAnnotation1("test11") @MarkerAnnotation2 @SingleMemberAnnotation2("test9") X xxx) {
		return xxx != null;
	}

	/**
	 * method4.
	 *
	 * @return always {@code true}
	 */
	public boolean method4(final String param) {
		return "Today could be a good day".length() > "Not in this reality my friend :p".length();
	}

	/**
	 * method4.
	 *
	 * @return always {@code true}
	 */
	public boolean method5(@SuppressWarnings("unused") final String param) {
		@SuppressWarnings("unchecked")
		final List myList = Arrays.asList("");

		return "Today could be a good day".length() > "Not in this reality my friend :p".length();
	}
}
