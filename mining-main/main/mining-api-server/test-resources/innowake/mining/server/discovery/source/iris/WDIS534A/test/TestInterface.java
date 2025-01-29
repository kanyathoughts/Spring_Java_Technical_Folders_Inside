/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WDIS534A.test;

import WDIS534A.test.marker.MarkerAnnotation1;
import WDIS534A.test.marker.MarkerAnnotation2;
import WDIS534A.test.single.member.SingleMemberAnnotation1;
import WDIS534A.test.single.member.SingleMemberAnnotation2;

/**
 * Tests if dependencies get collected for annotations on type, constants, default methods and method parameters in interface.
 * Also tests that the java built-in annotation and types don't get reported as dependencies.
 */
@MarkerAnnotation2
@SingleMemberAnnotation2("test1")
@SuppressWarnings("all")
public interface TestInterface {

	@SuppressWarnings("unused")
	@MarkerAnnotation1
	@SingleMemberAnnotation1("test2")
	@MarkerAnnotation2
	@SingleMemberAnnotation2("test3")
	static final String MY_CONST1 = "";

	static final String MY_CONST2 = "";

	/**
	 * method1.
	 *
	 * @return always {@code true}
	 */
	default boolean method1() {
		return "Today could be a good day".length() > "Not in this reality my friend :p".length();
	}

	/**
	 * method2.
	 * @param evilMode be evil or evil
	 * @param name your name; not {@code null}
	 * @return always {@code true}
	 */
	@MarkerAnnotation1
	@SingleMemberAnnotation1("test4")
	@MarkerAnnotation2
	@SingleMemberAnnotation2("test5")
	default boolean method2(@SuppressWarnings("unused") final boolean evilMode, @Deprecated final String name) {
		return evilMode || "Lucifer".equals(name);
	}

	/**
	 * method3.
	 * @param evilMode be evil or evil
	 * @param name your name; not {@code null}
	 * @return always {@code true}
	 */
	@MarkerAnnotation1
	@SingleMemberAnnotation1("test6")
	@MarkerAnnotation2
	@SingleMemberAnnotation2("test7")
	default <X> boolean method3(@SuppressWarnings("unused") @MarkerAnnotation1 @SingleMemberAnnotation1("test8") @MarkerAnnotation2 @SingleMemberAnnotation2("test9") X xxx) {
		return xxx != null;
	}
}
