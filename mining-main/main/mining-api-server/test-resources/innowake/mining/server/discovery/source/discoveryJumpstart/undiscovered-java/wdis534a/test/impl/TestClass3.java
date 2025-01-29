/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package wdis534a.test.impl;

import wdis534a.test.TestInterface;
import wdis534a.test.normal.NormalAnnotation;

/**
 * Tests if dependencies get collected for {@link org.eclipse.jdt.core.dom.NormalAnnotation}s on type, constant, class variable,
 * member, local variables, methods and method parameters in class.
 * Also tests that the java built-in annotation and types don't get reported as dependencies.
 */
@NormalAnnotation(message = "Test1", error = false)
public class TestClass3 implements TestInterface {

	@NormalAnnotation(message = "Test2", error = false)
	private static final String MY_CONST1 = "";

	@NormalAnnotation(message = "Test3", error = false)
	private final String name;

	@NormalAnnotation(message = "Test4", error = false)
	public TestClass3(	@NormalAnnotation(message = "Test5", error = false) final String name) {
		this.name = name;
	}
	
	/**
	 * method1.
	 *
	 * @return always {@code true}
	 */
	@Override
	public boolean method1() {
		return "Today could be a good day".length() > "Not in this reality my friend :p".length();
	}

	/**
	 * method2.
	 * @param evilMode be evil or evil
	 * @param name your name; not {@code null}
	 * @return always {@code true}
	 */
	@Override
	@NormalAnnotation(message = "Test7", error = false)
	public boolean method2(@NormalAnnotation(message = "Test8", error = false) final boolean evilMode,
						   @NormalAnnotation(message = "Test9", error = false) final String name) {
		@NormalAnnotation(message = "Test10", error = false)
		final String mute = "Lucifer";
		return evilMode || mute.equals(name);
	}

	/**
	 * method3.
	 * @param xxx your name; not {@code null}
	 * @return always {@code true}
	 */
	@Override
	public <X> boolean method3(@NormalAnnotation(message = "Test11", error = false)  X xxx) {
		return xxx != null;
	}
}
