/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.java.annotation;

@NormalAnnotation1(message = "Test1", error = false)
public class TestClass3 implements TestInterface {

	@NormalAnnotation1(message = "Test2", error = false)
	private static final String MY_CONST1 = "";

	@NormalAnnotation1(message = "Test3", error = false)
	private final String name;

	@NormalAnnotation1(message = "Test4", error = false)
	public TestClass3(	@NormalAnnotation1(message = "Test5", error = false) final String name) {
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
	@NormalAnnotation1(message = "Test7", error = false)
	public boolean method2(@NormalAnnotation1(message = "Test8", error = false) final boolean evilMode,
						   @NormalAnnotation1(message = "Test9", error = false) final String name) {
		return evilMode || "Lucifer".equals(name);
	}

	/**
	 * method3.
	 * @param xxx your name; not {@code null}
	 * @return always {@code true}
	 */
	@Override
	public <X> boolean method3(@NormalAnnotation1(message = "Test10", error = false)  X xxx) {
		return xxx != null;
	}
}
