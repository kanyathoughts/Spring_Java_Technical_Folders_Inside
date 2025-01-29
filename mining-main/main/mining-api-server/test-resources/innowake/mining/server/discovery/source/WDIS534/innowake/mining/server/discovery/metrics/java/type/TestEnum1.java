/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.java.type;

import innowake.mining.server.discovery.metrics.java.type.test.*;

public enum TestEnum1 implements Interface1, Interface2, Interface9<Interface7> {

	A;

	@Override
	public String toString() {
		return super.toString();
	}

	public Interface1 get() {
		return new TestClass1();
	}
}
