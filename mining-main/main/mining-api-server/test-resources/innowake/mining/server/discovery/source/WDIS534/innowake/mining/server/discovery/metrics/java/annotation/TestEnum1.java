/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.java.annotation;

import innowake.lib.core.api.lang.Nullable;

@MarkerAnnotation2
@SingleMemberAnnotation2("test1")
public enum TestEnum1 {

	OP1("One"),
	OP2("Two"),
	OP3("Three");

	@Nullable
	@MarkerAnnotation1
	@SingleMemberAnnotation2("test2")
	private final String name;

	private TestEnum1(@Nullable final String name) {
		this.name = name;
	}

	@SingleMemberAnnotation2("test3")
	@Override
	public String toString() {
		return name;
	}
}
