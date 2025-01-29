/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package WDIS534A.test.impl;

import WDIS534A.test.marker.MarkerAnnotation1;
import WDIS534A.test.marker.MarkerAnnotation2;
import WDIS534A.test.single.member.SingleMemberAnnotation2;
import innowake.lib.core.api.lang.Nullable;

/**
 * Tests if dependencies get collected for annotations on type, member, methods and method parameters in enum.
 * Also tests that the java built-in annotation and types don't get reported as dependencies.
 */
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
