/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;

import innowake.lib.core.api.lang.Nullable;

/**
 * Test class for defining queries automatically from {@code @QueryMapping} annotations.
 */
public class TestQueryMappingClass {

	
	@QueryMapping
	public List<String> helloQuery(@Argument String firstName, @Argument @Nullable final String lastName) {
		return Collections.singletonList("Hello " + firstName + (lastName == null ? "" : lastName));
	}

	@SuppressWarnings("unused")
	@QueryMapping
	public String queryWithCustomParameter(@MiningDataPoint(referenceTypeName = "CustomType") @Argument("customParm") final Map<String, Object> customParm) {
		return "Hello, World";
	}
}
