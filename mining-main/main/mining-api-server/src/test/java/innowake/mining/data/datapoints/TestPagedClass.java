/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import org.springframework.graphql.data.method.annotation.SchemaMapping;

/**
 * Test class for defining {@link Paged} type via reflection.
 */
public class TestPagedClass {

	@MiningDataType(name = "TestValueClass")
	public static class TestValueClass {
		/* no properties */
	}

	@SchemaMapping(typeName = "TestPagedType")
	@Nullable
	public Paged<TestValueClass> pagedTest() {
		return null;
	}
}
