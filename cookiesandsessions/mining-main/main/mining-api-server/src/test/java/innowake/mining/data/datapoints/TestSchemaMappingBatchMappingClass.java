/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import org.springframework.graphql.data.method.annotation.BatchMapping;
import org.springframework.graphql.data.method.annotation.SchemaMapping;

import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Test class for defining data points automatically from {@code @SchemaMapping} and {@code BatchMapping} annotations.
 */
public class TestSchemaMappingBatchMappingClass {

	@MiningDataPoint(displayName = "Custom Display Name", description = "Custom Description")
	@Usage(value = "bar", attributes = {
			@UsageAttribute(key = "the key", value = "the value"),
			@UsageAttribute(key = "the key2", value = "the value2")
	})
	@Usage("baz")
	@SchemaMapping(typeName = "HelloType")
	public String helloWorld() {
		return "Hello, World";
		
	}

	@MiningDataPoint(displayName = "Custom Display Name", description = "Custom Description")
	@Usage(value = "bar", attributes = {
			@UsageAttribute(key = "the key", value = "the value"),
			@UsageAttribute(key = "the key2", value = "the value2")
	})
	@Usage("baz")
	@BatchMapping(typeName = "HelloTypeBatch")
	public Map<Long, String> helloWorldBatch(final List<Long> ids) {
		return ids.stream().collect(Collectors.toMap(Function.identity(), id -> "Hello, World"));
	}
}
