/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsIn.in;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;

import innowake.mining.data.datapoints.builder.MiningDataPointDefinitionWithCustomFetch;
import innowake.mining.data.datapoints.registry.DataPointRegistry;

import org.junit.jupiter.api.Test;

import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.MiningDataTypeDefinition;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;
import org.mockito.Mockito;

/**
 * Tests for reflective capabilities of {@link MiningDataPointBuilder} (i.e. auto-discovery of data points).
 */
class MiningDataPointSourceReflectionTest {
	
	@Test
	void testDefineDataPointsViaReflection() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType")
				.representedBy(TestModelClass.class)
				.withDefaultProperties()
				.add();
		};
		final DataPointRegistry registry = getRegistry(dataPointSource);
		
		final MiningDataTypeDefinition helloType = registry.getTypeDefinitions().get("HelloType");
		final Map<String, MiningDataPointDefinitionWithCustomFetch> helloTypeDataPoints = registry.getDataPointDefinitions().get(helloType.getName());
		final MiningDataTypeDefinition otherType = registry.getTypeDefinitions().get("TestOtherModelClassWithCustomName");
		final Map<String, MiningDataPointDefinitionWithCustomFetch> otherTypeDataPoints = registry.getDataPointDefinitions().get(otherType.getName());
		final MiningDataTypeDefinition otherType2 = registry.getTypeDefinitions().get("innowake_mining_data_datapoints_TestOtherModelClass2");
		final Map<String, MiningDataPointDefinitionWithCustomFetch> otherType2DataPoints = registry.getDataPointDefinitions().get(otherType2.getName());
		final MiningEnumDefinition enumType = registry.getEnumDefinitions().get("TestEnumWithCustomName");
		
		final MiningDataTypeDefinition mapStringStringType = registry.getTypeDefinitions().get("MAP_STRING_STRING");
		final MiningDataPointDefinitionWithCustomFetch mapStringStringKey = registry.getDataPointDefinitions().get("MAP_STRING_STRING").get("key");
		final MiningDataPointDefinitionWithCustomFetch mapStringStringValue = registry.getDataPointDefinitions().get("MAP_STRING_STRING").get("value");
		
		final MiningDataTypeDefinition mapIntRefType = registry.getTypeDefinitions().get("MAP_INT_TestOtherModelClassWithCustomName");
		final MiningDataPointDefinitionWithCustomFetch mapIntRefKey = registry.getDataPointDefinitions()
				.get("MAP_INT_TestOtherModelClassWithCustomName").get("key");
		final MiningDataPointDefinitionWithCustomFetch mapIntRefValue = registry.getDataPointDefinitions()
				.get("MAP_INT_TestOtherModelClassWithCustomName").get("value");
		
		final MiningDataTypeDefinition mapStringWithCollectionType = registry.getTypeDefinitions().get("MAP_STRING_ARRAY_OF_STRING");
		final MiningDataPointDefinitionWithCustomFetch mapStringWithCollectionKey = registry.getDataPointDefinitions()
				.get("MAP_STRING_ARRAY_OF_STRING").get("key");
		final MiningDataPointDefinitionWithCustomFetch mapStringWithCollectionValue = registry.getDataPointDefinitions()
				.get("MAP_STRING_ARRAY_OF_STRING").get("value");
		
		final MiningDataTypeDefinition mapStringWithCollectionReferenceType = registry.getTypeDefinitions().get("MAP_STRING_ARRAY_OF_TestOtherModelClassWithCustomName");
		final MiningDataPointDefinitionWithCustomFetch mapStringWithCollectionReferenceKey = registry.getDataPointDefinitions()
				.get("MAP_STRING_ARRAY_OF_TestOtherModelClassWithCustomName").get("key");
		final MiningDataPointDefinitionWithCustomFetch mapStringWithCollectionReferenceValue = registry.getDataPointDefinitions()
				.get("MAP_STRING_ARRAY_OF_TestOtherModelClassWithCustomName").get("value");
		
		assertEquals("HelloType", helloType.getName());
		assertEquals("innowake.mining.data.datapoints.TestModelClass", helloType.getClassName());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloType.getProvidedBy()));
		
		assertEquals("stringProp", helloTypeDataPoints.get("stringProp").getName());
		assertEquals(ScalarType.STRING, helloTypeDataPoints.get("stringProp").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("stringProp").getProvidedBy()));
		assertFalse(helloTypeDataPoints.get("stringProp").isArray());
		assertEquals("Display Name", helloTypeDataPoints.get("stringProp").getDisplayName());
		assertEquals("Custom Description", helloTypeDataPoints.get("stringProp").getDescription());
		
		assertEquals("intProp", helloTypeDataPoints.get("intProp").getName());
		assertEquals(ScalarType.INT, helloTypeDataPoints.get("intProp").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("intProp").getProvidedBy()));
		assertFalse(helloTypeDataPoints.get("intProp").isArray());
		
		assertEquals("longProp", helloTypeDataPoints.get("longProp").getName());
		assertEquals(ScalarType.LONG, helloTypeDataPoints.get("longProp").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("longProp").getProvidedBy()));
		assertFalse(helloTypeDataPoints.get("longProp").isArray());
		
		assertEquals("floatProp", helloTypeDataPoints.get("floatProp").getName());
		assertEquals(ScalarType.FLOAT, helloTypeDataPoints.get("floatProp").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("floatProp").getProvidedBy()));
		assertFalse(helloTypeDataPoints.get("floatProp").isArray());
		
		assertEquals("doubleProp", helloTypeDataPoints.get("doubleProp").getName());
		assertEquals(ScalarType.FLOAT, helloTypeDataPoints.get("doubleProp").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("doubleProp").getProvidedBy()));
		assertFalse(helloTypeDataPoints.get("doubleProp").isArray());
		
		assertNull(helloTypeDataPoints.get("ignoredProp"));
		
		assertEquals("refProp", helloTypeDataPoints.get("refProp").getName());
		assertEquals("TestOtherModelClassWithCustomName", helloTypeDataPoints.get("refProp").getReferenceTypeName());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("refProp").getProvidedBy()));
		assertFalse(helloTypeDataPoints.get("refProp").isArray());
		
		assertEquals("refProp2", helloTypeDataPoints.get("refProp2").getName());
		assertEquals("innowake_mining_data_datapoints_TestOtherModelClass2", helloTypeDataPoints.get("refProp2").getReferenceTypeName());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("refProp2").getProvidedBy()));
		assertFalse(helloTypeDataPoints.get("refProp2").isArray());
		
		assertEquals("enumProp", helloTypeDataPoints.get("enumProp").getName());
		assertEquals("TestEnumWithCustomName", helloTypeDataPoints.get("enumProp").getReferenceTypeName());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("enumProp").getProvidedBy()));
		assertFalse(helloTypeDataPoints.get("enumProp").isArray());
		
		assertEquals("listBoolProp", helloTypeDataPoints.get("listBoolProp").getName());
		assertEquals(ScalarType.BOOLEAN, helloTypeDataPoints.get("listBoolProp").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("listBoolProp").getProvidedBy()));
		assertTrue(helloTypeDataPoints.get("listBoolProp").isArray());
		
		assertEquals("setStringProp", helloTypeDataPoints.get("setStringProp").getName());
		assertEquals(ScalarType.STRING, helloTypeDataPoints.get("setStringProp").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("setStringProp").getProvidedBy()));
		assertTrue(helloTypeDataPoints.get("setStringProp").isArray());
		
		assertEquals("listRefProp", helloTypeDataPoints.get("listRefProp").getName());
		assertEquals("TestOtherModelClassWithCustomName", helloTypeDataPoints.get("listRefProp").getReferenceTypeName());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("listRefProp").getProvidedBy()));
		assertTrue(helloTypeDataPoints.get("listRefProp").isArray());
		
		assertEquals("setRefProp", helloTypeDataPoints.get("setRefProp").getName());
		assertEquals("innowake_mining_data_datapoints_TestOtherModelClass2", helloTypeDataPoints.get("setRefProp").getReferenceTypeName());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("setRefProp").getProvidedBy()));
		assertTrue(helloTypeDataPoints.get("setRefProp").isArray());
		
		assertEquals("mapStringStringProp", helloTypeDataPoints.get("mapStringStringProp").getName());
		assertEquals("MAP_STRING_STRING", helloTypeDataPoints.get("mapStringStringProp").getReferenceTypeName());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("mapStringStringProp").getProvidedBy()));
		assertTrue(helloTypeDataPoints.get("mapStringStringProp").isArray());
		
		assertEquals("MAP_STRING_STRING", mapStringStringType.getName());
		assertThat("HelloType", in(mapStringStringType.getProvidedBy()));
		assertEquals(ScalarType.STRING, mapStringStringKey.getScalarType());
		assertThat("HelloType", in(mapStringStringKey.getProvidedBy()));
		assertEquals(ScalarType.STRING, mapStringStringValue.getScalarType());
		assertThat("HelloType", in(mapStringStringValue.getProvidedBy()));
		assertFalse(mapStringStringValue.isArray());
		
		assertEquals("mapIntRefProp", helloTypeDataPoints.get("mapIntRefProp").getName());
		assertEquals("MAP_INT_TestOtherModelClassWithCustomName", helloTypeDataPoints.get("mapIntRefProp").getReferenceTypeName());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("mapIntRefProp").getProvidedBy()));
		assertTrue(helloTypeDataPoints.get("mapIntRefProp").isArray());
		
		assertEquals("MAP_INT_TestOtherModelClassWithCustomName", mapIntRefType.getName());
		assertThat("HelloType", in(mapIntRefType.getProvidedBy()));
		assertEquals(ScalarType.INT, mapIntRefKey.getScalarType());
		assertThat("HelloType", in(mapIntRefKey.getProvidedBy()));
		assertEquals("TestOtherModelClassWithCustomName", mapIntRefValue.getReferenceTypeName());
		assertThat("HelloType", in(mapIntRefValue.getProvidedBy()));
		assertFalse(mapIntRefValue.isArray());
		
		assertEquals("mapStringWithCollectionType", helloTypeDataPoints.get("mapStringWithCollectionType").getName());
		assertEquals("MAP_STRING_ARRAY_OF_STRING", helloTypeDataPoints.get("mapStringWithCollectionType").getReferenceTypeName());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("mapStringWithCollectionType").getProvidedBy()));
		assertTrue(helloTypeDataPoints.get("mapStringWithCollectionType").isArray());
		
		assertEquals("MAP_STRING_ARRAY_OF_STRING", mapStringWithCollectionType.getName());
		assertThat("HelloType", in(mapStringWithCollectionType.getProvidedBy()));
		assertEquals(ScalarType.STRING, mapStringWithCollectionKey.getScalarType());
		assertThat("HelloType", in(mapStringWithCollectionKey.getProvidedBy()));
		assertEquals(ScalarType.STRING, mapStringWithCollectionValue.getScalarType());
		assertThat("HelloType", in(mapStringWithCollectionValue.getProvidedBy()));
		assertTrue(mapStringWithCollectionValue.isArray());
		
		assertEquals("mapStringWithCollectionReferenceType", helloTypeDataPoints.get("mapStringWithCollectionReferenceType").getName());
		assertEquals("MAP_STRING_ARRAY_OF_TestOtherModelClassWithCustomName", helloTypeDataPoints.get("mapStringWithCollectionReferenceType").getReferenceTypeName());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("mapStringWithCollectionReferenceType").getProvidedBy()));
		assertTrue(helloTypeDataPoints.get("mapStringWithCollectionReferenceType").isArray());
		
		assertEquals("MAP_STRING_ARRAY_OF_TestOtherModelClassWithCustomName", mapStringWithCollectionReferenceType.getName());
		assertThat("HelloType", in(mapStringWithCollectionReferenceType.getProvidedBy()));
		assertEquals(ScalarType.STRING, mapStringWithCollectionReferenceKey.getScalarType());
		assertThat("HelloType", in(mapStringWithCollectionReferenceKey.getProvidedBy()));
		assertEquals("TestOtherModelClassWithCustomName", mapStringWithCollectionReferenceValue.getReferenceTypeName());
		assertThat("HelloType", in(mapStringWithCollectionReferenceValue.getProvidedBy()));
		assertTrue(mapStringWithCollectionReferenceValue.isArray());
		
		assertEquals("propWithSingleUsage", helloTypeDataPoints.get("propWithSingleUsage").getName());
		assertEquals(ScalarType.STRING, helloTypeDataPoints.get("propWithSingleUsage").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("propWithSingleUsage").getProvidedBy()));
		assertFalse(helloTypeDataPoints.get("propWithSingleUsage").isArray());
		assertEquals(new HashSet<>(Arrays.asList("foo")), helloTypeDataPoints.get("propWithSingleUsage").getUsages());
		assertTrue(helloTypeDataPoints.get("propWithSingleUsage").getUsageAttributes().isEmpty());
		
		assertEquals("propWithMultiUsage", helloTypeDataPoints.get("propWithMultiUsage").getName());
		assertEquals(ScalarType.STRING, helloTypeDataPoints.get("propWithMultiUsage").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestModelClass", in(helloTypeDataPoints.get("propWithMultiUsage").getProvidedBy()));
		assertFalse(helloTypeDataPoints.get("propWithMultiUsage").isArray());
		assertEquals(new HashSet<>(Arrays.asList("bar", "baz")), helloTypeDataPoints.get("propWithMultiUsage").getUsages());
		assertFalse(helloTypeDataPoints.get("propWithMultiUsage").getUsageAttributes().isEmpty());
		assertEquals("the value", helloTypeDataPoints.get("propWithMultiUsage").getUsageAttributes().get("bar").get("the key"));
		assertEquals("the value2", helloTypeDataPoints.get("propWithMultiUsage").getUsageAttributes().get("bar").get("the key2"));
		assertNull(helloTypeDataPoints.get("propWithMultiUsage").getUsageAttributes().get("baz"));
		
		assertEquals("TestOtherModelClassWithCustomName", otherType.getName());
		assertEquals("innowake.mining.data.datapoints.TestOtherModelClass", otherType.getClassName());
		assertThat("innowake.mining.data.datapoints.TestOtherModelClass", in(otherType.getProvidedBy()));
		
		assertEquals("testStringProp", otherTypeDataPoints.get("testStringProp").getName());
		assertEquals(ScalarType.STRING, otherTypeDataPoints.get("testStringProp").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestOtherModelClass", in(otherTypeDataPoints.get("testStringProp").getProvidedBy()));
		
		assertEquals("innowake_mining_data_datapoints_TestOtherModelClass2", otherType2.getName());
		assertEquals("innowake.mining.data.datapoints.TestOtherModelClass2", otherType2.getClassName());
		assertThat("innowake.mining.data.datapoints.TestOtherModelClass2", in(otherType2.getProvidedBy()));
		
		assertEquals("testStringProp", otherType2DataPoints.get("testStringProp").getName());
		assertEquals(ScalarType.STRING, otherType2DataPoints.get("testStringProp").getScalarType());
		assertThat("innowake.mining.data.datapoints.TestOtherModelClass2", in(otherType2DataPoints.get("testStringProp").getProvidedBy()));
		
		assertEquals("TestEnumWithCustomName", enumType.getName());
		assertEquals("innowake.mining.data.datapoints.TestEnum", enumType.getClassName());
		assertThat("innowake.mining.data.datapoints.TestEnum", in(enumType.getProvidedBy()));
		assertEquals(Arrays.asList("FOO", "BAR"), enumType.getValues());

		assertEquals("propWithCustomName", helloTypeDataPoints.get("propWithCustomName").getName());
		assertEquals(ScalarType.STRING, helloTypeDataPoints.get("propWithCustomName").getScalarType());

		assertEquals("stringAsEnumProp", helloTypeDataPoints.get("stringAsEnumProp").getName());
		assertNull(helloTypeDataPoints.get("stringAsEnumProp").getScalarType());
		assertEquals("TestEnumWithCustomName", helloTypeDataPoints.get("stringAsEnumProp").getReferenceTypeName());

		assertEquals("stringAsEnumProp2", helloTypeDataPoints.get("stringAsEnumProp2").getName());
		assertNull(helloTypeDataPoints.get("stringAsEnumProp2").getScalarType());
		assertEquals("TestEnumWithCustomName", helloTypeDataPoints.get("stringAsEnumProp2").getReferenceTypeName());

		assertEquals("mapAsJsonProp", helloTypeDataPoints.get("mapAsJsonProp").getName());
		assertEquals(ScalarType.JSON, helloTypeDataPoints.get("mapAsJsonProp").getScalarType());
		assertNull(helloTypeDataPoints.get("mapAsJsonProp").getReferenceTypeName());

		assertEquals("optionalProp", helloTypeDataPoints.get("optionalProp").getName());
		assertEquals(ScalarType.STRING, helloTypeDataPoints.get("optionalProp").getScalarType());
		assertNull(helloTypeDataPoints.get("optionalProp").getReferenceTypeName());
		assertTrue(helloTypeDataPoints.get("optionalProp").isNullable());
	}
	
	@Test
	void testDefineDataPointsViaSchemaMapping() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType")
				.add();
			builder.defineType("HelloTypeBatch")
				.add();
			builder.defineDataPointsFromSchemaMappingAnnotations(TestSchemaMappingBatchMappingClass.class);
		};
		final DataPointRegistry registry = getRegistry(dataPointSource);
		
		final MiningDataTypeDefinition helloType = registry.getTypeDefinitions().get("HelloType");
		final Map<String, MiningDataPointDefinitionWithCustomFetch> helloTypeDataPoints = registry.getDataPointDefinitions().get(helloType.getName());
		
		assertEquals(1, helloTypeDataPoints.size());
		
		final MiningDataPointDefinitionWithCustomFetch helloWorldDef = helloTypeDataPoints.get("helloWorld");
		assertEquals("helloWorld", helloWorldDef.getName());
		assertEquals("HelloType", helloWorldDef.getParentTypeName());
		assertEquals("Custom Display Name", helloWorldDef.getDisplayName());
		assertEquals("Custom Description", helloWorldDef.getDescription());
		assertEquals(new HashSet<>(Arrays.asList("bar", "baz")), helloWorldDef.getUsages());
		assertFalse(helloWorldDef.getUsageAttributes().isEmpty());
		assertEquals("the value", helloWorldDef.getUsageAttributes().get("bar").get("the key"));
		assertEquals("the value2", helloWorldDef.getUsageAttributes().get("bar").get("the key2"));
		assertNull(helloWorldDef.getUsageAttributes().get("baz"));
	}

	@Test
	void testDefineDataPointsViaBatchMapping() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType")
					.add();
			builder.defineType("HelloTypeBatch")
					.add();
			builder.defineDataPointsFromSchemaMappingAnnotations(TestSchemaMappingBatchMappingClass.class);
		};
		final DataPointRegistry registry = getRegistry(dataPointSource);

		final MiningDataTypeDefinition helloType = registry.getTypeDefinitions().get("HelloTypeBatch");
		final Map<String, MiningDataPointDefinitionWithCustomFetch> helloTypeDataPoints = registry.getDataPointDefinitions().get(helloType.getName());

		assertEquals(1, helloTypeDataPoints.size());

		final MiningDataPointDefinitionWithCustomFetch helloWorldDef = helloTypeDataPoints.get("helloWorldBatch");
		assertEquals("helloWorldBatch", helloWorldDef.getName());
		assertEquals("HelloTypeBatch", helloWorldDef.getParentTypeName());
		assertEquals("Custom Display Name", helloWorldDef.getDisplayName());
		assertEquals("Custom Description", helloWorldDef.getDescription());
		assertEquals(new HashSet<>(Arrays.asList("bar", "baz")), helloWorldDef.getUsages());
		assertFalse(helloWorldDef.getUsageAttributes().isEmpty());
		assertEquals("the value", helloWorldDef.getUsageAttributes().get("bar").get("the key"));
		assertEquals("the value2", helloWorldDef.getUsageAttributes().get("bar").get("the key2"));
		assertNull(helloWorldDef.getUsageAttributes().get("baz"));
	}
	
	@Test
	void testDefineQueryViaSchemaMapping() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineDataPointsFromSchemaMappingAnnotations(TestQueryMappingClass.class);
		};
		final DataPointRegistry registry = getRegistry(dataPointSource);
		
		final MiningDataPointDefinition helloQuery = registry.getQueryDefinitions().get("helloQuery");
		
		assertEquals(ScalarType.STRING, helloQuery.getScalarType());
		assertTrue(helloQuery.isArray());
		assertEquals(2, helloQuery.getParameters().size());
		
		assertEquals("firstName", helloQuery.getParameters().get(0).getName());
		assertEquals(ScalarType.STRING, helloQuery.getParameters().get(0).getScalarType());

		assertEquals("lastName", helloQuery.getParameters().get(1).getName());
		assertEquals(ScalarType.STRING, helloQuery.getParameters().get(1).getScalarType());
	}

	@Test
	void testDefineQueryViaSchemaMappingWithCustomParameter() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("CustomType");
			builder.defineDataPointsFromSchemaMappingAnnotations(TestQueryMappingClass.class);
		};
		final DataPointRegistry registry = getRegistry(dataPointSource);

		final MiningDataPointDefinition helloQuery = registry.getQueryDefinitions().get("queryWithCustomParameter");

		assertEquals(ScalarType.STRING, helloQuery.getScalarType());
		assertEquals(1, helloQuery.getParameters().size());

		assertEquals("customParm", helloQuery.getParameters().get(0).getName());
		assertEquals("CustomType", helloQuery.getParameters().get(0).getReferenceTypeName());
	}

	@Test
	void testSuperClassAnnotatedWithMiningDataType() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType().representedBy(TestModelSubType.class).withDefaultProperties().add();
		};
		final DataPointRegistry registry = getRegistry(dataPointSource);

		assertEquals(1, registry.getTypeDefinitions().size());
		assertNotNull(registry.getTypeDefinitions().get("TestModelSuperType"));

		final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints = registry.getDataPointDefinitions().get("TestModelSuperType");
		assertEquals(1, dataPoints.size());
		assertEquals("superTypeProp", dataPoints.get("superTypeProp").getName());
		assertEquals(ScalarType.STRING, dataPoints.get("superTypeProp").getScalarType());
	}
	
	private DataPointRegistry getRegistry(final MiningDataPointSource... sources) {
		final DataPointRegistry registry = new DataPointRegistry((event) -> {}, Mockito.mock(ProjectService.class));
		registry.initializeDataPointSources(Arrays.asList(sources));
		return registry;
	}
}
