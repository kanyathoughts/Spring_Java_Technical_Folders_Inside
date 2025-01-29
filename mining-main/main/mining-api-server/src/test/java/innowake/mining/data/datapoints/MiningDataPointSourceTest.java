/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.datapoints;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsIn.in;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

import com.google.common.collect.Sets;

import graphql.schema.DataFetcher;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.datapoints.builder.MiningDataPointDefinitionWithCustomFetch;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.data.event.MiningDataPointSourceInvalidatedEvent;
import innowake.mining.server.graphql.controller.ClientGraphQlController;
import innowake.mining.server.graphql.controller.ProjectGraphQlController;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.MiningDataPointFilterCallbacks;
import innowake.mining.shared.datapoints.definition.MiningDataPointSortCallback;
import innowake.mining.shared.datapoints.definition.MiningEnumDefinition;
import innowake.mining.shared.model.Technology;

/**
 * Tests for {@link MiningDataPointSource} and {@link MiningDataPointBuilder}.
 */
class MiningDataPointSourceTest {
	
	private final long TEST_PROJECT_ID = 1;
	private static final String TYPE_NAME = "TestFilterAndSorting";
	private static final String QUERY_NAME = "testQuery";
	private static final String DATAPOINT = "testLongProp";

	@Test
	void testDefineDataType() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		assertEquals(1, registry.getTypeDefinitions().size());
		assertNotNull(registry.getTypeDefinitions().get("HelloType"));
		assertEquals("HelloType", registry.getTypeDefinitions().get("HelloType").getName());
		assertThat(dataPointSource.getClass().getName(), in(registry.getTypeDefinitions().get("HelloType").getProvidedBy()));
		assertNull(registry.getTypeDefinitions().get("HelloType").getClassName());
	}
	
	@Test
	void testDefineDataPoints() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
			
			builder.defineDataPoint("HelloType", "stringProp").type(ScalarType.STRING).add();
			builder.defineDataPoint("HelloType", "stringArrayProp").arrayOfType(ScalarType.STRING).add();
			builder.defineDataPoint("HelloType", "refProp").type("SomeOtherType").add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints = registry.getDataPointDefinitions().get("HelloType");
		
		assertEquals(ScalarType.STRING, dataPoints.get("stringProp").getScalarType());
		assertFalse(dataPoints.get("stringProp").isArray());
		assertNull(dataPoints.get("stringProp").getReferenceTypeName());
		assertThat(dataPointSource.getClass().getName(), in(dataPoints.get("stringProp").getProvidedBy()));
		
		assertEquals(ScalarType.STRING, dataPoints.get("stringArrayProp").getScalarType());
		assertTrue(dataPoints.get("stringArrayProp").isArray());
		assertNull(dataPoints.get("stringArrayProp").getReferenceTypeName());
		assertThat(dataPointSource.getClass().getName(), in(dataPoints.get("stringArrayProp").getProvidedBy()));
		
		assertEquals("SomeOtherType", dataPoints.get("refProp").getReferenceTypeName());
		assertFalse(dataPoints.get("refProp").isArray());
		assertNull(dataPoints.get("refProp").getScalarType());
		assertThat(dataPointSource.getClass().getName(), in(dataPoints.get("refProp").getProvidedBy()));
	}
	
	@Test
	void testDefineDataPointsOnProject() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
			builder.defineDataPoint("HelloType", "globalProp").type(ScalarType.STRING).add();
			builder.defineDataPoint("HelloType", "additionalProp").onlyOnProjects(TEST_PROJECT_ID).type(ScalarType.STRING).add();
			
			builder.defineType("HelloProjectType").onlyOnProjects(TEST_PROJECT_ID).add();
			builder.defineDataPoint("HelloProjectType", "projectProp").onlyOnProjects(TEST_PROJECT_ID).type(ScalarType.STRING).add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> dataPointsDefs;
		Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints;
		
		dataPointsDefs = registry.getDataPointDefinitions();
		assertEquals(1, dataPointsDefs.size());
		
		dataPoints = dataPointsDefs.get("HelloType");
		assertNotNull(dataPoints);
		assertEquals(1, dataPoints.size());
		assertTrue(dataPoints.containsKey("globalProp"));
		
		dataPointsDefs = registry.getDataPointDefinitions(Optional.of(TEST_PROJECT_ID));
		assertEquals(2, dataPointsDefs.size());
		
		dataPoints = dataPointsDefs.get("HelloType");
		assertNotNull(dataPoints);
		assertEquals(2, dataPoints.size());
		assertTrue(dataPoints.containsKey("globalProp"));
		assertTrue(dataPoints.containsKey("additionalProp"));
		
		dataPoints = dataPointsDefs.get("HelloProjectType");
		assertNotNull(dataPoints);
		assertEquals(1, dataPoints.size());
		assertTrue(dataPoints.containsKey("projectProp"));
	}
	
	@Test
	void testRedefineDataPointsOnProject() {
		final MiningDataPointSource dataPointSource = new MiningDataPointSource() {
			int n = 0;
			
			@Override
			public void provideDataPoints(MiningDataPointBuilder builder) {
				n++;
				if (! builder.getProjectId().isPresent()) {
					builder.defineType("HelloType").add();
					builder.defineDataPoint("HelloType", "globalProp").type(ScalarType.STRING).add();
				}
				builder.defineDataPoint("HelloType", "additionalProp" + n).onlyOnProjects(TEST_PROJECT_ID).type(ScalarType.STRING).add();
				builder.defineType("HelloProjectType" + n).onlyOnProjects(TEST_PROJECT_ID).add();
				builder.defineDataPoint("HelloProjectType" + n, "projectProp" + n).onlyOnProjects(TEST_PROJECT_ID).type(ScalarType.STRING).add();
			}
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		for (int n = 1; n < 4; n++) {
			Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> dataPointsDefs;
			Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints;
			
			if (n > 1) {
				registry.onDataPointSourceInvalidated(new MiningDataPointSourceInvalidatedEvent(dataPointSource,
						n % 2 == 0 ? Optional.of(EntityId.of(TEST_PROJECT_ID)) : Optional.empty()));
			}
			
			dataPointsDefs = registry.getDataPointDefinitions();
			assertEquals(1, dataPointsDefs.size());
			
			dataPoints = dataPointsDefs.get("HelloType");
			assertNotNull(dataPoints);
			assertEquals(1, dataPoints.size());
			assertTrue(dataPoints.containsKey("globalProp"));
			
			dataPointsDefs = registry.getDataPointDefinitions(Optional.of(TEST_PROJECT_ID));
			assertEquals(2, dataPointsDefs.size());
			
			dataPoints = dataPointsDefs.get("HelloType");
			assertNotNull(dataPoints);
			assertEquals(2, dataPoints.size());
			assertTrue(dataPoints.containsKey("globalProp"));
			assertTrue(dataPoints.containsKey("additionalProp" + n));
			
			dataPoints = dataPointsDefs.get("HelloProjectType" + n);
			assertNotNull(dataPoints);
			assertEquals(1, dataPoints.size());
			assertTrue(dataPoints.containsKey("projectProp" + n));
		}
	}
	
	@Test
	void testDefineDataPointWithUsage() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
			
			builder.defineDataPoint("HelloType", "testProp").type(ScalarType.STRING)
				.withUsage("foo")
				.withUsage("bar")
				.withUsageAttribute("foo", "hello", "world")
				.add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final MiningDataPointDefinition dataPoint = registry.getDataPointDefinitions().get("HelloType").get("testProp");
		
		assertEquals(new HashSet<>(Arrays.asList("foo", "bar")), dataPoint.getUsages());
		assertEquals("world", dataPoint.getUsageAttributes().get("foo").get("hello"));
	}
	
	@Test
	void testDefineDataPointWithCustomFetch() {
		final DataFetcher<String> dataFetcher = (env) -> "Hello, World";
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
			
			builder.defineDataPoint("HelloType", "testProp").type(ScalarType.STRING)
				.withCustomFetch(dataFetcher)
				.add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final MiningDataPointDefinitionWithCustomFetch dataPoint = registry.getDataPointDefinitions().get("HelloType").get("testProp");
		
		assertEquals(dataFetcher, dataPoint.getCustomFetch());
	}
	
	@Test
	void testDefineEnum() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineEnum("TestEnum")
				.withValues("FOO", "BAR", "BAZ")
				.add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final MiningEnumDefinition enumDef = registry.getEnumDefinitions().get("TestEnum");
		assertEquals("TestEnum", enumDef.getName());
		assertNull(enumDef.getClassName());
		assertEquals(Arrays.asList("FOO", "BAR", "BAZ"), enumDef.getValues());
	}
	
	@Test
	void testDefineEnumOnProject() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineEnum("TestEnum")
				.withValues("FOO", "BAR", "BAZ")
				.add();
			builder.defineEnum("ProjectEnum").onlyOnProjects(TEST_PROJECT_ID)
				.withValues("QWER", "ASDF", "ZXCV")
				.add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		Map<String, MiningEnumDefinition> enumDefs;
		MiningEnumDefinition enumDef;
		
		enumDefs = registry.getEnumDefinitions();
		assertEquals(1, enumDefs.size());
		enumDef = enumDefs.get("TestEnum");
		assertNotNull(enumDef);
		assertEquals(Arrays.asList("FOO", "BAR", "BAZ"), enumDef.getValues());
		
		enumDefs = registry.getEnumDefinitions(Optional.of(TEST_PROJECT_ID));
		assertEquals(2, enumDefs.size());
		assertTrue(enumDefs.containsKey("TestEnum"));
		enumDef = enumDefs.get("ProjectEnum");
		assertNotNull(enumDef);
		assertEquals(Arrays.asList("QWER", "ASDF", "ZXCV"), enumDef.getValues());
		
	}
	
	@Test
	void testDefineQuery() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
			
			builder.defineQuery("testQuery")
				.type(ScalarType.STRING)
				.withOptionalParameter("optionalStrParm", ScalarType.STRING)
				.withOptionalParameter("optionalRefParm", "SomeOtherType")
				.withOptionalParameterArray("optionalArrayStrParm", ScalarType.STRING)
				.withOptionalParameterArray("optionalArrayRefParm", "SomeOtherType")
				.withRequiredParameter("requiredStrParm", ScalarType.STRING)
				.withRequiredParameter("requiredRefParm", "SomeOtherType")
				.withRequiredParameterArray("requiredArrayStrParm", ScalarType.STRING)
				.withRequiredParameterArray("requiredArrayRefParm", "SomeOtherType")
				.add();
			
			builder.defineQuery("testQueryArray").arrayOfType(ScalarType.STRING).add();
			builder.defineQuery("testQueryRef").type("SomeOtherType").add();
			builder.defineQuery("testQueryRefArray").arrayOfType("SomeOtherType").add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final MiningDataPointDefinition testQueryDef = registry.getQueryDefinitions().get("testQuery");
		final List<MiningDataPointDefinition> parameters = testQueryDef.getParameters();
		final MiningDataPointDefinition testQueryArrayDef = registry.getQueryDefinitions().get("testQueryArray");
		final MiningDataPointDefinition testQueryRefDef = registry.getQueryDefinitions().get("testQueryRef");
		final MiningDataPointDefinition testQueryRefArrayDef = registry.getQueryDefinitions().get("testQueryRefArray");
		
		assertEquals("testQuery", testQueryDef.getName());
		assertEquals(ScalarType.STRING, testQueryDef.getScalarType());
		assertFalse(testQueryDef.isArray());
		assertEquals(8, parameters.size());
		assertEquals("optionalStrParm", parameters.get(0).getName());
		assertEquals(ScalarType.STRING, parameters.get(0).getScalarType());
		assertFalse(parameters.get(0).isArray());
		assertTrue(parameters.get(0).isNullable());
		
		assertEquals("optionalRefParm", parameters.get(1).getName());
		assertEquals("SomeOtherType", parameters.get(1).getReferenceTypeName());
		assertFalse(parameters.get(1).isArray());
		assertTrue(parameters.get(1).isNullable());
		
		assertEquals("optionalArrayStrParm", parameters.get(2).getName());
		assertEquals(ScalarType.STRING, parameters.get(2).getScalarType());
		assertTrue(parameters.get(2).isArray());
		assertTrue(parameters.get(2).isNullable());
		
		assertEquals("optionalArrayRefParm", parameters.get(3).getName());
		assertEquals("SomeOtherType", parameters.get(3).getReferenceTypeName());
		assertTrue(parameters.get(3).isArray());
		assertTrue(parameters.get(3).isNullable());
		
		assertEquals("requiredStrParm", parameters.get(4).getName());
		assertEquals(ScalarType.STRING, parameters.get(4).getScalarType());
		assertFalse(parameters.get(4).isArray());
		assertFalse(parameters.get(4).isNullable());
		
		assertEquals("requiredRefParm", parameters.get(5).getName());
		assertEquals("SomeOtherType", parameters.get(5).getReferenceTypeName());
		assertFalse(parameters.get(5).isArray());
		assertFalse(parameters.get(5).isNullable());
		
		assertEquals("requiredArrayStrParm", parameters.get(6).getName());
		assertEquals(ScalarType.STRING, parameters.get(6).getScalarType());
		assertTrue(parameters.get(6).isArray());
		assertFalse(parameters.get(6).isNullable());
		
		assertEquals("requiredArrayRefParm", parameters.get(7).getName());
		assertEquals("SomeOtherType", parameters.get(7).getReferenceTypeName());
		assertTrue(parameters.get(7).isArray());
		assertFalse(parameters.get(7).isNullable());
		
		assertEquals("testQueryArray", testQueryArrayDef.getName());
		assertEquals(ScalarType.STRING, testQueryArrayDef.getScalarType());
		assertTrue(testQueryArrayDef.isArray());
		
		assertEquals("testQueryRef", testQueryRefDef.getName());
		assertEquals("SomeOtherType", testQueryRefDef.getReferenceTypeName());
		assertFalse(testQueryRefDef.isArray());
		
		assertEquals("testQueryRefArray", testQueryRefArrayDef.getName());
		assertEquals("SomeOtherType", testQueryRefArrayDef.getReferenceTypeName());
		assertTrue(testQueryRefArrayDef.isArray());
	}
	
	@Test
	void testDefineString() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
			
			builder.defineDataPoint("HelloType", "stringProp1").type(ScalarType.STRING).add();
			builder.defineDataPoint("HelloType", "stringProp2").type(String.class).add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints = registry.getDataPointDefinitions().get("HelloType");
		final MiningDataPointDefinitionWithCustomFetch stringProp1Def = dataPoints.get("stringProp1");
		final MiningDataPointDefinitionWithCustomFetch stringProp2Def = dataPoints.get("stringProp2");
		
		assertEquals(ScalarType.STRING, stringProp1Def.getScalarType());
		assertEquals(ScalarType.STRING, stringProp2Def.getScalarType());
	}
	
	@Test
	void testDefineInteger() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
			
			builder.defineDataPoint("HelloType", "intProp1").type(ScalarType.INT).add();
			builder.defineDataPoint("HelloType", "intProp2").type(int.class).add();
			builder.defineDataPoint("HelloType", "intProp3").type(Integer.class).add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints = registry.getDataPointDefinitions().get("HelloType");
		final MiningDataPointDefinitionWithCustomFetch intProp1Def = dataPoints.get("intProp1");
		final MiningDataPointDefinitionWithCustomFetch intProp2Def = dataPoints.get("intProp2");
		final MiningDataPointDefinitionWithCustomFetch intProp3Def = dataPoints.get("intProp3");
		
		assertEquals(ScalarType.INT, intProp1Def.getScalarType());
		assertEquals(ScalarType.INT, intProp2Def.getScalarType());
		assertEquals(ScalarType.INT, intProp3Def.getScalarType());
	}
	
	@Test
	void testDefineLong() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
			
			builder.defineDataPoint("HelloType", "longProp1").type(ScalarType.LONG).add();
			builder.defineDataPoint("HelloType", "longProp2").type(long.class).add();
			builder.defineDataPoint("HelloType", "longProp3").type(Long.class).add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints = registry.getDataPointDefinitions().get("HelloType");
		final MiningDataPointDefinitionWithCustomFetch longProp1Def = dataPoints.get("longProp1");
		final MiningDataPointDefinitionWithCustomFetch longProp2Def = dataPoints.get("longProp2");
		final MiningDataPointDefinitionWithCustomFetch longProp3Def = dataPoints.get("longProp3");
		
		assertEquals(ScalarType.LONG, longProp1Def.getScalarType());
		assertEquals(ScalarType.LONG, longProp2Def.getScalarType());
		assertEquals(ScalarType.LONG, longProp3Def.getScalarType());
	}
	
	@Test
	void testDefineBoolean() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
			
			builder.defineDataPoint("HelloType", "booleanProp1").type(ScalarType.BOOLEAN).add();
			builder.defineDataPoint("HelloType", "booleanProp2").type(boolean.class).add();
			builder.defineDataPoint("HelloType", "booleanProp3").type(Boolean.class).add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints = registry.getDataPointDefinitions().get("HelloType");
		final MiningDataPointDefinitionWithCustomFetch booleanProp1Def = dataPoints.get("booleanProp1");
		final MiningDataPointDefinitionWithCustomFetch booleanProp2Def = dataPoints.get("booleanProp2");
		final MiningDataPointDefinitionWithCustomFetch booleanProp3Def = dataPoints.get("booleanProp3");
		
		assertEquals(ScalarType.BOOLEAN, booleanProp1Def.getScalarType());
		assertEquals(ScalarType.BOOLEAN, booleanProp2Def.getScalarType());
		assertEquals(ScalarType.BOOLEAN, booleanProp3Def.getScalarType());
	}
	
	@Test
	void testRedefineType() {
		final MiningDataPointSource dataPointSource1 = new MiningDataPointSource() {
			@Override
			public void provideDataPoints(MiningDataPointBuilder builder) {
				builder.defineType("OneType").add();
				builder.defineDataPoint("OneType", "intProp").type(ScalarType.INT).add();
			}
		};
		
		final MiningDataPointSource dataPointSource2 = new MiningDataPointSource() {
			int n = 0;
			
			@Override
			public void provideDataPoints(MiningDataPointBuilder builder) {
				n++;
				builder.defineType("TwoType" + (n / 2)).add();
				builder.defineDataPoint("TwoType" + (n / 2), "booleanProp").type(ScalarType.BOOLEAN).add();
				builder.defineDataPoint("TwoType" + (n / 2), "intProp" + n).type(ScalarType.INT).add();
				builder.defineDataPoint("TwoType" + (n / 2), "stringProp" + n).type(ScalarType.STRING).add();
			}
		};
		
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource1, dataPointSource2);
		
		for (int n = 1; n < 5; n++) {
			if (n > 1) {
				registry.onDataPointSourceInvalidated(new MiningDataPointSourceInvalidatedEvent(dataPointSource2));
			}
			
			final Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> dataTypes = registry.getDataPointDefinitions();
			assertEquals(2, dataTypes.size());
			
			final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints1 = dataTypes.get("OneType");
			assertEquals(1, dataPoints1.size());
			assertEquals(ScalarType.INT, dataPoints1.get("intProp").getScalarType());
			
			final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints2 = dataTypes.get("TwoType" + (n / 2));
			assertEquals(3, dataPoints2.size());
			assertEquals(ScalarType.BOOLEAN, dataPoints2.get("booleanProp").getScalarType());
			assertEquals(ScalarType.INT, dataPoints2.get("intProp" + n).getScalarType());
			assertEquals(ScalarType.STRING, dataPoints2.get("stringProp" + n).getScalarType());
		}
	}
	
	@Test
	void testDefineDate() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("HelloType").add();
			
			builder.defineDataPoint("HelloType", "dateProp").type(ScalarType.DATETIME).add();
			builder.defineDataPoint("HelloType", "datePropDt").type(Date.class).add();
			builder.defineDataPoint("HelloType", "datePropIns").type(Instant.class).add();
			builder.defineDataPoint("HelloType", "datePropOdt").type(OffsetDateTime.class).add();
			builder.defineDataPoint("HelloType", "datePropZdt").type(ZonedDateTime.class).add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints = registry.getDataPointDefinitions().get("HelloType");

		assertFalse(dataPoints.get("dateProp").isAlias());
		assertEquals(ScalarType.DATETIME, dataPoints.get("dateProp").getScalarType());
		assertEquals(ScalarType.DATETIME, dataPoints.get("datePropDt").getScalarType());
		assertEquals(ScalarType.DATETIME, dataPoints.get("datePropIns").getScalarType());
		assertEquals(ScalarType.DATETIME, dataPoints.get("datePropOdt").getScalarType());
		assertEquals(ScalarType.DATETIME, dataPoints.get("datePropZdt").getScalarType());

		assertEquals(ScalarType.TIMESTAMP, dataPoints.get("datePropTimestamp").getScalarType());
		assertEquals(ScalarType.TIMESTAMP, dataPoints.get("datePropDtTimestamp").getScalarType());
		assertEquals(ScalarType.TIMESTAMP, dataPoints.get("datePropInsTimestamp").getScalarType());
		assertEquals(ScalarType.TIMESTAMP, dataPoints.get("datePropOdtTimestamp").getScalarType());
		assertEquals(ScalarType.TIMESTAMP, dataPoints.get("datePropZdtTimestamp").getScalarType());
	}
	
	@Test
	void testDefineAlias() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("TestType").add();
			builder.defineType("TestType2").add();
			
			builder.defineDataPoint("TestType", "someDataPoint")
				.type(ScalarType.STRING)
				.notNull()
				.add();
			
			builder.defineDataPoint("TestType" , "someDataPointWithCustomType")
				.type("TestType2")
				.add();
			
			builder.defineDataPoint("TestType2", "someOtherDataPoint")
				.arrayOfType("SomeComplexType")
				.nullable()
				.add();
			
			builder.defineAlias("TestType", "someAlias")
				.forDataPoint("someDataPoint")
				.add();
			
			builder.defineAlias("TestType", "someOtherAlias")
				.forDataPoint("someDataPointWithCustomType")
				.withSubSelection("someOtherDataPoint")
				.withParameter("booleanParameter", true)
				.withParameter("numberParameter", 42)
				.withParameter("floatParameter", 4.2)
				.withParameter("stringParameter", "Hello, World!")
				.withParameter("enumParameter", Technology.COBOL)
				.add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPoints = registry.getDataPointDefinitions().get("TestType");
		assertEquals(4, dataPoints.size());
		
		final MiningDataPointDefinitionWithCustomFetch someAliasDef = dataPoints.get("someAlias");
		final MiningDataPointDefinitionWithCustomFetch someOtherAliasDef = dataPoints.get("someOtherAlias");
		
		assertEquals("TestType", someAliasDef.getParentTypeName());
		assertEquals("someAlias", someAliasDef.getName());
		assertTrue(someAliasDef.isAlias());
		assertEquals("someDataPoint", Assert.assertNotNull(someAliasDef.getAliasFor()).getAliasFor());
		assertEquals(ScalarType.STRING, someAliasDef.getScalarType());
		assertNull(someAliasDef.getReferenceTypeName());
		assertEquals(false, someAliasDef.isArray());
		assertEquals(false, someAliasDef.isNullable());
		
		assertEquals("TestType", someOtherAliasDef.getParentTypeName());
		assertEquals("someOtherAlias", someOtherAliasDef.getName());
		assertTrue(someOtherAliasDef.isAlias());
		assertEquals("someDataPointWithCustomType", Assert.assertNotNull(someOtherAliasDef.getAliasFor()).getAliasFor());
		assertNull(someOtherAliasDef.getScalarType());
		assertEquals("SomeComplexType", someOtherAliasDef.getReferenceTypeName());
		assertEquals(true, someOtherAliasDef.isArray());
		assertEquals(true, someOtherAliasDef.isNullable());
		/* use HashSet to compare ignoring order */
		assertEquals(new HashSet<>(Arrays.asList("booleanParameter: true", "numberParameter: 42", "floatParameter: 4.2", "stringParameter: \"Hello, World!\"", "enumParameter: COBOL")),
				new HashSet<>(Assert.assertNotNull(someOtherAliasDef.getAliasFor()).getParameters()));
	}
	
	@Test
	void testDefineInvalidAlias() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineAlias("TestType", "someAlias")
				.forDataPoint("doesNotExist")
				.add();
		};
		assertThrows(IllegalArgumentException.class, () -> DataPointTestHelper.getRegistry(dataPointSource));
	}

	@Test
	void testGetDataPointsWithUsageWithProjectProperty() {
		/* tests that registry.getDataPointDefinitionsWithUsage() correctly returns global AND project-specific data points */

		final MiningDataPointSource dataPointSource = builder -> {
			builder.defineType("HelloType").add();
			builder.defineDataPoint("HelloType", "globalProp").type(ScalarType.STRING).add();
			builder.defineDataPoint("HelloType", "projectProp").onlyOnProjects(TEST_PROJECT_ID).type(ScalarType.STRING).add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);

		final Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> dataPointDefinitions = registry.getDataPointDefinitions(Optional.of(TEST_PROJECT_ID));
		final Map<String, Map<String, MiningDataPointDefinition>> dataPointDefinitionsWithUsage = registry.getDataPointDefinitionsWithUsage(Optional.of(TEST_PROJECT_ID), Collections.emptyList());

		/* since we didn't actually specify any usages, the returned data points should be the same in either case */
		assertEquals(dataPointDefinitions, dataPointDefinitionsWithUsage);

		assertEquals(Collections.singleton("HelloType"), dataPointDefinitions.keySet());
		final Map<String, MiningDataPointDefinitionWithCustomFetch> helloTypeDef = dataPointDefinitions.get("HelloType");
		/* check that all data points were returned, global and project-specific one */
		assertEquals(Sets.newHashSet("globalProp", "projectProp"), helloTypeDef.keySet());
	}

	@Test
	void testEscapeName() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("TestType").add();
			builder.defineDataPoint("TestType", "1invalid Name-lol+")
					.type(String.class)
					.add();
		};

		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final Map<String, MiningDataPointDefinitionWithCustomFetch> dataPointDefinitions = registry.getDataPointDefinitions().get("TestType");

		final MiningDataPointDefinitionWithCustomFetch dp = dataPointDefinitions.get("_1invalid_Name_lol_");
		assertNotNull(dp);
		assertEquals("_1invalid_Name_lol_", dp.getName());
	}

	@Test
	void testInconsistentTypeNameForClass() {
		final MiningDataPointSource dataPointSource1 = (builder) -> {
			builder.defineType().representedBy(TestOtherModelClass.class).add();
		};

		final MiningDataPointSource dataPointSource2 = (builder) -> {
			builder.defineType("CustomName").representedBy(TestOtherModelClass.class).add();
		};

		final MiningDataPointSource dataPointSource3 = (builder) -> {
			builder.defineType().representedBy(TestOtherModelClass.class).add();
			builder.defineType("CustomName").representedBy(TestOtherModelClass.class).add();
		};

		final MiningDataPointSource dataPointSource4 = (builder) -> {
			builder.defineType("CustomName").representedBy(TestOtherModelClass.class).add();
			builder.defineType().representedBy(TestOtherModelClass.class).add();
		};

		/* using custom name after class was already registered with default name fails */
		assertThrows(IllegalArgumentException.class, () -> DataPointTestHelper.getRegistry(dataPointSource1, dataPointSource2));
		/* using default name for existing class registered with custom name does not fail */
		assertDoesNotThrow(() -> DataPointTestHelper.getRegistry(dataPointSource2, dataPointSource1));

		assertThrows(IllegalArgumentException.class, () -> DataPointTestHelper.getRegistry(dataPointSource3));
		assertDoesNotThrow(() -> DataPointTestHelper.getRegistry(dataPointSource4));
	}

	@Test
	void testInvalidateProject() {
		final MiningDataPointSource dataPointSource = Mockito.mock(MiningDataPointSource.class);
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);

		/* verify provideDataPoints() has been called once by the registry to collect the initial data points */
		Mockito.verify(dataPointSource, Mockito.times(1)).provideDataPoints(ArgumentMatchers.any());

		registry.onDataPointSourceInvalidated(new MiningDataPointSourceInvalidatedEvent(dataPointSource, Optional.of(EntityId.of(TEST_PROJECT_ID))));

		/* verify provideDataPoints() has been called again by the registry to re-collect data points for the project */
		Mockito.verify(dataPointSource, Mockito.times(2)).provideDataPoints(ArgumentMatchers.any());
	}

	@Test
	void testInvalidateDeletedProject() {
		final MiningDataPointSource dataPointSource = Mockito.mock(MiningDataPointSource.class);
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(true, dataPointSource);

		/* verify provideDataPoints() has been called once by the registry to collect the initial data points */
		Mockito.verify(dataPointSource, Mockito.times(1)).provideDataPoints(ArgumentMatchers.any());

		registry.onDataPointSourceInvalidated(new MiningDataPointSourceInvalidatedEvent(dataPointSource, Optional.of(EntityId.of(TEST_PROJECT_ID))));

		/* verify provideDataPoints() has NOT been called again by the registry since the project was marked for deletion */
		Mockito.verify(dataPointSource, Mockito.times(1)).provideDataPoints(ArgumentMatchers.any());
	}
	
	@Test
	void testConflictingTypeNames() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineDataPointsFromSchemaMappingAnnotations(ClientGraphQlController.class);
			builder.defineDataPointsFromSchemaMappingAnnotations(ProjectGraphQlController.class);
		};
		
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> dataPointDefinitions = registry.getDataPointDefinitions();
		assertNotNull(dataPointDefinitions);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> clientDataPointDefinitions = dataPointDefinitions.get("Client");
		assertNotNull(clientDataPointDefinitions);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> projectDataPointDefinitions = dataPointDefinitions.get("Project");
		assertNotNull(projectDataPointDefinitions);
	}
	
	@Test
	void testConflictingTypeNamesReverseOrder() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineDataPointsFromSchemaMappingAnnotations(ProjectGraphQlController.class);
			builder.defineDataPointsFromSchemaMappingAnnotations(ClientGraphQlController.class);
		};
		
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final Map<String, Map<String, MiningDataPointDefinitionWithCustomFetch>> dataPointDefinitions = registry.getDataPointDefinitions();
		assertNotNull(dataPointDefinitions);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> clientDataPointDefinitions = dataPointDefinitions.get("Client");
		assertNotNull(clientDataPointDefinitions);
		
		final Map<String, MiningDataPointDefinitionWithCustomFetch> projectDataPointDefinitions = dataPointDefinitions.get("Project");
		assertNotNull(projectDataPointDefinitions);
	}

	@Test
	void testDefineUUIDType() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("TestType").add();
			builder.defineDataPoint("TestType", "uuidDataPoint")
					.type(UUID.class)
					.add();
		};

		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final MiningDataPointDefinitionWithCustomFetch dp = registry.getDataPointDefinitions().get("TestType").get("uuidDataPoint");

		assertNotNull(dp, "Expected 'uuidDataPoint' data point to be defined");
		assertEquals(ScalarType.UUID, dp.getScalarType(), "Expected 'uuidDataPoint' to have UUID scalar type");
	}

	@Test
	void testDefineEntityIdType() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("TestType").add();
			builder.defineDataPoint("TestType", "entityIdDataPoint")
					.type(EntityId.class)
					.add();
		};

		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final MiningDataPointDefinitionWithCustomFetch dp = registry.getDataPointDefinitions().get("TestType").get("entityIdDataPoint");

		assertNotNull(dp, "Expected 'entityIdDataPoint' data point to be defined");
		assertEquals(ScalarType.ENTITY_ID, dp.getScalarType(), "Expected 'entityIdDataPoint' to have ENTITY_ID scalar type");
	}

	@Test
	void testDefinePagedType() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("TestPagedType").add();
			builder.defineDataPointsFromSchemaMappingAnnotations(TestPagedClass.class);
		};

		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final MiningDataPointDefinitionWithCustomFetch dp = registry.getDataPointDefinitions().get("TestPagedType").get("pagedTest");

		assertNotNull(dp, "Expected 'pagedTest' data point to be defined");
		assertEquals("PAGED_TestValueClass", dp.getReferenceTypeName(), "Expected 'pagedTest' to have PAGED_TestValueClass type");

		final MiningDataPointDefinitionWithCustomFetch content = registry.getDataPointDefinitions().get("PAGED_TestValueClass").get("content");
		assertNotNull(content, "Expected 'content' data point to be defined");
		assertEquals("TestValueClass", content.getReferenceTypeName(), "Expected 'content' to have TestValueClass type");

		final Map<String, MiningDataPointDefinitionWithCustomFetch> pagedType = registry.getDataPointDefinitions().get("PAGED_TestValueClass");

		assertTrue(pagedType.containsKey("totalPages"), "Expected Paged type to have 'totalPages' data point");
		assertTrue(pagedType.containsKey("totalElements"), "Expected Paged type to have 'totalElements' data point");
		assertTrue(pagedType.containsKey("number"), "Expected Paged type to have 'number' data point");
		assertTrue(pagedType.containsKey("offset"), "Expected Paged type to have 'offset' data point");
		assertTrue(pagedType.containsKey("size"), "Expected Paged type to have 'size' data point");
		assertTrue(pagedType.containsKey("limit"), "Expected Paged type to have 'limit' data point");
	}

	@Test
	void testWithFiltering() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(TYPE_NAME, DATAPOINT).withFiltering(QUERY_NAME, filtering -> {
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
			}).add();
			defineQueryOnBuilder(TYPE_NAME, QUERY_NAME, builder);
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final Map<String, MiningDataPointDefinitionWithCustomFetch> definitions =  registry.getDataPointDefinitions().get(TYPE_NAME);
		final MiningDataPointFilterCallbacks callbacks = definitions.get(DATAPOINT).getFilterCallbacks(QUERY_NAME);
		assertNotNull(definitions.get("testStringProp").getFilterCallbacks(QUERY_NAME));
		assertTrue(callbacks.getEq().isPresent());
	}
	
	@Test
	void testWithDoubleExtendFiltering() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(TYPE_NAME, DATAPOINT).withFiltering("Query1", filtering -> {
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
			}).add();
			builder.extend(TYPE_NAME, DATAPOINT).withFiltering("Query2", filtering -> {
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
			}).add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final Map<String, MiningDataPointDefinitionWithCustomFetch> definitions =  registry.getDataPointDefinitions().get(TYPE_NAME);
		final MiningDataPointFilterCallbacks callback1 = definitions.get(DATAPOINT).getFilterCallbacks("Query1");
		final MiningDataPointFilterCallbacks callback2 = definitions.get(DATAPOINT).getFilterCallbacks("Query2");
		//Assert that both callbacks have an equal operation defined
		assertTrue(callback1.getEq().isPresent());
		assertTrue(callback2.getEq().isPresent());
	}
	
	@Test
	void testWithDoubleExtendSorting() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(TYPE_NAME, DATAPOINT).withSorting("Query1", TestFilterAndSorting.TestOrderBuilder::sortId).add();
			builder.extend(TYPE_NAME, DATAPOINT).withSorting("Query2", TestFilterAndSorting.TestOrderBuilder::sortId).add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final Map<String, MiningDataPointDefinitionWithCustomFetch> definitions =  registry.getDataPointDefinitions().get(TYPE_NAME);
		final Optional<MiningDataPointSortCallback<Object>> callback1 = definitions.get(DATAPOINT).getSortCallback("Query1");
		final Optional<MiningDataPointSortCallback<Object>> callback2 = definitions.get(DATAPOINT).getSortCallback("Query2");
		//Assert that both callbacks have an equal operation defined
		assertTrue(callback1.isPresent());
		assertTrue(callback2.isPresent());
	}

	@Test
	void testWithFilteringCustomType() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("FilterTypeOverride").add();
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(TYPE_NAME, DATAPOINT).withFiltering(QUERY_NAME, ScalarType.STRING, filtering -> {
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
			}).add();
			builder.extend(TYPE_NAME, "testStringProp").withFiltering(QUERY_NAME, "FilterTypeOverride", filtering -> {
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
			}).add();
			defineQueryOnBuilder(TYPE_NAME, QUERY_NAME, builder);
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final Map<String, MiningDataPointDefinitionWithCustomFetch> definitions =  registry.getDataPointDefinitions().get(TYPE_NAME);
		final MiningDataPointFilterCallbacks longPropCallbacks = definitions.get(DATAPOINT).getFilterCallbacks(QUERY_NAME);
		final MiningDataPointFilterCallbacks stringPropCallbacks = definitions.get("testStringProp").getFilterCallbacks(QUERY_NAME);

		assertEquals(ScalarType.STRING, longPropCallbacks.getScalarType());
		assertNull(longPropCallbacks.getReferenceTypeName());
		assertEquals("FilterTypeOverride", stringPropCallbacks.getReferenceTypeName());
		assertNull(stringPropCallbacks.getScalarType());
	}

	@Test
	void testExtendOnUndefinedDatapoint() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(TYPE_NAME, "testBoolProp").withFiltering(QUERY_NAME, filtering -> {
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
			}).add();
			defineQueryOnBuilder(TYPE_NAME, QUERY_NAME, builder);
		};
		final Throwable exception = assertThrows(IllegalArgumentException.class, () -> {
			DataPointTestHelper.getRegistry(dataPointSource);
		});
		assertEquals("extend() was used, but data point TestFilterAndSorting.testBoolProp was not yet defined", exception.getMessage());
	}

	@Test
	void testExtendOnDataPointFromDifferentSource() {
		/* Tests that extending a data point defined by a different data point source works. Should not be done in practice because it depends on the order
		 * in which the sources are processed. See testExtendOnDataPointFromDifferentSourceWithDuplicateDefinition() how to do it properly */
		final MiningDataPointSource dataPointSource1 = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
		};

		final MiningDataPointSource dataPointSource2 = (builder) -> {
			builder.extend(TYPE_NAME, "testStringProp").withFiltering(QUERY_NAME, filtering -> {
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
			}).add();
		};

		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource1, dataPointSource2);
		final MiningDataPointDefinitionWithCustomFetch dp = registry.getDataPointDefinitions().get(TYPE_NAME).get("testStringProp");
		final MiningDataPointFilterCallbacks filterCallbacks = dp.getFilterCallbacks(QUERY_NAME);
		assertTrue(filterCallbacks.getEq().isPresent(), "data point should have been extended with 'eq' filter operation");
	}

	@Test
	void testExtendOnDataPointFromDifferentSourceWithDuplicateDefinition() {
		/* Tests that extending a data point defined by a different data point source works. */
		final MiningDataPointSource dataPointSource1 = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
		};

		final MiningDataPointSource dataPointSource2 = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(TYPE_NAME, "testStringProp").withFiltering(QUERY_NAME, filtering -> {
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
			}).add();
		};

		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource1, dataPointSource2);
		final MiningDataPointDefinitionWithCustomFetch dp = registry.getDataPointDefinitions().get(TYPE_NAME).get("testStringProp");
		final MiningDataPointFilterCallbacks filterCallbacks = dp.getFilterCallbacks(QUERY_NAME);
		assertTrue(filterCallbacks.getEq().isPresent(), "data point should have been extended with 'eq' filter operation");
	}

	@Test
	void testWithSorting() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(TYPE_NAME, DATAPOINT).withSorting(QUERY_NAME, TestFilterAndSorting.TestOrderBuilder::sortId).add();
			defineQueryOnBuilder(TYPE_NAME, QUERY_NAME, builder);
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final Map<String, MiningDataPointDefinitionWithCustomFetch> definitions =  registry.getDataPointDefinitions().get(TYPE_NAME);
		final Optional<MiningDataPointSortCallback<Object>> callbacks = definitions.get(DATAPOINT).getSortCallback(QUERY_NAME);
		assertTrue(definitions.get("testLongProp").getSortCallback(QUERY_NAME).isPresent());
		assertTrue(callbacks.isPresent());
	}

	@Test
	void testSortingAndFiltering() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(TYPE_NAME, DATAPOINT).withFiltering(QUERY_NAME, filtering -> {
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
			}).withSorting(QUERY_NAME, TestFilterAndSorting.TestOrderBuilder::sortId).add();
			defineQueryOnBuilder(TYPE_NAME, QUERY_NAME, builder);
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final MiningDataPointDefinitionWithCustomFetch datapointDefinition =  registry.getDataPointDefinitions().get(TYPE_NAME).get(DATAPOINT);
		final MiningDataPointFilterCallbacks filterCallbacks = datapointDefinition.getFilterCallbacks(QUERY_NAME);
		assertTrue(filterCallbacks.getEq().isPresent());
		final Optional<MiningDataPointSortCallback<Object>> sortCallbacks = datapointDefinition.getSortCallback(QUERY_NAME);
		assertTrue(sortCallbacks.isPresent());
	}

	@Test
	void testProjectSpecificAlias() {
		/* test project-specific alias to global data point */
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("TestType").add();
			builder.defineDataPoint("TestType", "testProp")
					.type(ScalarType.STRING)
					.add();

			builder.defineAlias("TestType", "testAlias")
					.forDataPoint("testProp")
					.onlyOnProjects(TEST_PROJECT_ID)
					.add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final List<MiningDataPointDefinitionWithPath> globalDataPoints = registry.getDataPointsForTypeRecursively("TestType");

		assertEquals(1, globalDataPoints.size());
		assertEquals(Set.of("testProp"), globalDataPoints.stream().map(MiningDataPointDefinition::getName).collect(Collectors.toSet()));

		final List<MiningDataPointDefinitionWithPath> projectSpecificDataPoints = registry.getDataPointsForTypeRecursively(
				Optional.of(TEST_PROJECT_ID), "TestType");


		assertEquals(2, projectSpecificDataPoints.size());
		assertEquals(Set.of("testProp", "testAlias"), projectSpecificDataPoints.stream().map(MiningDataPointDefinition::getName).collect(Collectors.toSet()));

		final Optional<MiningDataPointDefinitionWithPath> alias = projectSpecificDataPoints.stream().filter(dp -> dp.getName().equals("testAlias")).findAny();
		assertTrue(alias.isPresent(), "expected to find alias data point");
		assertEquals(ScalarType.STRING, alias.get().getScalarType());
	}

	@Test
	void testProjectSpecificAliasForProjectSpecificDataPoint() {
		/* test project-specific alias to project-specific data point */
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("TestType").onlyOnProjects(TEST_PROJECT_ID).add();
			builder.defineDataPoint("TestType", "testProp")
					.type(ScalarType.STRING)
					.onlyOnProjects(TEST_PROJECT_ID)
					.add();

			builder.defineAlias("TestType", "testAlias")
					.forDataPoint("testProp")
					.onlyOnProjects(TEST_PROJECT_ID)
					.add();
		};
		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final List<MiningDataPointDefinitionWithPath> globalDataPoints = registry.getDataPointsForTypeRecursively("TestType");

		assertEquals(0, globalDataPoints.size());

		final List<MiningDataPointDefinitionWithPath> projectSpecificDataPoints = registry.getDataPointsForTypeRecursively(
				Optional.of(TEST_PROJECT_ID), "TestType");


		assertEquals(2, projectSpecificDataPoints.size());
		assertEquals(Set.of("testProp", "testAlias"), projectSpecificDataPoints.stream().map(MiningDataPointDefinition::getName).collect(Collectors.toSet()));

		final Optional<MiningDataPointDefinitionWithPath> alias = projectSpecificDataPoints.stream().filter(dp -> dp.getName().equals("testAlias")).findAny();
		assertTrue(alias.isPresent(), "expected to find alias data point");
		assertEquals(ScalarType.STRING, alias.get().getScalarType());
	}

	@Test
	void testAliasForProjectSpecificDataPoint() {
		/* test global alias to project-specific data point - THIS DOES NOT WORK! */
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("TestType").onlyOnProjects(TEST_PROJECT_ID).add();
			builder.defineDataPoint("TestType", "testProp")
					.type(ScalarType.STRING)
					.onlyOnProjects(TEST_PROJECT_ID)
					.add();

			builder.defineAlias("TestType", "testAlias")
					.forDataPoint("testProp")
					.add();
		};

		/* throws IllegalArgumentException from RegistryModel.resolveAliasType(), because the aliased data point can not be found (which is correct) */
		assertThrows(IllegalArgumentException.class, () -> DataPointTestHelper.getRegistry(dataPointSource));
	}

	@Test
	void testExtendAliasWithFiltering() {
		/* tests that it is possible to "extend" an alias with filtering */
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("TestType").add();

			builder.defineDataPoint("TestType", "testProp")
					.type(ScalarType.STRING)
					.add();

			builder.defineAlias("TestType", "testAlias")
					.forDataPoint("testProp")
					.add();

			defineQueryOnBuilder("TestType", QUERY_NAME, builder);

			builder.extend("TestType", "testAlias").withFiltering(QUERY_NAME, filtering -> {
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
			}).add();
		};

		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final Map<String, MiningDataPointDefinitionWithCustomFetch> definitions =  registry.getDataPointDefinitions().get("TestType");
		final MiningDataPointFilterCallbacks callbacks = definitions.get("testAlias").getFilterCallbacks(QUERY_NAME);
		assertTrue(callbacks.getEq().isPresent(), "expected alias to have filter callback");
	}

	@Test
	void testSortByAlias() {
		/* tests that it is possible to "extend" an alias with sorting */
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType("TestType").add();

			builder.defineDataPoint("TestType", "testProp")
					.type(ScalarType.STRING)
					.add();

			builder.defineAlias("TestType", "testAlias")
					.forDataPoint("testProp")
					.add();

			defineQueryOnBuilder("TestType", QUERY_NAME, builder);

			builder.extend("TestType", "testAlias").withSorting(QUERY_NAME, (b, direction) -> {
				/* no-op - we just need the declaration for this test */
			}).add();
		};

		final DataPointRegistry registry = DataPointTestHelper.getRegistry(dataPointSource);
		final SortObjectService service = new SortObjectService(registry);

		final Map<String, Map<String, String>> schema = service.buildSortObjectsSchema(Optional.empty());
		assertNotNull(schema);
		final Map<String, MiningDataPointDefinitionWithCustomFetch> definitions =  registry.getDataPointDefinitions().get("TestType");
		final Optional<MiningDataPointSortCallback<Object>> callbacks = definitions.get("testAlias").getSortCallback(QUERY_NAME);
		assertTrue(callbacks.isPresent(), "expected alias to have sort callback");
	}

	private void defineQueryOnBuilder(final String type, final String queryName, final MiningDataPointBuilder builder) {
		builder.defineQuery(queryName)
				.type(type)
				.withRequiredParameter("projectId", ScalarType.LONG)
				.withOptionalParameter("filterObject", "Map_String_java_lang_Object")
				.add();
	}
}
