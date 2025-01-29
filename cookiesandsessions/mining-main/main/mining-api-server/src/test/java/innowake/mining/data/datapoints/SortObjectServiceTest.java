/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.collections4.map.HashedMap;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.datapoints.TestFilterAndSorting.TestOrderBuilder;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;

/**
 * Test class for {@linkplain SortObjectService} class.
 */
class SortObjectServiceTest {
	
	private static final Long PROJECT_ID = 1L;
	private static final String TYPE_NAME = "TestFilterAndSorting";
	private static final String QUERY_NAME = "testQuery";
	private static final String LONG_DATAPOINT = "testLongProp";
	private static final String STRING_DATAPOINT = "testStringProp";
	
	@Nullable
	private static SortObjectService service;
	
	@Test
	void testApplySortObjectAsc() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(TYPE_NAME, STRING_DATAPOINT).withSorting(QUERY_NAME, TestFilterAndSorting.TestOrderBuilder::sortName).add();
			
			defineQueryOnBuilder(TYPE_NAME, QUERY_NAME, builder);
		};
		service = new SortObjectService(DataPointTestHelper.getRegistry(dataPointSource));
		final Map<String, String> sortObject = new HashedMap<>();
		sortObject.put(STRING_DATAPOINT, "ASC");
		final TestOrderBuilder orderBuilder = new TestFilterAndSorting().new TestOrderBuilder();
		assertNotNull(service);
		service.applySortObject(PROJECT_ID, QUERY_NAME, Collections.singletonList(sortObject), orderBuilder);
		assertEquals(" testStringProp ASCENDING", orderBuilder.getQueryFilter());
	}
	
	@Test
	void testMultipleSortObjects() {
		final MiningDataPointSource dataPointSource = defineSorting();
		service = new SortObjectService(DataPointTestHelper.getRegistry(dataPointSource));
		final Map<String, String> sortObject = new HashedMap<>();
		sortObject.put(STRING_DATAPOINT, "ASC");
		sortObject.put(LONG_DATAPOINT, "DESC");
		final TestOrderBuilder orderBuilder = new TestFilterAndSorting().new TestOrderBuilder();
		assertNotNull(service);
		service.applySortObject(PROJECT_ID, QUERY_NAME, Collections.singletonList(sortObject), orderBuilder);
		assertEquals(" testStringProp ASCENDING testLongProp DESCENDING", orderBuilder.getQueryFilter());
	}
	
	@Test
	void testSortingSchemaCreation() {
		final MiningDataPointSource dataPointSource = defineSorting();
		service = new SortObjectService(DataPointTestHelper.getRegistry(dataPointSource));
		assertNotNull(service);
		final Map<String, Map<String, String>> sortObjectsSchema =  service.buildSortObjectsSchema(Optional.of(PROJECT_ID));
		assertTrue("Schema should contain SortObject_testQuery", sortObjectsSchema.containsKey("SortObject_testQuery"));
		
		final Map<String, String> sortObjects = sortObjectsSchema.get("SortObject_testQuery");
		assertEquals(2, sortObjects.size());
		assertTrue("sortObjects should contain testLongProp, testStringProp", sortObjects.keySet()
				.containsAll(Arrays.asList("testStringProp", "testLongProp")));
	}
	
	@Test
	@Disabled("We don't throw Exception anymore after adding postgres filter object")
	void testDataPointNotSortable() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			defineQueryOnBuilder(TYPE_NAME, QUERY_NAME, builder);
		};
		service = new SortObjectService(DataPointTestHelper.getRegistry(dataPointSource));
		final TestOrderBuilder orderBuilder = new TestFilterAndSorting().new TestOrderBuilder();
		final Map<String, String> sortObject = new HashedMap<>();
		sortObject.put(STRING_DATAPOINT, "ASC");
		final List<Map<String, String>> sortObjects = Collections.singletonList(sortObject);
		final SortObjectService serviceObj = Assert.assertNotNull(service);
		final Throwable exception = assertThrows(IllegalArgumentException.class, () -> {
			serviceObj.applySortObject(PROJECT_ID, QUERY_NAME, sortObjects, orderBuilder);
		});
		assertEquals("While applying sorting: Unable to sort by testStringProp: data point is not sortable", exception.getMessage());
	}
	
	@Test
	@Disabled("We don't throw Exception anymore after adding postgres filter object")
	void testUndefinedDataPoint() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			defineQueryOnBuilder(TYPE_NAME, QUERY_NAME, builder);
		};
		service = new SortObjectService(DataPointTestHelper.getRegistry(dataPointSource));
		final TestOrderBuilder orderBuilder = new TestFilterAndSorting().new TestOrderBuilder();
		final Map<String, String> sortObject = new HashedMap<>();
		sortObject.put("testBooleanProp", "ASC");
		final List<Map<String, String>> sortObjects = Collections.singletonList(sortObject);
		final SortObjectService serviceObj = Assert.assertNotNull(service);
		final Throwable exception = assertThrows(IllegalArgumentException.class, () -> {
			serviceObj.applySortObject(PROJECT_ID, QUERY_NAME, sortObjects, orderBuilder);
		});
		assertEquals("While applying sorting: Unable to sort by testBooleanProp: unknown data point", exception.getMessage());
	}

	private MiningDataPointSource defineSorting() {
		return (builder) -> {
			builder.defineType(TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(TYPE_NAME, STRING_DATAPOINT).withSorting(QUERY_NAME, TestFilterAndSorting.TestOrderBuilder::sortName).add();
			builder.extend(TYPE_NAME, LONG_DATAPOINT).withSorting(QUERY_NAME, TestFilterAndSorting.TestOrderBuilder::sortId).add();
			defineQueryOnBuilder(TYPE_NAME, QUERY_NAME, builder);
		};
	}
	
	private void defineQueryOnBuilder(final String type, final String QUERY_NAME, final MiningDataPointBuilder builder) {
		builder.defineQuery(QUERY_NAME)
		.type(type)
		.withRequiredParameter("projectId", ScalarType.LONG)
		.withOptionalParameter("sortObject", "Map_String_java_lang_Object")
		.add();
	}

}
