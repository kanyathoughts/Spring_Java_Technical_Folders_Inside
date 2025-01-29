/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.datapoints;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.commons.collections4.map.HashedMap;
import org.apache.commons.lang3.tuple.Pair;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;
import org.junit.jupiter.params.provider.ArgumentsSource;

import com.google.gson.Gson;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.datapoints.MiningDataPointBuilder.FilterBuilder;
import innowake.mining.data.datapoints.TestFilterAndSorting.TestInquiryBuilder;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.spring.data.orientdb.api.query.clause.OrientClause;

/**
 * Test class for {@linkplain FilterObjectService} class.
 */
@TestMethodOrder(OrderAnnotation.class)
class FilterObjectServiceTest {
	
	private static final Optional<Long> PROJECT_ID = Optional.of(1L);
	private static final String TYPE_NAME = "TestType";
	private static final String QUERY_NAME = "testQuery";
	private static final Gson GSON = new Gson();
	private static final String DATAPOINT_TYPE_NAME = "TestFilterAndSorting";
	private static final String FILTER_QUERY_NAME = "testQuery";
	private static final String LONG_DATAPOINT = "testLongProp";
	private static final String STRING_DATAPOINT = "testStringProp";
	
	@Nullable
	private static FilterObjectService service;
	
	@BeforeAll
	static void setup() {
		final MiningDataPointSource dataPointSource = builder -> {
			builder.defineType(TYPE_NAME)
					.add();
			
			builder.defineDataPoint(TYPE_NAME, "stringProp")
					.type(ScalarType.STRING)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_EQ, "stringProp = ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_IN, "stringProp IN ?")
					.add();
			builder.defineDataPoint(TYPE_NAME, "numberProp")
					.type(ScalarType.INT)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_EQ, "numberProp = ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_GTE, "numberProp >= ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_GT, "numberProp > ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_LTE, "numberProp <= ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_LT, "numberProp < ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_IN, "numberProp IN ?")
					.add();
			builder.defineDataPoint(TYPE_NAME, "booleanProp")
					.type(ScalarType.BOOLEAN)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_TRUE, "booleanProp IS true")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_FALSE, "booleanProp IS NOT true")
					.add();
			builder.defineDataPoint(TYPE_NAME, "noneProp")
					.type(ScalarType.STRING)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_NONE, "noneProp IS Null")
					.add();
			builder.defineDataPoint(TYPE_NAME, "lowerCaseProp")
					.type(ScalarType.STRING)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_EQ, "lowerCaseProp = ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME,
							SearchFilterAttributes.SQL_FRAGMENT_EQ + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
							SearchFilterAttributes.SQL_FRAGMENT_FLAG_TO_LOWERCASE)
					.add();
			builder.defineDataPoint(TYPE_NAME, "escapeProp")
					.type(ScalarType.BOOLEAN)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_EQ, "escapeProp = ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME,
							SearchFilterAttributes.SQL_FRAGMENT_EQ + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
							SearchFilterAttributes.SQL_FRAGMENT_FLAG_ESCAPE_LUCENE)
					.add();
			builder.defineDataPoint(TYPE_NAME, "beginsWithProp")
					.type(ScalarType.BOOLEAN)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_EQ, "beginsWithProp = ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME,
							SearchFilterAttributes.SQL_FRAGMENT_EQ + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
							SearchFilterAttributes.SQL_FRAGMENT_FLAG_BEGINS_WITH)
					.add();
			builder.defineDataPoint(TYPE_NAME, "endsWithProp")
					.type(ScalarType.BOOLEAN)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_EQ, "endsWithProp = ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME,
							SearchFilterAttributes.SQL_FRAGMENT_EQ + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
							SearchFilterAttributes.SQL_FRAGMENT_FLAG_ENDS_WITH)
					.add();
			builder.defineDataPoint(TYPE_NAME, "beginsWithEscapeProp")
					.type(ScalarType.BOOLEAN)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_EQ, "beginsWithEscapeProp = ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME,
							SearchFilterAttributes.SQL_FRAGMENT_EQ + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
							SearchFilterAttributes.SQL_FRAGMENT_FLAG_ESCAPE_LUCENE + SearchFilterAttributes.SQL_FRAGMENT_FLAG_BEGINS_WITH)
					.add();
			builder.defineDataPoint(TYPE_NAME, "endsWithEscapeProp")
					.type(ScalarType.BOOLEAN)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME, SearchFilterAttributes.SQL_FRAGMENT_EQ, "endsWithEscapeProp = ?")
					.withUsageAttribute(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME,
							SearchFilterAttributes.SQL_FRAGMENT_EQ + SearchFilterAttributes.SQL_FRAGMENT_FLAGS_SUFFIX,
							SearchFilterAttributes.SQL_FRAGMENT_FLAG_ESCAPE_LUCENE + SearchFilterAttributes.SQL_FRAGMENT_FLAG_ENDS_WITH)
					.add();

			builder.defineDataPoint(TYPE_NAME, "customFilterProp")
					.type(ScalarType.STRING)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withFiltering(QUERY_NAME, f -> f.eq((b, value) -> {}))
					.add();

			builder.defineDataPoint(TYPE_NAME, "customFilterScalarTypeProp")
					.type(ScalarType.STRING)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withFiltering(QUERY_NAME, ScalarType.UUID, f -> f.eq((b, value) -> {}))
					.add();

			builder.defineType("FilterTypeOverride").add();
			builder.defineDataPoint(TYPE_NAME, "customFilterReferenceTypeProp")
					.type(ScalarType.STRING)
					.withUsage(Usages.GRAPHQL_QUERY_PREFIX + QUERY_NAME)
					.withFiltering(QUERY_NAME, "FilterTypeOverride", f -> f.eq((b, value) -> {}))
					.add();
			
			builder.defineQuery(QUERY_NAME)
					.type(TYPE_NAME)
					.add();
		};
		
		service = new FilterObjectService(DataPointTestHelper.getRegistry(dataPointSource));
	}
	
	static class ApplyFilterObjectsArgumentsProvider implements ArgumentsProvider {

		@Override
		public Stream<? extends Arguments> provideArguments(@Nullable final ExtensionContext context) {
			return Stream.of(
				Arguments.of(STRING_DATAPOINT, (Consumer<FilterBuilder>) filtering -> {filtering.isAbsent(TestFilterAndSorting.TestInquiryBuilder::isAbsent);}, 
						Collections.singletonMap("is", null), "testStringProp IS NULL"),
				Arguments.of(STRING_DATAPOINT, (Consumer<FilterBuilder>) filtering -> {filtering.notIn(TestFilterAndSorting.TestInquiryBuilder::withNameNotIn);}, 
						Collections.singletonMap("notIn", Arrays.asList("1", "2", "3", "4", "5")), "testStringProp != any(1,2,3,4,5)"),
				Arguments.of(LONG_DATAPOINT, (Consumer<FilterBuilder>) filtering -> {filtering.in(TestFilterAndSorting.TestInquiryBuilder::withIds);}, 
						Collections.singletonMap("in", Arrays.asList(1L, 2L, 3L, 4L, 5L)), "testLongProp = any(1,2,3,4,5)"),
				Arguments.of(STRING_DATAPOINT, (Consumer<FilterBuilder>) filtering -> {filtering.isFalse(TestFilterAndSorting.TestInquiryBuilder::isFalse);},
						Collections.singletonMap("is", false), "testStringProp = FALSE"),
				Arguments.of(STRING_DATAPOINT, (Consumer<FilterBuilder>) filtering -> {filtering.isTrue(TestFilterAndSorting.TestInquiryBuilder::isTrue);},
						Collections.singletonMap("is", true), "testStringProp = TRUE"),
				Arguments.of(LONG_DATAPOINT, (Consumer<FilterBuilder>) filtering -> {filtering.lte(TestFilterAndSorting.TestInquiryBuilder::withIdBelow);}, 
						Collections.singletonMap("lte", 1L), "testLongProp < 1"),
				Arguments.of(LONG_DATAPOINT, (Consumer<FilterBuilder>) filtering -> {filtering.gte(TestFilterAndSorting.TestInquiryBuilder::withIdAbove);}, 
						Collections.singletonMap("gte", 1L), "testLongProp > 1"),
				Arguments.of(LONG_DATAPOINT, (Consumer<FilterBuilder>) filtering -> {filtering.notEq(TestFilterAndSorting.TestInquiryBuilder::withIdNotEqual);}, 
						Collections.singletonMap("notEq", 1L), "testLongProp != 1"),
				Arguments.of(LONG_DATAPOINT, (Consumer<FilterBuilder>) filtering -> {filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);}, 
						Collections.singletonMap("eq", 1L), "testLongProp = 1")						
			);
		}
	}

	@Test
	@Order(1)
	void testSchemaCreation() {
		assertNotNull(service);
		final Map<String, Map<String, String>> filterObjectsSchema = service.buildFilterObjectsSchema(PROJECT_ID);
		assertTrue("Schema should contain FilterObject_testQuery", filterObjectsSchema.containsKey("FilterObject_testQuery"));
		assertTrue("Schema should contain FilterObject_testQuery_numberProp", filterObjectsSchema.containsKey("FilterObject_testQuery_numberProp"));
		assertTrue("Schema should contain FilterObject_testQuery_booleanProp", filterObjectsSchema.containsKey("FilterObject_testQuery_booleanProp"));
		assertTrue("Schema should contain FilterObject_testQuery_noneProp", filterObjectsSchema.containsKey("FilterObject_testQuery_noneProp"));
		assertTrue("Schema should contain FilterObject_testQuery_stringProp", filterObjectsSchema.containsKey("FilterObject_testQuery_stringProp"));
		assertTrue("Schema should contain FilterObject_testQuery_escapeProp", filterObjectsSchema.containsKey("FilterObject_testQuery_escapeProp"));
		assertTrue("Schema should contain FilterObject_testQuery_lowerCaseProp", filterObjectsSchema.containsKey("FilterObject_testQuery_lowerCaseProp"));
		assertTrue("Schema should contain FilterObject_testQuery_beginsWithProp", filterObjectsSchema.containsKey("FilterObject_testQuery_beginsWithProp"));
		assertTrue("Schema should contain FilterObject_testQuery_endsWithProp", filterObjectsSchema.containsKey("FilterObject_testQuery_endsWithProp"));
		assertTrue("Schema should contain FilterObject_testQuery_beginsWithEscapeProp",
					filterObjectsSchema.containsKey("FilterObject_testQuery_beginsWithEscapeProp"));
		assertTrue("Schema should contain FilterObject_testQuery_endsWithEscapeProp",
					filterObjectsSchema.containsKey("FilterObject_testQuery_endsWithEscapeProp"));

		final Map<String, String> numberProp = filterObjectsSchema.get("FilterObject_testQuery_numberProp");
		assertEquals(6, numberProp.size());
		assertTrue("numberProp should contain eq, lte, gte, lt, gt, in", numberProp.keySet().containsAll(Arrays.asList("eq", "lte", "lt", "gte", "gt", "in")));
		
		final Map<String, String> stringProp = filterObjectsSchema.get("FilterObject_testQuery_stringProp");
		assertEquals(2, stringProp.size());
		assertTrue("stringProp should contain eq, in", stringProp.keySet().containsAll(Arrays.asList("eq", "in")));
		
		final Map<String, String> booleanProp = filterObjectsSchema.get("FilterObject_testQuery_booleanProp");
		assertEquals(1, booleanProp.size());
		assertTrue("booleanProp should contain is", booleanProp.containsKey("is"));
		
		final Map<String, String> noneProp = filterObjectsSchema.get("FilterObject_testQuery_noneProp");
		assertEquals(1, noneProp.size());
		assertTrue("noneProp should contain is", noneProp.containsKey("is"));
		
		final Map<String, String> escapeProp = filterObjectsSchema.get("FilterObject_testQuery_escapeProp");
		assertEquals(1, escapeProp.size());
		assertTrue("escapeProp should contain eq", escapeProp.containsKey("eq"));
		
		final Map<String, String> lowerCaseProp = filterObjectsSchema.get("FilterObject_testQuery_lowerCaseProp");
		assertEquals(1, lowerCaseProp.size());
		assertTrue("lowerCaseProp should contain eq", lowerCaseProp.containsKey("eq"));

		final Map<String, String> customFilterProp = filterObjectsSchema.get("FilterObject_testQuery_customFilterProp");
		assertEquals(1, customFilterProp.size());
		assertTrue("customFilterProp should contain eq", customFilterProp.containsKey("eq"));
		assertEquals("String", customFilterProp.get("eq"));

		final Map<String, String> customFilterScalarTypeProp = filterObjectsSchema.get("FilterObject_testQuery_customFilterScalarTypeProp");
		assertEquals(1, customFilterScalarTypeProp.size());
		assertTrue("customFilterScalarTypeProp should contain eq", customFilterScalarTypeProp.containsKey("eq"));
		assertEquals("UUID", customFilterScalarTypeProp.get("eq"));

		final Map<String, String> customFilterReferenceTypeProp = filterObjectsSchema.get("FilterObject_testQuery_customFilterReferenceTypeProp");
		assertEquals(1, customFilterReferenceTypeProp.size());
		assertTrue("customFilterReferenceTypeProp should contain eq", customFilterReferenceTypeProp.containsKey("eq"));
		assertEquals("FilterTypeOverride", customFilterReferenceTypeProp.get("eq"));

		final Map<String, String> queryProps = filterObjectsSchema.get("FilterObject_testQuery");
		assertEquals(16, queryProps.size());
		assertTrue("Query should contain FilterObject", queryProps.keySet().containsAll(
				Arrays.asList("_or", "_and", "_not", "stringProp", "numberProp", "booleanProp", "escapeProp", "lowerCaseProp",
						"beginsWithProp", "endsWithProp", "beginsWithEscapeProp", "endsWithEscapeProp",
						"customFilterProp", "customFilterScalarTypeProp", "customFilterReferenceTypeProp")));
	}
	
	@Test
	@Order(2)
	void testEqOperator() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("stringProp", GSON.fromJson(" { \"eq\" : \"test\" } ", Object.class));
		
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("stringProp = ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, values.size());
		assertEquals("test", values.get(0));
	}
	
	@Test
	@Order(3)
	void testLteOperator() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("numberProp", GSON.fromJson(" { \"lte\" : \"4\" } ", Object.class));
		
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("numberProp <= ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, values.size());
		assertEquals(4, Integer.parseInt(values.get(0).toString()));
	}
	
	@Test
	@Order(4)
	void testGteOperator() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("numberProp", GSON.fromJson(" { \"gte\" : \"4\" } ", Object.class));
		
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("numberProp >= ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, values.size());
		assertEquals(4, Integer.parseInt(values.get(0).toString()));
	}
	
	@SuppressWarnings("unchecked")
	@Test
	@Order(5)
	void testInOperatorForNumber() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("numberProp", GSON.fromJson(" { \"in\" : [\"4\", \"6\"] } ", Object.class));
		
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("numberProp IN ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, values.size());
		final ArrayList<String> resultList = (ArrayList<String>) values.get(0);
		assertEquals(4, Integer.parseInt(resultList.get(0)));
		assertEquals(6, Integer.parseInt(resultList.get(1)));
	}
	
	@SuppressWarnings("unchecked")
	@Test
	@Order(6)
	void testInOperatorForString() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("stringProp", GSON.fromJson(" { \"in\" : [\"DATABASE\", \"RULE\"] } ", Object.class));
		
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("stringProp IN ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, values.size());
		final ArrayList<String> resultList = (ArrayList<String>) values.get(0);
		assertEquals("DATABASE", resultList.get(0));
		assertEquals("RULE", resultList.get(1));
	}
	
	@Test
	@Order(7)
	void testMultipleOperator() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("numberProp", GSON.fromJson(" { \"gte\" : \"4\", \"lte\" : \"9\" } ", Object.class));
		
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("numberProp >= ? AND numberProp <= ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(2, values.size());
		assertEquals(4, Integer.parseInt(values.get(0).toString()));
		assertEquals(9, Integer.parseInt(values.get(1).toString()));
	}
	
	@Test
	@Order(8)
	void testTrueOperator() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("booleanProp", GSON.fromJson(" { \"is\" : \"true\" } ", Object.class));
		
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		assertEquals("booleanProp IS true", parseFilterObject.getLeft().getClause().toString());
		assertEquals(0, parseFilterObject.getRight().size());
	}
	
	@Test
	@Order(9)
	void testFalseOperator() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("booleanProp", GSON.fromJson(" { \"is\" : \"false\" } ", Object.class));
		
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		assertEquals("booleanProp IS NOT true", parseFilterObject.getLeft().getClause().toString());
		assertEquals(0, parseFilterObject.getRight().size());
	}
	
	@Test
	@Order(10)
	void testNoneOperator() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("noneProp", GSON.fromJson(" { \"is\" : null } ", Object.class));
		
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		assertEquals("noneProp IS Null", parseFilterObject.getLeft().getClause().toString());
		assertEquals(0, parseFilterObject.getRight().size()); /* assert no argument is passed for "is: null" */
	}
	
	@Test
	@Order(11)
	void testAndOperator() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("_and", GSON.fromJson(" [ { \"numberProp\" : { \"gte\" : \"4\" } }, { \"numberProp\" : { \"lte\" : \"7\" } } ] ", Object.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("numberProp >= ? AND numberProp <= ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(2, values.size());
		assertTrue("4 and 7 should be contained in values", values.containsAll(Arrays.asList("4", "7")));
	}
	
	@Test
	@Order(12)
	void testAndOperatorMultiField() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("_and", GSON.fromJson(" [ { \"numberProp\" : { \"gte\" : \"4\" } }, { \"booleanProp\" : { \"is\" : \"7\" } } ] ", Object.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("numberProp >= ? AND booleanProp IS NOT true", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, values.size());
		assertTrue("4 should be contained in values", values.containsAll(Arrays.asList("4")));
	}
	
	@Test
	@Order(13)
	void testOrOperator() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("_or", GSON.fromJson(" [ { \"stringProp\" : { \"eq\" : \"test\" } }, { \"stringProp\" : { \"eq\" : \"TEST\" } } ] ", Object.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("( stringProp = ? OR stringProp = ? )", parseFilterObject.getLeft().getClause().toString());
		assertEquals(2, values.size());
		assertTrue("test and TEST should be contained in values", values.containsAll(Arrays.asList("test", "TEST")));
	}
	
	@Test
	@Order(14)
	void testOrOperatorMultiField() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("_or", GSON.fromJson(" [ { \"stringProp\" : { \"eq\" : \"test\" } }, { \"numberProp\" : { \"gte\" : \"99\" } } ] ", Object.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("( stringProp = ? OR numberProp >= ? )", parseFilterObject.getLeft().getClause().toString());
		assertEquals(2, values.size());
		assertTrue("test and 99 should be contained in values", values.containsAll(Arrays.asList("test", "99")));
	}
	
	@Test
	@Order(15)
	void testNotOperator() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("_not", GSON.fromJson(" { \"numberProp\" : { \"gte\" : \"4\" } } ", Object.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		assertEquals("NOT (numberProp >= ?)", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, parseFilterObject.getRight().size());
		assertEquals(4, Integer.parseInt(parseFilterObject.getRight().get(0).toString()));
	}
	
	@Test
	@Order(16)
	void testLowerCaseFlag() {		
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("_or", GSON.fromJson(" [ { \"lowerCaseProp\" : { \"eq\" : \"test\" } }, { \"lowerCaseProp\" : { \"eq\" : \"TEST\" } },"
				+ " { \"lowerCaseProp\" : { \"eq\" : \"TesT\" } } ] ", Object.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("( lowerCaseProp = ? OR lowerCaseProp = ? OR lowerCaseProp = ? )", parseFilterObject.getLeft().getClause().toString());
		assertEquals(3, values.size());
		assertTrue("All values should be test (lower case)", values.stream().allMatch(r -> "test".equals(r)));
	}
	
	@Test
	@Order(17)
	void testEscapeLuceneFlag() {		
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("_or", GSON.fromJson(" [ { \"escapeProp\" : { \"eq\" : \"t~s?\" } }, { \"escapeProp\" : { \"eq\" : \"^ES*\" } } ] ", Object.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("( escapeProp = ? OR escapeProp = ? )", parseFilterObject.getLeft().getClause().toString());
		assertEquals(2, values.size());
		assertTrue("Special characters should be escaped in values", values.containsAll(Arrays.asList("t\\~s\\?", "\\^ES*")));
	}

	@Test
	@Order(18)
	void testBeginsWithFlag() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("beginsWithProp", GSON.fromJson(" { \"eq\" : \"test\" } ", Map.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("beginsWithProp = ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, values.size());
		assertTrue("All values should be test (lower case)", values.stream().allMatch("test%"::equals));
	}

	@Test
	@Order(19)
	void testEndsWithFlag() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("endsWithProp", GSON.fromJson(" { \"eq\" : \"test\" } ", Map.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("endsWithProp = ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, values.size());
		assertTrue("All values should be test (lower case)", values.stream().allMatch("%test"::equals));
	}

	@Test
	@Order(20)
	void testBeginsWithEscapeFlag() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("beginsWithEscapeProp", GSON.fromJson(" { \"eq\" : \"test\" } ", Map.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("beginsWithEscapeProp = ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, values.size());
		assertTrue("All values should be test (lower case)", values.stream().allMatch("test*"::equals));
	}

	@Test
	@Order(21)
	void testEndsWithEscapeFlag() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("endsWithEscapeProp", GSON.fromJson(" { \"eq\" : \"test\" } ", Map.class));
		assertNotNull(service);
		final Pair<OrientClause, List<Object>> parseFilterObject = service.parseFilterObject(PROJECT_ID.get(), QUERY_NAME, filterObject);
		final List<Object> values = parseFilterObject.getRight();
		assertEquals("endsWithEscapeProp = ?", parseFilterObject.getLeft().getClause().toString());
		assertEquals(1, values.size());
		assertTrue("All values should be test (lower case)", values.stream().allMatch("*test"::equals));
	}
	
	@Test
	@Order(22)
	void testSchemaCreationMigrated() {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(DATAPOINT_TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(DATAPOINT_TYPE_NAME, STRING_DATAPOINT).withFiltering(FILTER_QUERY_NAME, filtering -> {
				filtering.isTrue(TestFilterAndSorting.TestInquiryBuilder::isTrue);
				filtering.notIn(TestFilterAndSorting.TestInquiryBuilder::withNameNotIn);
				filtering.isFalse(TestFilterAndSorting.TestInquiryBuilder::isFalse);
				filtering.isAbsent(TestFilterAndSorting.TestInquiryBuilder::isAbsent);
			}).add();
			
			builder.extend(DATAPOINT_TYPE_NAME, LONG_DATAPOINT).withFiltering(FILTER_QUERY_NAME, filtering -> {
				filtering.in(TestFilterAndSorting.TestInquiryBuilder::withIds);
				filtering.gte(TestFilterAndSorting.TestInquiryBuilder::withIdAbove);
				filtering.lte(TestFilterAndSorting.TestInquiryBuilder::withIdBelow);
				filtering.eq(TestFilterAndSorting.TestInquiryBuilder::withId);
				filtering.notEq(TestFilterAndSorting.TestInquiryBuilder::withIdNotEqual);
			}).add();
			defineQueryOnBuilder(DATAPOINT_TYPE_NAME, FILTER_QUERY_NAME, builder);
		};
		service = new FilterObjectService(DataPointTestHelper.getRegistry(dataPointSource));
		final Map<String, Map<String, String>> filterObjectsSchema = service.buildFilterObjectsSchema(PROJECT_ID);
		assertTrue("Schema should contain FilterObject_testQuery", filterObjectsSchema.containsKey("FilterObject_testQuery"));
		assertTrue("Schema should contain FilterObject_testQuery_testStringProp", filterObjectsSchema.containsKey("FilterObject_testQuery_testStringProp"));
		assertTrue("Schema should contain FilterObject_testQuery_testLongProp", filterObjectsSchema.containsKey("FilterObject_testQuery_testLongProp"));
		
		final Map<String, String> numberProp = filterObjectsSchema.get("FilterObject_testQuery_testLongProp");
		MatcherAssert.assertThat("should contain all", numberProp.keySet(), Matchers.containsInAnyOrder("eq", "lte", "gte", "in", "notEq"));

		final Map<String, String> stringProp = filterObjectsSchema.get("FilterObject_testQuery_testStringProp");
		MatcherAssert.assertThat("should contain all", stringProp.keySet(), Matchers.containsInAnyOrder("is", "notIn"));
	}
	
	@Test
	@Disabled("We don't throw Exception anymore after adding postgres filter object")
	void testFilterNotSupported() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put(STRING_DATAPOINT, Collections.singletonMap("isAbsent", STRING_DATAPOINT));
		final String errorMsg =  "While applying filters on testStringProp: operator isAbsent is not supported";
		validateErrors(filterObject, errorMsg);
	}
	
	@Test
	@Disabled("We don't throw Exception anymore after adding postgres filter object")
	void testUnknownDataPoint() {
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put("testBooleanProp", Collections.singletonMap("isAbsent", STRING_DATAPOINT));
		final String errorMsg = "While applying filters: Unable to filter by testBooleanProp: unknown data point or data point is not filterable";
		validateErrors(filterObject, errorMsg);
	}
	
	@Order(23)
	@ParameterizedTest
	@ArgumentsSource(ApplyFilterObjectsArgumentsProvider.class)
	void testApplyFilterObject(final String datapoint, final Consumer<FilterBuilder> filterBuilder, 
			final Map<String, String> filterObjectValue, final String expectedOutput) {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(DATAPOINT_TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			builder.extend(DATAPOINT_TYPE_NAME, datapoint).withFiltering(FILTER_QUERY_NAME, filterBuilder).add();
			defineQueryOnBuilder(DATAPOINT_TYPE_NAME, FILTER_QUERY_NAME, builder);
		};
		service = new FilterObjectService(DataPointTestHelper.getRegistry(dataPointSource));
		final Map<String, Object> filterObject = new HashedMap<>();
		filterObject.put(datapoint, filterObjectValue);
		final TestInquiryBuilder inquiryBuilder = new TestFilterAndSorting().new TestInquiryBuilder();
		assertNotNull(service);
		service.applyFilterObject(PROJECT_ID.get(), FILTER_QUERY_NAME, filterObject, inquiryBuilder);
		assertEquals(expectedOutput, inquiryBuilder.getQueryFilter());
	}
	
	private void defineQueryOnBuilder(final String type, final String QUERY_NAME, final MiningDataPointBuilder builder) {
		builder.defineQuery(QUERY_NAME)
		.type(type)
		.withRequiredParameter("projectId", ScalarType.LONG)
		.withOptionalParameter("filterObject", "Map_String_java_lang_Object")
		.add();
	}
	
	private void validateErrors(final Map<String, Object> filterObject, final String errorMsg) {
		final MiningDataPointSource dataPointSource = (builder) -> {
			builder.defineType(DATAPOINT_TYPE_NAME).representedBy(TestFilterAndSorting.class).withDefaultProperties().add();
			defineQueryOnBuilder(DATAPOINT_TYPE_NAME, FILTER_QUERY_NAME, builder);
		};
		service = new FilterObjectService(DataPointTestHelper.getRegistry(dataPointSource));
		final TestInquiryBuilder inquiryBuilder = new TestFilterAndSorting().new TestInquiryBuilder();
		final FilterObjectService serviceObj = Assert.assertNotNull(service);
		final Throwable exception = assertThrows(IllegalArgumentException.class, () -> {
			serviceObj.applyFilterObject(1L, FILTER_QUERY_NAME, filterObject, inquiryBuilder);
		});
		assertEquals(errorMsg, exception.getMessage());
	}
}
