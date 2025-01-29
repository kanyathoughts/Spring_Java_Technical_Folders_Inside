/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.model.StatementFieldName;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.test.util.RestTemplateUtil;

/**
 * Integration tests for the getAggregatedValues of Statement and SqlStatement.
 */
class StatementAggregationTest extends IntegrationTest {

	private static final Long ONE = Long.valueOf(1);
	final RestTemplate restTemplate = new RestTemplate();
	final ConnectionInfo info = getConnectionInfo();
	final ParameterizedTypeReference<List<AggregationResult<StatementFieldName>>> responseType =
			new ParameterizedTypeReference<List<AggregationResult<StatementFieldName>>>() { };
	private static final String AGGREGATIONS_URL = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/statements/aggregations";

	/**
	 * Test for a non existing project.
	 */
	@Test
	void testPostWrongProject() {
		final AggregationRequest<StatementFieldName> aggregationRequest = new AggregationRequest<>();
		final HttpEntity<AggregationRequest<StatementFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		try {
			restTemplate.exchange(info.getUrl() + AGGREGATIONS_URL, HttpMethod.POST, request, responseType,
					Long.valueOf(10));
		} catch (final HttpClientErrorException e) {
			assertEquals(404, e.getRawStatusCode());
			return;
		}
		fail("Expected HttpClientErrorException with a status code of 404.");
	}

	/**
	 * Tests filtering and ordering for annotation aggregated values.
	 */
	@Test
	void testPostAggregatedValuesFiltereAndOrdered() {
		final Set<StatementFieldName> groupBy = new HashSet<>();
		groupBy.add(StatementFieldName.TECHNOLOGY);
		groupBy.add(StatementFieldName.TEXT);
		final List<AggregationResult<StatementFieldName>> listAggregationResult =
				generateAggregationResultForStatementField(groupBy, Map.of(StatementFieldName.TECHNOLOGY, Map.of(FilterOperators.OPERATOR_IN, List.of("COBOL"))),
						Collections.singletonMap(StatementFieldName.ID, AggregationOperator.COUNT), Collections.singletonList(StatementFieldName.TEXT));
		assertNotNull(listAggregationResult);
		assertFalse("Aggregation results must not be empty", listAggregationResult.isEmpty());
		listAggregationResult.forEach(result -> assertEquals("COBOL", result.getGroup().get(StatementFieldName.TECHNOLOGY)));
		final List<String> types = listAggregationResult.stream().map(aggregation -> aggregation.getGroup().get(StatementFieldName.TEXT).toString())
				.collect(Collectors.toList());
		for (int i = 1; i < types.size(); i++) {
			assertTrue(types.get(i - 1).compareTo(types.get(i)) <= 0);
		}
	}

	/**
	 * Tests only aggregated values for annotation.
	 */
	@Test
	void testPostAggregatedValuesFieldsOnly() {
		final Map<StatementFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(StatementFieldName.ID, AggregationOperator.COUNT);
		fields.put(StatementFieldName.PROJECT_ID, AggregationOperator.LIST);
		final List<AggregationResult<StatementFieldName>> listAggregationResult = generateAggregationResultForStatementField(null, null, fields, null);
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		assertNotNull(listAggregationResult.get(0).getFields().get(StatementFieldName.ID));
		assertNotNull(listAggregationResult.get(0).getFields().get(StatementFieldName.PROJECT_ID));
	}

	/**
	 * Tests only group by clause for annotation aggregation.
	 */
	@Test
	void testPostAggregatedValuesGroupByOnly() {
		final Set<StatementFieldName> groupBy = new HashSet<>();
		groupBy.add(StatementFieldName.TECHNOLOGY);
		groupBy.add(StatementFieldName.TEXT);
		final List<AggregationResult<StatementFieldName>> listAggregationResult = generateAggregationResultForStatementField(groupBy, null, null, null);
		assertNotNull(listAggregationResult);
		assertFalse("Aggregation results must not be empty", listAggregationResult.isEmpty());
		listAggregationResult.forEach(result -> {
			assertNotNull(result.getGroup().get(StatementFieldName.TECHNOLOGY));
			assertNotNull(result.getGroup().get(StatementFieldName.TEXT));
		});
	}

	/**
	 * Test for a non existing project.
	 */
	@Test
	void testPostSqlWrongProject() {
		final AggregationRequest<StatementFieldName> aggregationRequest = new AggregationRequest<>();
		final HttpEntity<AggregationRequest<StatementFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		try {
			restTemplate.exchange(info.getUrl() + 
					AGGREGATIONS_URL, HttpMethod.POST, request, responseType, Long.valueOf(10));
		} catch (final HttpClientErrorException e) {
			assertEquals(404, e.getRawStatusCode());
			return;
		}
		fail("Expected HttpClientErrorException with a status code of 404.");
	}

	/**
	 * Tests filtering and ordering for annotation aggregated values.
	 */
	@Test
	void testPostSqlAggregatedValuesFiltereAndOrdered() {
		final Set<StatementFieldName> groupBy = new HashSet<>();
		groupBy.add(StatementFieldName.TECHNOLOGY);
		groupBy.add(StatementFieldName.TEXT);
		final List<AggregationResult<StatementFieldName>> listAggregationResult =
				generateAggregationResultForStatementField(groupBy,
						Map.of(StatementFieldName.TECHNOLOGY, Map.of(FilterOperators.OPERATOR_IN, List.of("COBOL"))),
						Collections.singletonMap(StatementFieldName.ID, AggregationOperator.COUNT), Collections.singletonList(StatementFieldName.TEXT));
		assertNotNull(listAggregationResult);
		listAggregationResult.forEach(result -> assertEquals("COBOL", result.getGroup().get(StatementFieldName.TECHNOLOGY)));
		final List<String> types = listAggregationResult.stream().map(aggregation -> aggregation.getGroup().get(StatementFieldName.TEXT).toString())
				.collect(Collectors.toList());
		for (int i = 1; i < types.size(); i++) {
			assertTrue(types.get(i - 1).compareTo(types.get(i)) <= 0);
		}
	}

	/**
	 * Tests filtering for SQL aggregated values by taxonomy.
	 */
	@Test
	void testPostSqlAggregatedValuesFiltereByTaxonomy() {
		final List<AggregationResult<StatementFieldName>> listAggregationResult =
				generateAggregationResultForStatementField(Collections.singleton(StatementFieldName.STATEMENT_TYPE),
//						"moduleLink.$out.HasTaxonomy.id==1",
						null,
						Collections.singletonMap(StatementFieldName.ID, AggregationOperator.COUNT),
						Collections.singletonList(StatementFieldName.STATEMENT_TYPE));
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		final Integer count = (Integer) listAggregationResult.get(0).getFields().get(StatementFieldName.ID);
		assertEquals(2, count);
	}

	/**
	 * Tests only aggregated values for annotation.
	 */
	@Test
	void testPostSqlAggregatedValuesFieldsOnly() {
		final Map<StatementFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(StatementFieldName.ID, AggregationOperator.COUNT);
		fields.put(StatementFieldName.PROJECT_ID, AggregationOperator.LIST);
		final List<AggregationResult<StatementFieldName>> listAggregationResult =
				generateAggregationResultForStatementField(null, null, fields, null);
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		assertNotNull(listAggregationResult.get(0).getFields().get(StatementFieldName.ID));
		assertNotNull(listAggregationResult.get(0).getFields().get(StatementFieldName.PROJECT_ID));
	}

	/**
	 * Tests only group by clause for annotation aggregation.
	 */
	@Test
	void testPostSqlAggregatedValuesGroupByOnly() {
		final Set<StatementFieldName> groupBy = new HashSet<>();
		groupBy.add(StatementFieldName.TECHNOLOGY);
		groupBy.add(StatementFieldName.TEXT);
		final List<AggregationResult<StatementFieldName>> listAggregationResult =
				generateAggregationResultForStatementField(groupBy, null, null, null);
		assertNotNull(listAggregationResult);
		listAggregationResult.forEach(result -> {
			assertNotNull(result.getGroup().get(StatementFieldName.TECHNOLOGY));
			assertNotNull(result.getGroup().get(StatementFieldName.TEXT));
		});
	}
	
	/**
	 * Tests aggregation on value Field by Statement Type and GroupBy as Id
	 */
	@Test
	void testPostSqlAggregatedValuesFieldByIdAndGroupByStatementType() {
		final List<AggregationResult<StatementFieldName>> listAggregationResult =
				generateAggregationResultForStatementField(Collections.singleton(StatementFieldName.STATEMENT_TYPE), null,
						Collections.singletonMap(StatementFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		/* We have only two statements in modules of project 1 */
		assertEquals(2, listAggregationResult.get(0).getFields().get(StatementFieldName.ID));
	}

	/**
	 * Tests aggregation on value Field by Statement Type and Filter By statementTypeLink.name =in=("INSERT","UPDATE", "DELETE", "MERGE")
	 */
	@Test
	void testPostSqlAggregatedValuesFieldByIdAndGroupByStatementTypeAndFilterByStatementTypeLink() {
		final List<Map<StatementFieldName, Map<String, Object>>> filterlist = new ArrayList<>();
		filterlist.add(Map.of(StatementFieldName.STATEMENT_TYPE, Map.of(FilterOperators.OPERATOR_IN, List.of("INSERT", "UPDATE", "DELETE", "MxERGE"))));
		filterlist.add(Map.of(StatementFieldName.STATEMENT_TYPE, Map.of(FilterOperators.OPERATOR_IN, List.of("ALTER", "CREATE", "DROP", "GRANT"))));
		filterlist.forEach(filter -> {
			final List<AggregationResult<StatementFieldName>> listAggregationResult =
					generateAggregationResultForStatementField(Collections.singleton(StatementFieldName.STATEMENT_TYPE), filter,
							Collections.singletonMap(StatementFieldName.ID, AggregationOperator.COUNT), null);
			assertNotNull(listAggregationResult);
			assertEquals(0, listAggregationResult.size());
		});
	}

	/**
	 * Tests aggregation on value Field by Distinct Table and Filter By halsteadComplexity.
	 */
	@Test
	void testPostSqlAggregatedValuesFieldByDistinctTableAndFilterByHalsteadComplexity() {
		final List<Map<StatementFieldName, Map<String, Object>>> filterlist = new ArrayList<>();
		filterlist.add(Map.of(StatementFieldName.HALSTEAD_COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 1, FilterOperators.OPERATOR_LTE, 10)));
		filterlist.add(Map.of(StatementFieldName.HALSTEAD_COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 11, FilterOperators.OPERATOR_LTE, 20)));
		filterlist.add(Map.of(StatementFieldName.HALSTEAD_COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 21, FilterOperators.OPERATOR_LTE, 30)));
		filterlist.add(Map.of(StatementFieldName.HALSTEAD_COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 31)));
		filterlist.forEach(filter -> {
			final List<AggregationResult<StatementFieldName>> listAggregationResult =
					generateAggregationResultForStatementField(Collections.singleton(StatementFieldName.STATEMENT_TYPE), filter,
							Collections.singletonMap(StatementFieldName.DISTINCT_TABLES, AggregationOperator.AVG), null);
			assertNotNull(listAggregationResult);
			if (filter.get(StatementFieldName.HALSTEAD_COMPLEXITY).get(FilterOperators.OPERATOR_GTE).equals(1)
					&& filter.get(StatementFieldName.HALSTEAD_COMPLEXITY).get(FilterOperators.OPERATOR_LTE).equals(10)) {
				assertEquals(1, listAggregationResult.size());
				assertEquals(21.0, listAggregationResult.get(0).getFields().get(StatementFieldName.DISTINCT_TABLES));
			} else {
				assertEquals(0, listAggregationResult.size());
			}
		});
	}

	@Test
	void testPostSqlAggregatedValuesFieldByIdAndFilterByHalsteadComplexity() {
		final List<AggregationResult<StatementFieldName>> listAggregationResult =
				generateAggregationResultForStatementField(Collections.singleton(StatementFieldName.HALSTEAD_COMPLEXITY),
						Map.of(StatementFieldName.HALSTEAD_COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 1)),
						Collections.singletonMap(StatementFieldName.ID, AggregationOperator.COUNT),
						Collections.singletonList(StatementFieldName.HALSTEAD_COMPLEXITY));
		assertNotNull(listAggregationResult);
		assertTrue("Halstead Complexity should be greater than 0",
				(Double) listAggregationResult.get(0).getGroup().get(StatementFieldName.HALSTEAD_COMPLEXITY) > 0);
	}

	/**
	 * Tests aggregation on value GroupBy as Id And Filter By text Length .
	 */
	@Test
	void testPostSqlAggregatedValuesFilteredByTextLength() {
		final List<Map<String, Object>> textLengths = new ArrayList<>();
		/* _TEXT_LENGTH > 0 and _TEXT_LENGTH <= 400 */
		textLengths.add(Map.of(FilterOperators.OPERATOR_GTE, 1, FilterOperators.OPERATOR_LTE, 400));
		/* _TEXT_LENGTH > 400 and _TEXT_LENGTH <= 800 */
		textLengths.add(Map.of(FilterOperators.OPERATOR_GTE, 401, FilterOperators.OPERATOR_LTE, 800));
		/* _TEXT_LENGTH > 800 and _TEXT_LENGTH <= 1200 */
		textLengths.add(Map.of(FilterOperators.OPERATOR_GTE, 801, FilterOperators.OPERATOR_LTE, 1200));
		/* _TEXT_LENGTH > 1200 */
		textLengths.add(Map.of(FilterOperators.OPERATOR_GTE, 1201));
		/* _TEXT_LENGTH <= 1200 */
		textLengths.add(Map.of(FilterOperators.OPERATOR_LTE, 1200));

		for (int i = 0; i < textLengths.size(); i++) {
			final List<AggregationResult<StatementFieldName>> listAggregationResult =
					generateAggregationResultForStatementField(Collections.singleton(StatementFieldName.ID), 
																Map.of(StatementFieldName.TEXT_LENGTH, textLengths.get(i)),
																Collections.singletonMap(StatementFieldName.TEXT_LENGTH, AggregationOperator.SUM),
																null);
			if (i == 0 || i == 4) {
				assertEquals(2, listAggregationResult.size());
				listAggregationResult.forEach(result -> {
					assertTrue("Text Length Should be Greater than 0 and less than 400", (Integer) result.getFields().get(StatementFieldName.TEXT_LENGTH) > 0
							&& (Integer) result.getFields().get(StatementFieldName.TEXT_LENGTH) <= 400);
					assertEquals(14, result.getFields().get(StatementFieldName.TEXT_LENGTH));
				});
			} else {
				assertEquals(0, listAggregationResult.size());
			}
		}
	}
	
	/**
	 * Return Aggregation List value based on FilterGroup, groupBy, Filter, fields and orderBy on StatementFieldName.
	 */
	private List<AggregationResult<StatementFieldName>> generateAggregationResultForStatementField(@Nullable final Set<StatementFieldName> groupBy,
			@Nullable final Map<StatementFieldName, Map<String, Object>> filters,
			@Nullable final Map<StatementFieldName, AggregationOperator> fields, @Nullable final List<StatementFieldName> orderBy) {
		AggregationRequest<StatementFieldName> aggregationRequest = new AggregationRequest<>();
		if (groupBy != null) {
			aggregationRequest.setGroupBy(groupBy);
		}
		if (orderBy != null) {
			aggregationRequest.setOrderBy(orderBy);
		}
		if (fields != null) {
			aggregationRequest.setFields(fields);
		}
		if (filters != null) {
			aggregationRequest.setFilterObject(filters);
		}
		final HttpEntity<AggregationRequest<StatementFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<StatementFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + AGGREGATIONS_URL, HttpMethod.POST, request, responseType, ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(200, clientOneResponseEntity.getStatusCode().value());
		final List<AggregationResult<StatementFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		return listAggregationResult;
	}
}
