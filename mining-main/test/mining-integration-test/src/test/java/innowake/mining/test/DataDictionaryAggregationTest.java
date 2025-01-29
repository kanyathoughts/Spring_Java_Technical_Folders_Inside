/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.model.DataDictionaryFieldName;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.test.util.RestTemplateUtil;

/**
 * Integration tests for the getAggregatedValues of data dictionary.
 */
class DataDictionaryAggregationTest extends IntegrationTest {

	private static final Long ONE = Long.valueOf(1);
	private static final String AGGREGATIONS_URL = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/data-dictionary/aggregations";
	private final RestTemplate restTemplate = new RestTemplate();
	private final ConnectionInfo info = getConnectionInfo();
	private final ParameterizedTypeReference<List<AggregationResult<DataDictionaryFieldName>>> responseType =
			new ParameterizedTypeReference<List<AggregationResult<DataDictionaryFieldName>>>() { };

	/**
	 * Test for a non existing project.
	 */
	@Test
	void testPostWrongProject() {
		final AggregationRequest<DataDictionaryFieldName> aggregationRequest = new AggregationRequest<>();
		final HttpEntity<AggregationRequest<DataDictionaryFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		try {
			restTemplate.exchange(
					info.getUrl() + AGGREGATIONS_URL,
					HttpMethod.POST,
					request,
					responseType,
					Long.valueOf(10));
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND.value(), e.getRawStatusCode());
		}
	}

	/**
	 * Tests filtering and ordering for data dictionary aggregated values.
	 */
	@Test
	void testPostAggregatedValuesFiltereAndOrdered() {
		final Set<DataDictionaryFieldName> groupBy = new HashSet<>();
		groupBy.add(DataDictionaryFieldName.DATA_ELEMENT_NAME);
		groupBy.add(DataDictionaryFieldName.CREATED_BY_USER_ID);
		final List<AggregationResult<DataDictionaryFieldName>> listAggregationResult =
				generateAggregationResult(groupBy, Map.of(DataDictionaryFieldName.CREATED_BY_USER_ID, Map.of(FilterOperators.OPERATOR_EQ, "admin")),
						Collections.singletonMap(DataDictionaryFieldName.ID, AggregationOperator.COUNT),
						Collections.singletonList(DataDictionaryFieldName.DATA_ELEMENT_NAME));
		assertNotNull(listAggregationResult);
		assertFalse("List of aggregation results must not be empty", listAggregationResult.isEmpty());
		listAggregationResult.forEach(result -> assertEquals("admin", result.getGroup().get(DataDictionaryFieldName.CREATED_BY_USER_ID)));
		final List<String> dataElements = listAggregationResult.stream().map(
				aggregation -> aggregation.getGroup().get(DataDictionaryFieldName.DATA_ELEMENT_NAME).toString()).collect(Collectors.toList());
		assertFalse(dataElements.isEmpty());
		/* Tests that resulting list is in increasing order of Data Element Names */
		for (int i = 1; i < dataElements.size(); i++) {
			assertTrue(dataElements.get(i - 1).compareTo(dataElements.get(i)) <= 0);
		}
	}

	/**
	 * Tests only aggregated values for data dictionary.
	 *
	 */
	@Test
	void testPostAggregatedValuesFieldsOnly() {
		final Map<DataDictionaryFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(DataDictionaryFieldName.ID, AggregationOperator.COUNT);
		fields.put(DataDictionaryFieldName.LENGTH, AggregationOperator.SUM);
		final List<AggregationResult<DataDictionaryFieldName>> listAggregationResult =
				generateAggregationResult(null, null, fields, null);
		assertNotNull(listAggregationResult);
		assertFalse("List of aggregation results must not be empty", listAggregationResult.isEmpty());
		assertNotNull(listAggregationResult.get(0).getFields().get(DataDictionaryFieldName.ID));
		assertNotNull(listAggregationResult.get(0).getFields().get(DataDictionaryFieldName.LENGTH));
	}

	/**
	 * Tests only group by clause for data dictionary aggregation.
	 */
	@Test
	void testPostAggregatedValuesGroupByOnly() {
		final Set<DataDictionaryFieldName> groupBy = new HashSet<>();
		groupBy.add(DataDictionaryFieldName.ID);
		groupBy.add(DataDictionaryFieldName.DATA_ELEMENT_NAME);
		groupBy.add(DataDictionaryFieldName.CREATED_BY_USER_ID);
		final List<AggregationResult<DataDictionaryFieldName>> listAggregationResult =
				generateAggregationResult(groupBy, null, null, null);
		assertNotNull(listAggregationResult);
		assertFalse("List of aggregation results must not be empty", listAggregationResult.isEmpty());
		listAggregationResult.forEach(result -> {
			assertNotNull(result.getGroup().get(DataDictionaryFieldName.ID));
			assertNotNull(result.getGroup().get(DataDictionaryFieldName.DATA_ELEMENT_NAME));
			assertNotNull(result.getGroup().get(DataDictionaryFieldName.CREATED_BY_USER_ID));
		});
	}

	/**
	 * Tests aggregation on value fields by Id and filter by isCandidate == true
	 */
	@Test
	void testPostAggregatedValuesFieldsByIdAndFilterByIsCandidateTrue() {
		final List<AggregationResult<DataDictionaryFieldName>> listAggregationResult =
				generateAggregationResult(null, Map.of(DataDictionaryFieldName.IS_CANDIDATE, Map.of(FilterOperators.OPERATOR_IS_TRUE, true)),
						Collections.singletonMap(DataDictionaryFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertFalse("List of aggregation results must not be empty", listAggregationResult.isEmpty());
		assertEquals(1, listAggregationResult.size());
		assertEquals(0, listAggregationResult.get(0).getFields().get(DataDictionaryFieldName.ID));
	}

	/**
	 * Tests aggregation on value GroupBy as format and field by Id
	 **/
	@Test
	void testPostAggregatedValuesFieldsByIdAndGroupByFormat() {
		final List<AggregationResult<DataDictionaryFieldName>> listAggregationResult =
				generateAggregationResult(Collections.singleton(DataDictionaryFieldName.FORMAT), null,
						Collections.singletonMap(DataDictionaryFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertFalse("List of aggregation results must not be empty", listAggregationResult.isEmpty());
		assertEquals(3, listAggregationResult.size());
		final HashMap<Object, Object> aggregationResult = new HashMap<>();
		listAggregationResult.forEach(result -> {
			assertFalse("Each group should appear only once in the result", aggregationResult.containsKey(result.getGroup().get(DataDictionaryFieldName.FORMAT)));
			aggregationResult.put(result.getGroup().get(DataDictionaryFieldName.FORMAT), result.getFields().get(DataDictionaryFieldName.ID));
		});
		assertEquals(1, aggregationResult.get("PICX"));
		assertEquals(1, aggregationResult.get("GROUP"));
		assertEquals(1, aggregationResult.get("PIC9"));
	}
	/**
	 * Tests aggregation with Defined Location aggregation
	 **/
	@Test
	void testPostAggregatedValuesForDefinedLocation() {
		final int updateCount = getDataSource().update("UPDATE data_dictionary "
				+ "SET defined_location = 'PROGRAM' "
				+ "WHERE nid = 1");
		assertEquals(1, updateCount);

		final var groupBy = Collections.singleton(DataDictionaryFieldName.DEFINED_LOCATION);
		final var fields = Collections.singletonMap(DataDictionaryFieldName.ID, AggregationOperator.COUNT);
		final List<AggregationResult<DataDictionaryFieldName>> listAggregationResult = generateAggregationResult(groupBy, null, fields, null);
		assertNotNull(listAggregationResult);
		assertFalse("List of aggregation results must not be empty", listAggregationResult.isEmpty());
		assertEquals(2, listAggregationResult.size());
		final AggregationResult<DataDictionaryFieldName> programResult = listAggregationResult.stream().filter(r -> "PROGRAM".equals(r.getGroup().get(DataDictionaryFieldName.DEFINED_LOCATION)))
				.findFirst()
				.get();
		assertEquals("PROGRAM", programResult.getGroup().get(DataDictionaryFieldName.DEFINED_LOCATION));
		assertEquals(1, programResult.getFields().get(DataDictionaryFieldName.ID));
		final AggregationResult<DataDictionaryFieldName> nullResult = listAggregationResult.stream().filter(r -> r.getGroup().get(DataDictionaryFieldName.DEFINED_LOCATION) == null)
				.findFirst()
				.get();
		assertEquals(2, nullResult.getFields().get(DataDictionaryFieldName.ID));
		assertEquals(null, nullResult.getGroup().get(DataDictionaryFieldName.DEFINED_LOCATION));
	}

	/**
	 * Return Aggregation List value based on groupBy, Filter, filterGroup and orderBy on DataDictionaryFieldName.
	 */
	private List<AggregationResult<DataDictionaryFieldName>> generateAggregationResult(@Nullable final Set<DataDictionaryFieldName> groupBy,
			@Nullable final Map<DataDictionaryFieldName, Map<String, Object>> filters,
			@Nullable final Map<DataDictionaryFieldName, AggregationOperator> fields, @Nullable final List<DataDictionaryFieldName> orderBy) {
		final AggregationRequest<DataDictionaryFieldName> aggregationRequest = new AggregationRequest<>();
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
		final HttpEntity<AggregationRequest<DataDictionaryFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<DataDictionaryFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + AGGREGATIONS_URL, HttpMethod.POST, request, responseType, ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(200, clientOneResponseEntity.getStatusCode().value());
		final List<AggregationResult<DataDictionaryFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		return listAggregationResult;
	}
}
