/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

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
import innowake.mining.shared.model.AnnotationFieldName;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.test.util.RestTemplateUtil;

/**
 * Integration tests for the getAggregatedValues of annotations.
 */
class AnnotationAggregationTest extends IntegrationTest {

	private static final Long ONE = Long.valueOf(1);
	private static final String AGGREGATIONS_URL = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/annotations/aggregations";
	private final RestTemplate restTemplate = new RestTemplate();
	private final ConnectionInfo info = getConnectionInfo();
	private final ParameterizedTypeReference<List<AggregationResult<AnnotationFieldName>>> responseType = 
			new ParameterizedTypeReference<List<AggregationResult<AnnotationFieldName>>>() { };

	/**
	 * Test for a non existing project.
	 */
	@Test
	void testPostWrongProject() {
		final AggregationRequest<AnnotationFieldName> aggregationRequest = new AggregationRequest<>();
		final HttpEntity<AggregationRequest<AnnotationFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
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
	 * Tests filtering and ordering for annotation aggregated values.
	 */
	@Test
	void testPostAggregatedValuesFiltereAndOrdered() {
		final Set<AnnotationFieldName> groupBy = new HashSet<>();
		groupBy.add(AnnotationFieldName.CATEGORY);
		groupBy.add(AnnotationFieldName.CREATED_BY_USER_ID);
		final List<AggregationResult<AnnotationFieldName>> listAggregationResult =
				generateAggregationResult(groupBy, Map.of(AnnotationFieldName.CREATED_BY_USER_ID, Map.of(FilterOperators.OPERATOR_IN, List.of("admin"))),
						Collections.singletonMap(AnnotationFieldName.ID, AggregationOperator.COUNT), Collections.singletonList(AnnotationFieldName.CATEGORY));
		assertNotNull(listAggregationResult);
		assertFalse("List of aggregation results must not be empty", listAggregationResult.isEmpty());
		listAggregationResult.forEach(result -> assertEquals("admin", result.getGroup().get(AnnotationFieldName.CREATED_BY_USER_ID)));
		final List<String> types = listAggregationResult.stream().map(
				aggregation -> aggregation.getGroup().get(AnnotationFieldName.CATEGORY).toString()).collect(Collectors.toList());
		for (int i = 1; i < types.size(); i++) {
			assertTrue(types.get(i - 1).compareTo(types.get(i)) <= 0);
		}
	}

	/**
	 * Tests only aggregated values for annotation.
	 */
	@Test
	void testPostAggregatedValuesFieldsOnly() {
		final Map<AnnotationFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(AnnotationFieldName.ID, AggregationOperator.COUNT);
		fields.put(AnnotationFieldName.PROJECT_ID, AggregationOperator.COUNT);
		final List<AggregationResult<AnnotationFieldName>> listAggregationResult =
				generateAggregationResult(null, null, fields, null);
		assertNotNull(listAggregationResult);
		assertEquals(ONE.intValue(), listAggregationResult.size());
		assertNotNull(listAggregationResult.get(0).getFields().get(AnnotationFieldName.ID));
		assertNotNull(listAggregationResult.get(0).getFields().get(AnnotationFieldName.PROJECT_ID));
	}

	/**
	 * Tests only group by clause for annotation aggregation.
	 */
	@Test
	void testPostAggregatedValuesGroupByOnly() {
		final Set<AnnotationFieldName> groupBy = new HashSet<>();
		groupBy.add(AnnotationFieldName.CATEGORY);
		groupBy.add(AnnotationFieldName.CREATED_BY_USER_ID);
		final List<AggregationResult<AnnotationFieldName>> listAggregationResult = 
				generateAggregationResult(groupBy, null, null, null);
		assertNotNull(listAggregationResult);
		assertFalse("List of aggregation results must not be empty", listAggregationResult.isEmpty());
		listAggregationResult.forEach(result -> {
			assertNotNull(result.getGroup().get(AnnotationFieldName.CATEGORY));
			assertNotNull(result.getGroup().get(AnnotationFieldName.CREATED_BY_USER_ID));
		});
	}

	/**
	 * Tests aggregation on multi-value (list) fields.
	 */
	@Test
	void testPostAggregatedMultiValues() {
		final List<AggregationResult<AnnotationFieldName>> listAggregationResult = 
				generateAggregationResult(Collections.singleton(AnnotationFieldName.METADATA),
						null, Collections.singletonMap(AnnotationFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertFalse("List of aggregation results must not be empty", listAggregationResult.isEmpty());
		final HashMap<Object, Object> aggregationResult = new HashMap<>();
		listAggregationResult.forEach(result -> {
			assertFalse("Eeach group should appear only once in the result", aggregationResult.containsKey(result.getGroup().get(AnnotationFieldName.METADATA)));
			aggregationResult.put(result.getGroup().get(AnnotationFieldName.METADATA), result.getFields().get(AnnotationFieldName.ID));
		});
		assertEquals(5, aggregationResult.size());
		assertEquals(1, aggregationResult.get("IF_ELSE_CONDITION"));
		assertEquals(2, aggregationResult.get("LOOP_CONDITION"));
		assertEquals(2, aggregationResult.get("OTHER_CONDITION"));
		assertEquals(1, aggregationResult.get("SELECT_CONDITION"));
	}

	/**
	 * Tests aggregation on value fields By Id And GroupBy as Category.
	 */
	@Test
	void testAggregatedValuesFieldByIdAndGroupByCategory() {
		final List<AggregationResult<AnnotationFieldName>> listAggregationResult =
				generateAggregationResult(Collections.singleton(AnnotationFieldName.CATEGORY),
						null, Collections.singletonMap(AnnotationFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(3, listAggregationResult.size());
		final HashMap<Object, Object> aggregationResult = new HashMap<>();
		listAggregationResult.forEach(result -> {
			assertFalse("Each group should appear only once in the result", aggregationResult.containsKey(result.getGroup().get(AnnotationFieldName.CATEGORY)));
			aggregationResult.put(result.getGroup().get(AnnotationFieldName.CATEGORY), result.getFields().get(AnnotationFieldName.ID));
		});
		assertEquals(4, aggregationResult.get("Annotation Category A"));
		assertEquals(4, aggregationResult.get("Annotation Category B"));
		assertEquals(1, aggregationResult.get("Annotation Category C"));
	}

	/**
	 * Tests aggregation on value fields By Id And GroupBy as State.
	 */
	@Test
	void testAggregatedValuesFieldByIdAndGroupByState() {
		final List<AggregationResult<AnnotationFieldName>> listAggregationResult =
				generateAggregationResult(Collections.singleton(AnnotationFieldName.STATE),
						null, Collections.singletonMap(AnnotationFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(3, listAggregationResult.size());
		final HashMap<Object, Object> aggregationResult = new HashMap<>();
		listAggregationResult.forEach(result -> {
			assertFalse("Each group should appear only once in the result", aggregationResult.containsKey(result.getGroup().get(AnnotationFieldName.STATE)));
			aggregationResult.put(result.getGroup().get(AnnotationFieldName.STATE), result.getFields().get(AnnotationFieldName.ID));
		});
		assertEquals(7, aggregationResult.get("CANDIDATE"));
		assertEquals(1, aggregationResult.get("IN_ANALYSIS"));
		assertEquals(1, aggregationResult.get("REJECTED"));
	}

	/**
	 * Tests aggregation on value fields By Id And GroupBy as Type.
	 */
	@Test
	void testAggregatedValuesFieldByIdAndGroupByType() {
		final List<AggregationResult<AnnotationFieldName>> listAggregationResult = 
				generateAggregationResult(Collections.singleton(AnnotationFieldName.TYPE), null,
						Collections.singletonMap(AnnotationFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(2, listAggregationResult.size());
		final HashMap<Object, Object> aggregationResult = new HashMap<>();
		listAggregationResult.forEach(result -> {
			assertFalse("Each group should appear only once in the result", aggregationResult.containsKey(result.getGroup().get(AnnotationFieldName.TYPE)));
			aggregationResult.put(result.getGroup().get(AnnotationFieldName.TYPE), result.getFields().get(AnnotationFieldName.ID));
		});
		assertEquals(6, aggregationResult.get("RULE"));
		assertEquals(3, aggregationResult.get("DATABASE"));
	}

	/**
	 * Tests aggregation on value fields By Id And GroupBy as Category and Type and Filter By statelink.name.
	 */
	@Test
	void testAggregatedValuesFieldByIdAndGroupByTypeAndCategory() {
		final Set<AnnotationFieldName> groupBy = new HashSet<>();
		groupBy.add(AnnotationFieldName.TYPE);
		groupBy.add(AnnotationFieldName.CATEGORY);
		final List<AggregationResult<AnnotationFieldName>> listAggregationResult =
				generateAggregationResult(groupBy, Map.of(AnnotationFieldName.STATE, Map.of(FilterOperators.OPERATOR_EQ, "CANDIDATE")),
						Collections.singletonMap(AnnotationFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(2, listAggregationResult.size());
		final HashMap<HashMap<Object, Object>, Object> aggregationResultMap = new HashMap<>();
		listAggregationResult.forEach(result -> {
			HashMap<Object, Object> aggregationResult = new HashMap<>();
			aggregationResult.put(result.getGroup().get(AnnotationFieldName.TYPE), result.getGroup().get(AnnotationFieldName.CATEGORY));
			aggregationResultMap.put(aggregationResult, result.getFields().get(AnnotationFieldName.ID));
		});
		assertEquals(3, aggregationResultMap.get(Collections.singletonMap("DATABASE", "Annotation Category B")));
		assertEquals(4, aggregationResultMap.get(Collections.singletonMap("RULE", "Annotation Category A")));
	}
	
	/**
	 * Return Aggregation List value based on groupBy, Filter, filterGroup and orderBy on AnnotationFieldName.
	 */
	private List<AggregationResult<AnnotationFieldName>> generateAggregationResult(
			@Nullable final Set<AnnotationFieldName> groupBy,
			@Nullable final Map<AnnotationFieldName, Map<String, Object>> filters,
			@Nullable final Map<AnnotationFieldName, AggregationOperator> fields, @Nullable final List<AnnotationFieldName> orderBy) {
		AggregationRequest<AnnotationFieldName> aggregationRequest = new AggregationRequest<>();
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
		final HttpEntity<AggregationRequest<AnnotationFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<AnnotationFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + AGGREGATIONS_URL, HttpMethod.POST, request, responseType, ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(200, clientOneResponseEntity.getStatusCode().value());
		final List<AggregationResult<AnnotationFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		return listAggregationResult;
	}
}
