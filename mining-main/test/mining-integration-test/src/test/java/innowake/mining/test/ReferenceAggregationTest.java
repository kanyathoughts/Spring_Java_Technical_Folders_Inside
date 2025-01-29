/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.model.RelationshipFieldName;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.test.util.RestTemplateUtil;

/**
 * Integration tests for the getAggregatedValues of the ReferenceController
 */
class ReferenceAggregationTest extends IntegrationTest {
	
	private static final Long ONE = Long.valueOf(1);

	private static final String REFERENCE_AGGREGATION_URI = RouteConfiguration.API_BASE + "/v1/projects/{clientId}/references/aggregations";
	
	final RestTemplate restTemplate = new RestTemplate();
	final ConnectionInfo info = getConnectionInfo();
	final ParameterizedTypeReference<List<AggregationResult<RelationshipFieldName>>> responseType = new ParameterizedTypeReference<List<AggregationResult<RelationshipFieldName>>>() { };
	
	@Test
	void testPostWrongProject() {
		final AggregationRequest<RelationshipFieldName> aggregationRequest = new AggregationRequest<>();
		final HttpEntity<AggregationRequest<RelationshipFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		try {
			restTemplate.exchange(
					info.getUrl() + REFERENCE_AGGREGATION_URI,
					HttpMethod.POST,
					request,
					responseType,
					Long.valueOf(10));
		} catch(final HttpClientErrorException e) {
			assertEquals(404, e.getRawStatusCode());
		}
	}
	
	@Test
	void testPostAggregatedValuesCountAll() {
		final AggregationRequest<RelationshipFieldName> aggregationRequest = new AggregationRequest<>();
		
		final Set<RelationshipFieldName> groupBy = new HashSet<>();
		groupBy.add(RelationshipFieldName.RELATIONSHIP);
		aggregationRequest.setGroupBy(groupBy);
		
		final Map<RelationshipFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(RelationshipFieldName.ID, AggregationOperator.COUNT);
		aggregationRequest.setFields(fields);
		
		final HttpEntity<AggregationRequest<RelationshipFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<RelationshipFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + REFERENCE_AGGREGATION_URI,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		
		assertNotNull(clientOneResponseEntity);
		assertEquals(200, clientOneResponseEntity.getStatusCode().value());
		
		final List<AggregationResult<RelationshipFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		
		/* We grouped by RELATIONSHIP type. With the current test data we expect the following groups:
		 * - "Calls" (19 References)
		 */
		
		assertEquals(1, listAggregationResult.size());
		final Map<String, Integer> groupsAndCounts = listAggregationResult.stream()
				.map(result -> ImmutablePair.of(result.getGroup().get(RelationshipFieldName.RELATIONSHIP).toString(), (Integer) result.getFields().get(RelationshipFieldName.ID)))
				.collect(Collectors.toMap(Pair::getLeft, Pair::getRight));
		assertEquals(new HashSet<>(Arrays.asList(
				RelationshipType.CALLS.name()
				)), groupsAndCounts.keySet());
		
		assertEquals(19, groupsAndCounts.get(RelationshipType.CALLS.name()));
	}
	
	@Test
	void testPostAggregatedValuesCountOnlyCalls() {
		final AggregationRequest<RelationshipFieldName> aggregationRequest = new AggregationRequest<>();
		
		final Set<RelationshipFieldName> groupBy = new HashSet<>();
		groupBy.add(RelationshipFieldName.RELATIONSHIP);
		aggregationRequest.setGroupBy(groupBy);
		
		final Map<RelationshipFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(RelationshipFieldName.ID, AggregationOperator.COUNT);
		aggregationRequest.setFields(fields);
		
		final HttpEntity<AggregationRequest<RelationshipFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<RelationshipFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				UriComponentsBuilder.fromHttpUrl(info.getUrl() + REFERENCE_AGGREGATION_URI)
					.queryParam("relationship", "CALLS")
					.build(ONE)
					.toString(),
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		
		assertNotNull(clientOneResponseEntity);
		assertEquals(200, clientOneResponseEntity.getStatusCode().value());
		
		final List<AggregationResult<RelationshipFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		
		/* We grouped by RELATIONSHIP type. But we only queried "Calls" edges. With the current test data we expect the following groups:
		 * - "Calls" (19 References)
		 */
		
		assertEquals(1, listAggregationResult.size());
		final Map<String, Integer> groupsAndCounts = listAggregationResult.stream()
				.map(result -> ImmutablePair.of(result.getGroup().get(RelationshipFieldName.RELATIONSHIP).toString(), (Integer) result.getFields().get(RelationshipFieldName.ID)))
				.collect(Collectors.toMap(Pair::getLeft, Pair::getRight));
		assertEquals(new HashSet<>(Arrays.asList(RelationshipType.CALLS.name())), groupsAndCounts.keySet());
		assertEquals(19, groupsAndCounts.get(RelationshipType.CALLS.name()));
	}
}
