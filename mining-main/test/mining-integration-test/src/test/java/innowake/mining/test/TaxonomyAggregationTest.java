/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.client.service.taxonomy.TaxonomyServiceProvider;
import innowake.mining.client.service.taxonomytype.TaxonomyTypeServiceProvider;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.model.TaxonomyFieldName;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.shared.model.taxonomy.assignment.AssignmentState;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;
import innowake.mining.test.util.RestTemplateUtil;

/**
 * Integration tests for the getAggregatedValues of the TaxonomyController
 */
class TaxonomyAggregationTest extends IntegrationTest {
	
	private static final Long ONE = Long.valueOf(1);
	private static final Long MODULE_2000 = Long.valueOf(2000); /* that's our favorite module */
	
	private static final String TAXONOMY_AGGREGATIONS_URL = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/taxonomies/aggregations";
	private static final String TAXONOMY_SLOC_URL = TAXONOMY_AGGREGATIONS_URL +"/sloc-by-type";
	
	private final RestTemplate restTemplate = new RestTemplate();
	private final ConnectionInfo info = getConnectionInfo();
	private final ParameterizedTypeReference<List<AggregationResult<TaxonomyFieldName>>> responseType =
			new ParameterizedTypeReference<List<AggregationResult<TaxonomyFieldName>>>() { };

	private final TaxonomyServiceProvider taxonomyServiceProvider = MiningApiClient.taxonomyService(getConnectionInfo());
	private final TaxonomyTypeServiceProvider taxonomyTypeServiceProvider = MiningApiClient.taxonomyTypeService(getConnectionInfo());
	
	/**
	 * Test for a non existing project.
	 */
	@Test
	void testPostWrongProject() {
		final AggregationRequest<TaxonomyFieldName> aggregationRequest = new AggregationRequest<TaxonomyFieldName>();
		final HttpEntity<AggregationRequest<TaxonomyFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		try {
			restTemplate.exchange(
					info.getUrl() + TAXONOMY_AGGREGATIONS_URL,
					HttpMethod.POST,
					request,
					responseType,
					Long.valueOf(10));
		} catch(final HttpClientErrorException e) {
			assertEquals(404, e.getRawStatusCode());
			return;
		}
		fail("Expected HttpClientErrorException with a status code of 404.");
	}
	
	/**
	 * Tests filtering and ordering for Taxonomy aggregated values. 
	 */
	@Test
	void testPostAggregatedValuesFilteredAndOrdered() {
		final AggregationRequest<TaxonomyFieldName> aggregationRequest = new AggregationRequest<TaxonomyFieldName>();
		aggregationRequest.setFilterObject(Map.of(TaxonomyFieldName.CATEGORY_NAME, Map.of(FilterOperators.OPERATOR_IN, List.of("Business Taxonomies"))));
		final Set<TaxonomyFieldName> groupBy = new HashSet<>();
		groupBy.add(TaxonomyFieldName.NAME);
		groupBy.add(TaxonomyFieldName.CATEGORY_NAME);
		aggregationRequest.setGroupBy(groupBy);
		final ArrayList<TaxonomyFieldName> orderBy = new ArrayList<>();
		orderBy.add(TaxonomyFieldName.NAME);
		aggregationRequest.setOrderBy(orderBy);
		final Map<TaxonomyFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(TaxonomyFieldName.ID, AggregationOperator.COUNT);
		aggregationRequest.setFields(fields);
		final HttpEntity<AggregationRequest<TaxonomyFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<TaxonomyFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + TAXONOMY_AGGREGATIONS_URL,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(HttpStatus.OK, clientOneResponseEntity.getStatusCode());
		final List<AggregationResult<TaxonomyFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		assertEquals(3, listAggregationResult.size());
		listAggregationResult.forEach(result -> assertEquals("Business Taxonomies", result.getGroup().get(TaxonomyFieldName.CATEGORY_NAME)));
		final List<String> names = listAggregationResult.stream().map(
				aggregation -> aggregation.getGroup().get(TaxonomyFieldName.NAME).toString()).collect(Collectors.toList());
		/* Tests that resulting list is in increasing order of name. */
		for (int i = 1; i < names.size(); i++) {
			assertTrue(names.get(i-1).compareTo(names.get(i)) <= 0);
		}
	}

	/**
	 * Tests only aggregated values for Taxonomy.
	 */
	@Test
	void testPostAggregatedValuesFieldsOnly() {
		final AggregationRequest<TaxonomyFieldName> aggregationRequest = new AggregationRequest<TaxonomyFieldName>();
		final Map<TaxonomyFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(TaxonomyFieldName.ID, AggregationOperator.COUNT);
		fields.put(TaxonomyFieldName.MODULE_ID, AggregationOperator.LIST);
		aggregationRequest.setFields(fields);
		final HttpEntity<AggregationRequest<TaxonomyFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<TaxonomyFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + TAXONOMY_AGGREGATIONS_URL,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(HttpStatus.OK, clientOneResponseEntity.getStatusCode());
		final List<AggregationResult<TaxonomyFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		assertEquals(ONE.intValue(), listAggregationResult.size());
		assertNotNull(listAggregationResult.get(0).getFields().get(TaxonomyFieldName.ID));
		assertNotNull(listAggregationResult.get(0).getFields().get(TaxonomyFieldName.MODULE_ID));
	}

	/**
	 * Tests aggregated codeLines, complexityMcCabe, commentLines and deadCodeLines for Taxonomy
	 * @throws IOException thrown by assignTaxonomy
	 *
	 */
	@Test
	void testCodeMetrics() throws IOException {
		final TaxonomyPojo additionalTaxonomyInTheDataDomain = createTaxonomy("Employee domain 2", "DataDomain");
		assignTaxonomy(2017L, additionalTaxonomyInTheDataDomain);
		final AggregationRequest<TaxonomyFieldName> aggregationRequest = new AggregationRequest<TaxonomyFieldName>();
		final Map<TaxonomyFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(TaxonomyFieldName.MODULE_LINES_OF_CODE, AggregationOperator.SUM);
		fields.put(TaxonomyFieldName.MODULE_COMPLEXITY, AggregationOperator.SUM);
		fields.put(TaxonomyFieldName.MODULE_LINES_OF_DEAD_CODE, AggregationOperator.SUM);
		fields.put(TaxonomyFieldName.MODULE_LINES_OF_COMMENT, AggregationOperator.SUM);
		aggregationRequest.setFields(fields);
		final HttpEntity<AggregationRequest<TaxonomyFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<TaxonomyFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + TAXONOMY_AGGREGATIONS_URL,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(HttpStatus.OK, clientOneResponseEntity.getStatusCode());
		final List<AggregationResult<TaxonomyFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		assertEquals(ONE.intValue(), listAggregationResult.size());
		/* Test if the code metrics(codeLines, complexityMcCabe, commentLines and deadCodeLines) are coming as expected */
		assertEquals(107, listAggregationResult.get(0).getFields().get(TaxonomyFieldName.MODULE_LINES_OF_CODE));
		assertEquals(3, listAggregationResult.get(0).getFields().get(TaxonomyFieldName.MODULE_COMPLEXITY));
		assertEquals(46, listAggregationResult.get(0).getFields().get(TaxonomyFieldName.MODULE_LINES_OF_COMMENT));
		assertNull(listAggregationResult.get(0).getFields().get(TaxonomyFieldName.MODULE_LINES_OF_DEAD_CODE));
	}

	/**
	 * Tests Module linesOfCode for Taxonomy Group by TypeName and Order by Module LinesOfCode when representation of Module is 'PHYSICAL' .
	 * @throws IOException thrown by assignTaxonomy
	 */
	@Test
	void testLineOfCodeGroupByTypeNameOrderByModuleLinesOfCode() throws IOException {
		final TaxonomyPojo taxonomy = createTaxonomy("Employee domain 2", "DataDomain");
		assignTaxonomy(2017L, taxonomy);
		assignTaxonomy(2018L, taxonomy);
		assignTaxonomy(2019L, taxonomy);
		assignTaxonomy(2020L, taxonomy);
		assignTaxonomy(2021L, taxonomy);

		final AggregationRequest<TaxonomyFieldName> aggregationRequest = new AggregationRequest<TaxonomyFieldName>();
		aggregationRequest.setFilterObject(Map.of(TaxonomyFieldName.MODULE_REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL"),
				TaxonomyFieldName.MODULE_LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 0)));
		final Map<TaxonomyFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(TaxonomyFieldName.MODULE_LINES_OF_CODE, AggregationOperator.SUM);
		aggregationRequest.setFields(fields);
		final Set<TaxonomyFieldName> groupBy = new HashSet<>();
		groupBy.add(TaxonomyFieldName.TYPE_NAME);
		aggregationRequest.setGroupBy(groupBy);
		final List<TaxonomyFieldName> orderBy = new ArrayList<>();
		orderBy.add(TaxonomyFieldName.MODULE_LINES_OF_CODE);
		aggregationRequest.setOrderBy(orderBy);
		
		final HttpEntity<AggregationRequest<TaxonomyFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<TaxonomyFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + TAXONOMY_AGGREGATIONS_URL,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(HttpStatus.OK, clientOneResponseEntity.getStatusCode());
		
		final List<AggregationResult<TaxonomyFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		assertEquals(ONE.intValue(), listAggregationResult.size());
		
		/* Test if the MODULE_LINES_OF_CODE are coming as expected */
		assertEquals(taxonomy.getType().getName(), listAggregationResult.get(0).getGroup().get(TaxonomyFieldName.TYPE_NAME));
		assertEquals(535, listAggregationResult.get(0).getFields().get(TaxonomyFieldName.MODULE_LINES_OF_CODE));
	}
	
	/**
	 * Tests aggregated sum of line of code by Taxonomy type
	 * @throws IOException thrown by assignTaxonomy
	 */
	@Test
	void testGetAggregationsSloc() throws IOException {
		final TaxonomyPojo taxonomy1 = createTaxonomy("Employee domain 2", "DataDomain");
		assignTaxonomy(2017L, taxonomy1);
		assignTaxonomy(2018L, taxonomy1);
		assignTaxonomy(2022L, taxonomy1);
		
		final TaxonomyPojo taxonomy2 = createTaxonomy("Employee domain 1", "DataDomain");
		
		assignTaxonomy(2019L, taxonomy2);
		assignTaxonomy(2020L, taxonomy2);
		assignTaxonomy(2021L, taxonomy2);
		
		/*Module with ID 2021 has multiple Taxonomies of the same type - DataDomain assigned*/
		assignTaxonomy(2021L, taxonomy1);
		
		/* Module with LOC and representation is VIRTUAL*/
		assignTaxonomy(2022L, taxonomy2);
		
		final TaxonomyPojo taxonomy3 = createTaxonomy("Business domain 1", "BusinessSubsystem");
		assignTaxonomy(2018L, taxonomy3);
		assignTaxonomy(2020L, taxonomy3);
		
		/*Taxonomy type with 0 Modules assigned*/
		createTaxonomy("Business domain 1", "BusinessProcess");

		final ResponseEntity<Map<String, Long>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + TAXONOMY_SLOC_URL,
				HttpMethod.GET,
				new HttpEntity<>(RestTemplateUtil.getHttpHeaders(info)),
				new ParameterizedTypeReference<Map<String, Long>>() {},
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(HttpStatus.OK, clientOneResponseEntity.getStatusCode());
		
		final Map<String, Long> taxonomySlocMap = clientOneResponseEntity.getBody();
		assertNotNull(taxonomySlocMap);
		
		/* DataDomain taxonomies: Employee domain 1 and Employee domain 2 are assigned to modules having IDs (2017L, 2018L, 2019L, 2020L, 2021L, 2022L) and
		 * codeLines = 107 each, but module id 2022L has representation="VIRTUAL" so the SLOC value for modules with representation="PHYSICAL" 
		 * will be 107 * 5 = 535 */
		assertEquals(Long.valueOf(535), taxonomySlocMap.get("DataDomain"));
		
		/* DataDomain taxonomies - Business domain 1 is assigned to modules having IDs(2018L, 2020L) and codeLines = 107 each,
		 * SLOC value for modules will be 107 * 2 = 214 */
		assertEquals(Long.valueOf(214), taxonomySlocMap.get("BusinessSubsystem"));
		
		/* BusinessProcess will not present in taxonomySlocMap map as this Taxonomy type has 0 Modules assigned so aggregated sum of line of code is 0.*/
		assertNull(taxonomySlocMap.get("BusinessProcess"), "BusinessProcess will be null because it has 0 modules attached.");
	}
	
	/**
	 * Tests only group by clause for Taxonomy aggregation. 
	 */
	@Test
	void testPostAggregatedValuesGroupByOnly() {
		final AggregationRequest<TaxonomyFieldName> aggregationRequest = new AggregationRequest<TaxonomyFieldName>();
		final Set<TaxonomyFieldName> groupBy = new HashSet<>();
		groupBy.add(TaxonomyFieldName.NAME);
		groupBy.add(TaxonomyFieldName.TYPE_NAME);
		aggregationRequest.setGroupBy(groupBy);
		final HttpEntity<AggregationRequest<TaxonomyFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<TaxonomyFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + TAXONOMY_AGGREGATIONS_URL,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(HttpStatus.OK, clientOneResponseEntity.getStatusCode());
		final List<AggregationResult<TaxonomyFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		assertFalse("List of aggregation results must not be empty", listAggregationResult.isEmpty());
		listAggregationResult.forEach(result -> {
			assertNotNull(result.getGroup().get(TaxonomyFieldName.NAME));
			assertNotNull(result.getGroup().get(TaxonomyFieldName.TYPE_NAME));
		});
	}
	
	/**
	 * Tests only order by clause for Taxonomy aggregation. 
	 */
	@Test
	void testPostAggregatedValuesOrderByField() {
		final AggregationRequest<TaxonomyFieldName> aggregationRequest = new AggregationRequest<TaxonomyFieldName>();
		final Set<TaxonomyFieldName> groupBy = new HashSet<>();
		groupBy.add(TaxonomyFieldName.ID);
		aggregationRequest.setGroupBy(groupBy);
		final ArrayList<TaxonomyFieldName> orderBy = new ArrayList<>();
		orderBy.add(TaxonomyFieldName.ID);
		aggregationRequest.setOrderBy(orderBy);
		final HttpEntity<AggregationRequest<TaxonomyFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<TaxonomyFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + TAXONOMY_AGGREGATIONS_URL,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(HttpStatus.OK, clientOneResponseEntity.getStatusCode());
		final List<AggregationResult<TaxonomyFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		assertEquals(3, listAggregationResult.size());
		final List<String> ids = listAggregationResult.stream().map(
				aggregation -> aggregation.getGroup().get(TaxonomyFieldName.ID).toString()).collect(Collectors.toList());
		/* Tests that resulting list is in increasing order of IDs */
		for (int i = 1; i < ids.size(); i++) {
			assertTrue(ids.get(i-1).compareTo(ids.get(i)) <= 0);
		}
	}
	
	/**
	 * Tests counting number of Modules by TaxonomyTypePojo. 
	 * @throws IOException exception thrown by assignTaxonomy
	 */
	@Test
	void testPostAggregatedValuesCountModulesByTaxonomyType() throws IOException {
		final TaxonomyPojo additionalTaxonomyInTheDataDomain = createTaxonomy("Employee domain 2", "DataDomain");
		assignTaxonomy(MODULE_2000, additionalTaxonomyInTheDataDomain);

		final AggregationRequest<TaxonomyFieldName> aggregationRequest = new AggregationRequest<TaxonomyFieldName>();
		
		final Set<TaxonomyFieldName> groupBy = new HashSet<>();
		groupBy.add(TaxonomyFieldName.TYPE_NAME);
		aggregationRequest.setGroupBy(groupBy);
		
		final Map<TaxonomyFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(TaxonomyFieldName.MODULE_ID, AggregationOperator.COUNT);
		aggregationRequest.setFields(fields);
		
		final ArrayList<TaxonomyFieldName> orderBy = new ArrayList<>();
		orderBy.add(TaxonomyFieldName.TYPE_NAME);
		aggregationRequest.setOrderBy(orderBy);
		
		final HttpEntity<AggregationRequest<TaxonomyFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<TaxonomyFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + TAXONOMY_AGGREGATIONS_URL,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(HttpStatus.OK, clientOneResponseEntity.getStatusCode());
		
		final List<AggregationResult<TaxonomyFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		assertEquals(3, listAggregationResult.size()); /* expect 3 taxonomy types: "BusinessProcess", "BusinessSubsystem", "DataDomain" */
		assertEquals(0, listAggregationResult.get(0).getFields().get(TaxonomyFieldName.MODULE_ID)); /* 0 Modules have "BusinessProcess" taxonomies */
		assertEquals(1, listAggregationResult.get(1).getFields().get(TaxonomyFieldName.MODULE_ID)); /* 1 Module has "BusinessSubsystem" taxonomies */
		assertEquals(3, listAggregationResult.get(2).getFields().get(TaxonomyFieldName.MODULE_ID)); /* 3 Modules have "DataDomain" taxonomies */
		/* Note: Module 2000 has two Taxonomies from the "DataDomain" Type so it is counted twice here */
	}

	/**
	 * Tests counting DISTINCT number of Modules by TaxonomyTypePojo. 
	 * @throws IOException thrown by assignTaxonomy
	 */
	@Test
	void testPostAggregatedValuesCountDistinctModulesByTaxonomyType() throws IOException {
		final TaxonomyPojo additionalTaxonomyInTheDataDomain = createTaxonomy("Employee domain 2", "DataDomain");
		assignTaxonomy(MODULE_2000, additionalTaxonomyInTheDataDomain);

		final AggregationRequest<TaxonomyFieldName> aggregationRequest = new AggregationRequest<TaxonomyFieldName>();
		
		final Set<TaxonomyFieldName> groupBy = new HashSet<>();
		groupBy.add(TaxonomyFieldName.TYPE_NAME);
		aggregationRequest.setGroupBy(groupBy);
		
		final Map<TaxonomyFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(TaxonomyFieldName.MODULE_ID, AggregationOperator.COUNT_DISTINCT);
		aggregationRequest.setFields(fields);
		
		final ArrayList<TaxonomyFieldName> orderBy = new ArrayList<>();
		orderBy.add(TaxonomyFieldName.TYPE_NAME);
		aggregationRequest.setOrderBy(orderBy);
		
		final HttpEntity<AggregationRequest<TaxonomyFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<TaxonomyFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + TAXONOMY_AGGREGATIONS_URL,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(HttpStatus.OK, clientOneResponseEntity.getStatusCode());
		
		final List<AggregationResult<TaxonomyFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		assertEquals(3, listAggregationResult.size()); /* expect 3 taxonomy types: "BusinessProcess", "BusinessSubsystem", "DataDomain" */
		assertEquals(0, listAggregationResult.get(0).getFields().get(TaxonomyFieldName.MODULE_ID)); /* 0 Modules have "BusinessProcess" taxonomies */
		assertEquals(1, listAggregationResult.get(1).getFields().get(TaxonomyFieldName.MODULE_ID)); /* 1 Module has "BusinessSubsystem" taxonomies */
		assertEquals(2, listAggregationResult.get(2).getFields().get(TaxonomyFieldName.MODULE_ID)); /* 3 Modules have "DataDomain" taxonomies */
		/* Note: Module 2000 has two Taxonomies from the "DataDomain" Type but is NOT counted twice here */
	}

	private TaxonomyPojo createTaxonomy(final String taxonomyName, final String taxonomyTypeName) throws IOException {
		final Optional<UUID> type = taxonomyTypeServiceProvider.findAllTaxonomyTypes()
			.setProjectId(ONE)
			.execute().getValue().map(Arrays::asList)
			.get().stream()
			.filter(q -> taxonomyTypeName.equals(q.getName()))
			.map(q -> q.getId())
			.findAny();
		
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setProject(EntityId.of(ONE))
				.setName(taxonomyName)
				.setType(type.get());
		final Result<TaxonomyPojo> result = taxonomyServiceProvider.createTaxonomy()
				.setProjectId(ONE).setTaxonomy(taxonomy).execute();
		return result.getValue().get();
	}

	private void assignTaxonomy(final Long moduleId, final TaxonomyPojo taxonomy) throws IOException {
		final ModuleMatcher moduleMatcher = new ModuleMatcher(Arrays.asList(EntityId.of(moduleId)), Collections.<String> emptyList());
		final TaxonomyAssignmentsSetRequest.TaxonomySetAssignment assignment = new TaxonomyAssignmentsSetRequest.TaxonomySetAssignment(taxonomy.identity(), AssignmentState.ALL);
		final TaxonomyAssignmentsSetRequest request = new TaxonomyAssignmentsSetRequest(moduleMatcher, Arrays.asList(assignment));
		Result<Void> result = taxonomyServiceProvider.updateTaxonomyAssignments().setProjectId(ONE).setRequest(request).execute();
		assertEquals(202, result.getStatusCode());
	}
	
	/**
	 * Tests aggregation on value fields By Module Id And GroupBy as Type name.
	 */
	@Test
	void testPostAggregatedValuesFieldByModuleIdAndGroupByTypeName() {
		final AggregationRequest<TaxonomyFieldName> aggregationRequest = new AggregationRequest<>();
		final Map<TaxonomyFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(TaxonomyFieldName.MODULE_ID, AggregationOperator.COUNT_DISTINCT);
		aggregationRequest.setFields(fields);
		aggregationRequest.setGroupBy(Collections.singleton(TaxonomyFieldName.TYPE_NAME));
		final HttpEntity<AggregationRequest<TaxonomyFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<TaxonomyFieldName>>> clientOneResponseEntity = restTemplate
				.exchange(info.getUrl() + TAXONOMY_AGGREGATIONS_URL, HttpMethod.POST, request, responseType, ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(HttpStatus.OK, clientOneResponseEntity.getStatusCode());
		final List<AggregationResult<TaxonomyFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		assertEquals(3, listAggregationResult.size());
		final HashMap<Object, Object> aggregationResult = new HashMap<>();
		listAggregationResult.forEach(result -> {
			assertFalse("Each group should appear only once in the result", aggregationResult.containsKey(result.getGroup().get(TaxonomyFieldName.TYPE_NAME)));
			aggregationResult.put(result.getGroup().get(TaxonomyFieldName.TYPE_NAME), result.getFields().get(TaxonomyFieldName.MODULE_ID));
		});
		assertEquals(2, aggregationResult.get("DataDomain"));
		assertEquals(0, aggregationResult.get("BusinessProcess"));
		assertEquals(1, aggregationResult.get("BusinessSubsystem"));
	}
}
