/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.core.io.Resource;
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
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.test.util.RestTemplateUtil;

/**
 * Integration tests for the getAggregatedValues of the ModuleController
 */
class ModuleControllerAggregationTest extends IntegrationTest {

	private static final Long ONE = Long.valueOf(1);

	private static final String MODULE_AGGREGATION_URI = RouteConfiguration.API_BASE + "/v1/projects/{clientId}/modules/aggregations";
	private static final String MODULE_UTILITY_AGGREGATION_URI = RouteConfiguration.API_BASE + "/v1/projects/{clientId}/modules/aggregations";
	private static final String UTILITY_AGGREGATIONS_CSV_URL = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/modules/utility-aggregations/csv";
	private static final String TAXONOMY_RETRIEVAL_URI = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/taxonomies";
	private static final Path EXPECTED_FOLDER = Paths.get("./src/test/resources/innowake/mining/test/WMIN5212/utility");
	private static final String CONTENT_TYPE = "text/csv";
	private static final String FIRST_HEADER_NAME = "content-type";
	private static final String UTILITY_AGGREGATION_CSV_FILE_NAME = "UTILITIES.csv";
	private static final String CONTENT_UNMATCHED_MSG = "Actual content does not match expected";

	final RestTemplate restTemplate = new RestTemplate();
	final ConnectionInfo info = getConnectionInfo();
	final ParameterizedTypeReference<List<AggregationResult<ModuleFieldName>>> responseType = new
			ParameterizedTypeReference<List<AggregationResult<ModuleFieldName>>>() { };

	@Test
	void testPostWrongProject() {
		final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<>();
		final HttpEntity<AggregationRequest<ModuleFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		try {
			restTemplate.exchange(
					info.getUrl() + MODULE_AGGREGATION_URI,
					HttpMethod.POST,
					request,
					responseType,
					Long.valueOf(10));
		} catch (final HttpClientErrorException e) {
			assertEquals(404, e.getRawStatusCode());
		}
	}

	@Test
	void testPostAggregatedValuesFilteredAndOrdered() {
		final Set<ModuleFieldName> groupBy = new HashSet<>();
		groupBy.add(ModuleFieldName.TECHNOLOGY);
		groupBy.add(ModuleFieldName.TYPE);
		final List<AggregationResult<ModuleFieldName>> listAggregationResult =
				generateAggregationResult(groupBy, Map.of(ModuleFieldName.TECHNOLOGY, Map.of(FilterOperators.OPERATOR_IN, List.of("COBOL"))),
						Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT),
						Collections.singletonList(ModuleFieldName.TYPE));
		assertNotNull(listAggregationResult);
		assertEquals(2, listAggregationResult.size());
		listAggregationResult.forEach(result -> assertEquals("COBOL", result.getGroup().get(ModuleFieldName.TECHNOLOGY)));
		final List<String> types = listAggregationResult.stream().map(
				aggregation -> aggregation.getGroup().get(ModuleFieldName.TYPE).toString()).collect(Collectors.toList());
		for (int i = 1; i < types.size(); i++) {
			assertTrue(types.get(i - 1).compareTo(types.get(i)) <= 0);
		}
	}

	@Test
	void testPostAggregatedValuesFilteredByTaxonomy() {
		final Set<ModuleFieldName> groupBy = new HashSet<>();
		groupBy.add(ModuleFieldName.TECHNOLOGY);
		groupBy.add(ModuleFieldName.TYPE);
		final Map<ModuleFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(ModuleFieldName.ID, AggregationOperator.COUNT);
		fields.put(ModuleFieldName.LINES_OF_CODE, AggregationOperator.SUM);
		final ResponseEntity<List<TaxonomyPojo>> testTaxonomy = restTemplate.exchange(
				info.getUrl() + TAXONOMY_RETRIEVAL_URI,
				HttpMethod.GET,
				new HttpEntity<Void>(RestTemplateUtil.getHttpHeaders(info)),
				new ParameterizedTypeReference<>() {},
				ONE);
		assertNotNull(testTaxonomy.getBody());
		final TaxonomyPojo testTaxomomy = Objects.requireNonNull(testTaxonomy.getBody()).stream().filter(t -> t.getName().equals("Employee domain")).findFirst().orElseThrow();

		/* Test with taxonomy UUID filter */
		final List<AggregationResult<ModuleFieldName>> listAggregationResult =
				generateAggregationResult(groupBy,
						Map.of(ModuleFieldName.TAXONOMY_ID, Map.of(FilterOperators.OPERATOR_IN, List.of(testTaxomomy.getUid())),
								ModuleFieldName.IDENTIFICATION, Map.of(FilterOperators.OPERATOR_EQ, "IDENTIFIED")),
						fields, Collections.singletonList(ModuleFieldName.TECHNOLOGY));
		assertNotNull(listAggregationResult);
		assertEquals(2, listAggregationResult.size());
		final List<String> types = listAggregationResult.stream().map(
				aggregation -> aggregation.getGroup().get(ModuleFieldName.TYPE).toString()).collect(Collectors.toList());
		for (int i = 1; i < types.size(); i++) {
			assertTrue(types.get(i - 1).compareTo(types.get(i)) <= 0);
		}

		/* Test with taxonomy NID filter */
		final List<AggregationResult<ModuleFieldName>> listAggregationResult1 =
				generateAggregationResult(groupBy,
						Map.of(ModuleFieldName.TAXONOMY_ID, Map.of(FilterOperators.OPERATOR_IN, List.of(testTaxomomy.getId())),
								ModuleFieldName.IDENTIFICATION, Map.of(FilterOperators.OPERATOR_EQ, "IDENTIFIED")),
						fields, Collections.singletonList(ModuleFieldName.TECHNOLOGY));
		assertNotNull(listAggregationResult1);
		assertEquals(2, listAggregationResult.size());
		final List<String> types1 = listAggregationResult.stream().map(
				aggregation -> aggregation.getGroup().get(ModuleFieldName.TYPE).toString()).collect(Collectors.toList());
		for (int i = 1; i < types.size(); i++) {
			assertTrue(types1.get(i - 1).compareTo(types1.get(i)) <= 0);
		}

		/* Test with taxonomy Entity ID filter */
		final List<AggregationResult<ModuleFieldName>> listAggregationResult2 =
				generateAggregationResult(groupBy,
						Map.of(ModuleFieldName.TAXONOMY_ID, Map.of(FilterOperators.OPERATOR_IN, List.of(testTaxomomy.getId())),
								ModuleFieldName.IDENTIFICATION, Map.of(FilterOperators.OPERATOR_EQ, "IDENTIFIED")),
						fields, Collections.singletonList(ModuleFieldName.TECHNOLOGY));
		assertNotNull(listAggregationResult2);
		assertEquals(2, listAggregationResult.size());
		final List<String> types2 = listAggregationResult.stream().map(
				aggregation -> aggregation.getGroup().get(ModuleFieldName.TYPE).toString()).collect(Collectors.toList());
		for (int i = 1; i < types2.size(); i++) {
			assertTrue(types2.get(i - 1).compareTo(types2.get(i)) <= 0);
		}
	}

	@Test
	void testPostUtilityAggregatedValuesFilteredByTaxonomy() {
		final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<ModuleFieldName>();
		aggregationRequest.setFilterObject(Map.of(ModuleFieldName.TAXONOMY_ID, Map.of(FilterOperators.OPERATOR_EQ, "4")));
		aggregationRequest.setGroupBy(Collections.singleton(ModuleFieldName.TECHNOLOGY));
		aggregationRequest.setFields(Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT));
		final HttpEntity<AggregationRequest<ModuleFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<ModuleFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + MODULE_UTILITY_AGGREGATION_URI,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(200, clientOneResponseEntity.getStatusCode().value());
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		assertEquals(0, listAggregationResult.size());
	}

	@Test
	void testPostAggregatedValuesFieldsOnly() {
		final Map<ModuleFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(ModuleFieldName.ID, AggregationOperator.COUNT);
		fields.put(ModuleFieldName.LINES_OF_CODE, AggregationOperator.SUM);
		final List<AggregationResult<ModuleFieldName>> listAggregationResult =
				generateAggregationResult(null, null, fields, null);
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		assertNotNull(listAggregationResult.get(0).getFields().get(ModuleFieldName.ID));
		assertNotNull(listAggregationResult.get(0).getFields().get(ModuleFieldName.LINES_OF_CODE));
	}

	@Test
	void testPostAggregatedValuesGroupByOnly() {
		final Set<ModuleFieldName> groupBy = new HashSet<>();
		groupBy.add(ModuleFieldName.TECHNOLOGY);
		groupBy.add(ModuleFieldName.TYPE);
		final List<AggregationResult<ModuleFieldName>> listAggregationResult =
				generateAggregationResult(groupBy, null, null, null);
		assertNotNull(listAggregationResult);
		assertEquals(10, listAggregationResult.size());
		listAggregationResult.forEach(result -> {
			assertNotNull(result.getGroup().get(ModuleFieldName.TECHNOLOGY));
			assertNotNull(result.getGroup().get(ModuleFieldName.TYPE));
		});
	}

	@Test
	void testPostAggregatedValuesOrderByField() {
		final List<AggregationResult<ModuleFieldName>> listAggregationResult =
				generateAggregationResult(Collections.singleton(ModuleFieldName.TECHNOLOGY), null,
						Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT), Collections.singletonList(ModuleFieldName.ID));
		assertNotNull(listAggregationResult);
		final List<Integer> idCounts = listAggregationResult.stream().map(
				aggregation -> (Integer) aggregation.getFields().get(ModuleFieldName.ID)).collect(Collectors.toList());
		for (int i = 1; i < idCounts.size(); i++) {
			assertTrue(idCounts.get(i - 1) - idCounts.get(i) <= 0);
		}
	}

	@SuppressWarnings("unchecked")
	@Test
	void testPostAggregatedValuesGroupByFieldContainingModule() {
		final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<>();
		final Set<ModuleFieldName> groupBy = new HashSet<>(2);
		groupBy.add(ModuleFieldName.CONTAINING_MODULE_ID);
		groupBy.add(ModuleFieldName.CONTAINING_MODULE_NAME);
		aggregationRequest.setGroupBy(groupBy);
		final Map<ModuleFieldName, AggregationOperator> fields = new HashMap<>(2);
		fields.put(ModuleFieldName.ID, AggregationOperator.LIST);
		fields.put(ModuleFieldName.NAME, AggregationOperator.LIST);
		aggregationRequest.setFields(fields);
		final HttpEntity<AggregationRequest<ModuleFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<List<AggregationResult<ModuleFieldName>>> clientThreeResponseEntity = restTemplate.exchange(
				info.getUrl() + MODULE_AGGREGATION_URI,
				HttpMethod.POST,
				request,
				responseType,
				Long.valueOf(3));
		assertEquals(200, clientThreeResponseEntity.getStatusCode().value(), "Status code doesn't match expected : 200; "
				+ "actual : " +clientThreeResponseEntity.getStatusCode().value());
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = clientThreeResponseEntity.getBody();
		assertNotNull("Aggregation result should not be null", listAggregationResult);
		assertEquals(2, listAggregationResult.size(), "listAggregationResult size doesn't match expected : 2; actual : " + listAggregationResult.size());
		listAggregationResult.forEach(result -> {
			assertNotNull("List of Name fields should not be null", result.getFields().get(ModuleFieldName.NAME));
			assertNotNull("List of ID fields should not be null", result.getFields().get(ModuleFieldName.ID));
			final int noOfNames = ((List<String>) result.getFields().get(ModuleFieldName.NAME)).size();
			final int noOfIds = ((List<String>) result.getFields().get(ModuleFieldName.ID)).size();
			assertEquals(noOfIds, noOfNames, "Size of list of field Names and IDs should be equal. Number of names : "
					+ noOfNames + ", number of IDs : " + noOfIds);
		});
	}

	@Test
	void testAggregatedUtilityValuesAsCsvGroupByName() throws IOException {
		final ParameterizedTypeReference<Resource> responseTypeCsv = new ParameterizedTypeReference<Resource>() { };
		final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<>();
		aggregationRequest.setFields(Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT));
		aggregationRequest.setGroupBy(Arrays.asList(ModuleFieldName.NAME).stream().collect(Collectors.toSet()));
		final Map<ModuleFieldName, String> csvHeaders = new LinkedHashMap<>();
		csvHeaders.put(ModuleFieldName.NAME, "Utility Name");
		csvHeaders.put(ModuleFieldName.ID, "Number Of Invocations");
		aggregationRequest.setCsvHeaders(csvHeaders);
		final HttpEntity<AggregationRequest<ModuleFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<Resource> responseEntity = restTemplate.exchange(
				info.getUrl() + UTILITY_AGGREGATIONS_CSV_URL,
				HttpMethod.POST,
				request,
				responseTypeCsv,
				ONE);
		assertNotNull(responseEntity);
		assertEquals(HttpStatus.OK.value(), responseEntity.getStatusCode().value());
		assertNotNull(responseEntity.getHeaders().getContentType());
		assertEquals(CONTENT_TYPE, responseEntity.getHeaders().getFirst(FIRST_HEADER_NAME));
		assertNotNull(responseEntity.getHeaders().getContentDisposition().getFilename());
		assertNotNull(responseEntity.getBody());
		final String actualFileContent = new String(IOUtils.toByteArray(Optional.of(responseEntity.getBody()).get().getInputStream()), StandardCharsets.UTF_8);
		final String expectedFileContent = FileUtils.readFileToString(
				EXPECTED_FOLDER.resolve("aggregation-test-data-A").resolve(UTILITY_AGGREGATION_CSV_FILE_NAME).toFile(), StandardCharsets.UTF_8);
		assertEquals(expectedFileContent.toString().trim().replace("\r",""), actualFileContent.toString().trim().replace("\r",""), CONTENT_UNMATCHED_MSG);
	}

	@Test
	void testAggregatedUtilityValuesAsCsvGroupByNameAndCategories() throws IOException {
		final ParameterizedTypeReference<Resource> responseTypeCsv = new ParameterizedTypeReference<Resource>() { };
		final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<>();
		aggregationRequest.setFields(Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT));
		aggregationRequest.setGroupBy(new HashSet<>(Arrays.asList(ModuleFieldName.CATEGORIES, ModuleFieldName.NAME)));
		final Map<ModuleFieldName, String> csvHeaders = new LinkedHashMap<>();
		csvHeaders.put(ModuleFieldName.CATEGORIES, "Category");
		csvHeaders.put(ModuleFieldName.NAME, "Utility Name");
		csvHeaders.put(ModuleFieldName.ID, "Number Of Invocations");
		aggregationRequest.setCsvHeaders(csvHeaders);
		final HttpEntity<AggregationRequest<ModuleFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		final ResponseEntity<Resource> responseEntity = restTemplate.exchange(
				info.getUrl() + UTILITY_AGGREGATIONS_CSV_URL,
				HttpMethod.POST,
				request,
				responseTypeCsv,
				ONE);
		assertNotNull(responseEntity);
		assertEquals(HttpStatus.OK.value(), responseEntity.getStatusCode().value());
		assertNotNull(responseEntity.getHeaders().getContentType());
		assertEquals(CONTENT_TYPE, responseEntity.getHeaders().getFirst(FIRST_HEADER_NAME));
		assertNotNull(responseEntity.getHeaders().getContentDisposition().getFilename());
		assertNotNull(responseEntity.getBody());
		final String actualFileContent = new String(IOUtils.toByteArray(Optional.of(responseEntity.getBody()).get().getInputStream()), StandardCharsets.UTF_8);
		final String expectedFileContent = FileUtils.readFileToString(
				EXPECTED_FOLDER.resolve("aggregation-test-data-B").resolve(UTILITY_AGGREGATION_CSV_FILE_NAME).toFile(), StandardCharsets.UTF_8);
		final Set<String> actualFileContentList = convertFileContentToList(actualFileContent);
		final Set<String> expectedFileContentList = convertFileContentToList(expectedFileContent);
		assertEquals(expectedFileContentList.size(), actualFileContentList.size(), CONTENT_UNMATCHED_MSG);
		expectedFileContentList.removeIf(actualFileContentList::contains);
		assertEquals(0, expectedFileContentList.size(), CONTENT_UNMATCHED_MSG);
	}

	private Set<String> convertFileContentToList(final String content) {
		return Arrays.stream(content.replace("\",", "").split("\"")).filter(StringUtils::isNotBlank).collect(Collectors.toSet());
	}

	/**
	 * Tests aggregation on value GroupBy as Technology and field by Id
	 **/
	@Test
	void testPostAggregatedValuesFieldByIdAndGroupByTechnology() {
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.TECHNOLOGY), null, Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		final HashMap<Object, Object> aggregationResult = new HashMap<>();
		listAggregationResult.forEach(result -> {
			assertFalse("Each group should appear only once in the result", aggregationResult.containsKey(result.getGroup().get(ModuleFieldName.TECHNOLOGY)));
			aggregationResult.put(result.getGroup().get(ModuleFieldName.TECHNOLOGY), result.getFields().get(ModuleFieldName.ID));
		});
		assertEquals(6, listAggregationResult.size());
		assertEquals(29, aggregationResult.get("COBOL"));
		assertEquals(7, aggregationResult.get("NATURAL"));
		assertEquals(1, aggregationResult.get("CICS"));
		assertEquals(3, aggregationResult.get("BASIC"));
		assertEquals(1, aggregationResult.get("JCL"));
		assertEquals(2, aggregationResult.get("UNKNOWN"));
	}

	/**
	 * Tests aggregation on value GroupBy as Type and field By Id
	 **/
	@Test
	void testPostAggregatedValuesGroupByTypeAndFieldById() {
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.TYPE), Map.of(), Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(8, listAggregationResult.size());
		final HashMap<Object, Object> aggregationResult = new HashMap<>();
		listAggregationResult.forEach(result -> {
			assertFalse("Each group should appear only once in the result", aggregationResult.containsKey(result.getGroup().get(ModuleFieldName.TYPE)));
			aggregationResult.put(result.getGroup().get(ModuleFieldName.TYPE), result.getFields().get(ModuleFieldName.ID));
		});
		assertEquals(17, aggregationResult.get("COPYBOOK"));
		assertEquals(19, aggregationResult.get("PROGRAM"));
		assertEquals(1, aggregationResult.get("COPYCODE"));
		assertEquals(1, aggregationResult.get("BMS_MAPSET"));
		assertEquals(1, aggregationResult.get("OBJECT"));
		assertEquals(1, aggregationResult.get("EXEC_PGM"));
		assertEquals(1, aggregationResult.get("FUNCTION"));
		assertEquals(2, aggregationResult.get("UTILITY"));
	}

	/**
	 * Tests aggregation on value GroupBy as Technology and filter by linesOfCode >=0 and representation == "PHYSICAL"
	 */
	@Test
	void testPostAggregatedValuesGroupByTechnologyAndFilterLinesOfCodeGreaterThanZeroAndRepresentation() {
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.TECHNOLOGY),
				Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 0),
						ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL")),
				Collections.singletonMap(ModuleFieldName.LINES_OF_CODE, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		assertTrue("Lines of code greater than 0", (Integer) listAggregationResult.get(0).getFields().get(ModuleFieldName.LINES_OF_CODE) >= 0);
	}

	/**
	 * Tests aggregation on value GroupBy as Technology and filter by linesOfDeadCode >= 0 and representation == "PHYSICAL"
	 */
	@Test
	void testPostAggregatedValuesGroupByTechnologyAndFilterLinesOfDeadCodeGreaterThanZeroAndRepresentation() {
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.TECHNOLOGY),
				Map.of(ModuleFieldName.LINES_OF_DEAD_CODE, Map.of(FilterOperators.OPERATOR_GTE, 0),
						ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL")),
				Collections.singletonMap(ModuleFieldName.LINES_OF_CODE, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(0, listAggregationResult.size());
	}

	/**
	 * Tests aggregation on value GroupBy as Technology and filter by linesOfComment>= 0 and representation == "PHYSICAL"
	 */
	@Test
	void testPostAggregatedValuesGroupByTechnologyAndLinesOfCommentGreaterthanZero() {
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.TECHNOLOGY),
				Map.of(ModuleFieldName.LINES_OF_COMMENT, Map.of(FilterOperators.OPERATOR_GTE, 0),
						ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL")),
				Collections.singletonMap(ModuleFieldName.LINES_OF_COMMENT, AggregationOperator.SUM), null);
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		assertEquals(230, listAggregationResult.get(0).getFields().get(ModuleFieldName.LINES_OF_COMMENT));
		assertEquals("COBOL", listAggregationResult.get(0).getGroup().get(ModuleFieldName.TECHNOLOGY));
	}

	/**
	 * Tests aggregation on value GroupBy as Technology and filter by representation== "PHYSICAL"
	 */
	@Test
	void testPostAggregatedValuesGroupByTechnologyAndFilterByRepresentation() {
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.TECHNOLOGY), Map.of(ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL")),
				Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		assertEquals(5, listAggregationResult.get(0).getFields().get(ModuleFieldName.ID));
		assertEquals("COBOL", listAggregationResult.get(0).getGroup().get(ModuleFieldName.TECHNOLOGY));
	}

	/**
	 * Tests aggregation on value GroupBy as Technology and Type and field By Lines of code and filter by identificationLink.name!='MISSING'
	 */
	@Test
	void testPostAggregatedValuesGroupByTechnologyAndTypeAndOrderByTechnology() {
		final List<ModuleFieldName> orderBy = new ArrayList<>();
		orderBy.add(ModuleFieldName.TECHNOLOGY);
		final Set<ModuleFieldName> groupBy = new HashSet<>();
		groupBy.add(ModuleFieldName.TECHNOLOGY);
		groupBy.add(ModuleFieldName.TYPE);
		final Map<ModuleFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(ModuleFieldName.ID, AggregationOperator.COUNT);
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(groupBy,
				Map.of(ModuleFieldName.IDENTIFICATION, Map.of(FilterOperators.OPERATOR_EQ, "IDENTIFIED")), fields, orderBy);
		assertNotNull(listAggregationResult);
		assertEquals(10, listAggregationResult.size());
		final HashMap<HashMap<Object, Object>, Object> aggregationResultMap = new HashMap<>();
		listAggregationResult.forEach(result -> {
			HashMap<Object, Object> aggregationResult = new HashMap<>();
			assertFalse("Each group should appear only once in the result", aggregationResult.containsKey(result.getGroup().get(ModuleFieldName.TECHNOLOGY)));
			aggregationResult.put(result.getGroup().get(ModuleFieldName.TECHNOLOGY), result.getGroup().get(ModuleFieldName.TYPE));
			aggregationResultMap.put(aggregationResult, result.getFields().get(ModuleFieldName.ID));
		});
		assertEquals(1, aggregationResultMap.get(Collections.singletonMap("BASIC", "OBJECT")));
		assertEquals(1, aggregationResultMap.get(Collections.singletonMap("BASIC", "FUNCTION")));
		assertEquals(1, aggregationResultMap.get(Collections.singletonMap("BASIC", "PROGRAM")));
		assertEquals(1, aggregationResultMap.get(Collections.singletonMap("CICS", "BMS_MAPSET")));
		assertEquals(12, aggregationResultMap.get(Collections.singletonMap("COBOL", "PROGRAM")));
		assertEquals(17, aggregationResultMap.get(Collections.singletonMap("COBOL", "COPYBOOK")));
		assertEquals(1, aggregationResultMap.get(Collections.singletonMap("JCL", "EXEC_PGM")));
		assertEquals(6, aggregationResultMap.get(Collections.singletonMap("NATURAL", "PROGRAM")));
		assertEquals(1, aggregationResultMap.get(Collections.singletonMap("NATURAL", "COPYCODE")));
		assertEquals(2, aggregationResultMap.get(Collections.singletonMap("UNKNOWN", "UTILITY")));
	}

	/**
	 * Tests aggregation on value GroupBy as Technology and Type and field By Lines of code and filter by identificationLink.name=='MISSING'
	 */
	@Test
	void testPostAggregatedValuesFilterByIdentificationLinkEqualToMissingAndGroupByTechnologyAndType() {
		final Set<ModuleFieldName> groupBy = new HashSet<>();
		groupBy.add(ModuleFieldName.TECHNOLOGY);
		groupBy.add(ModuleFieldName.TYPE);
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(groupBy,
				Map.of(ModuleFieldName.IDENTIFICATION, Map.of(FilterOperators.OPERATOR_IS_FALSE, "MISSING")),
				Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT),
				Collections.singletonList(ModuleFieldName.TECHNOLOGY));
		assertNotNull(listAggregationResult);
		assertEquals(0, listAggregationResult.size());
	}

	/**
	 * Tests aggregation on value GroupBy as Technology and Type and field By Lines of code and filter by identificationLink.name!="MISSING" and
	 *  representation == "PHYSICAL"
	 */
	@Test
	void testPostAggregatedValuesFilterByIdentificationLinkNotEqualToMissingAndGroupByTechnologyAndType() {
		final Set<ModuleFieldName> groupBy = new HashSet<>();
		groupBy.add(ModuleFieldName.TECHNOLOGY);
		groupBy.add(ModuleFieldName.TYPE);
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(groupBy,
				Map.of(ModuleFieldName.IDENTIFICATION, Map.of(FilterOperators.OPERATOR_IS_TRUE, "!MISSING"),
						ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL")),
				Collections.singletonMap(ModuleFieldName.LINES_OF_CODE, AggregationOperator.SUM), Collections.singletonList(ModuleFieldName.TECHNOLOGY));
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		assertEquals(535, listAggregationResult.get(0).getFields().get(ModuleFieldName.LINES_OF_CODE));
	}

	/**
	 * Tests aggregation on value Filter By linesofCode >0 and complexity and field by Id
	 */
	@Test
	void testPostAggregatedValuesFilterByComplexityAndLineOfCodeAndFieldById() {
		List<Map<ModuleFieldName, Map<String, Object>>> filterlist = new ArrayList<>();
		filterlist.add(Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1),
				ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL"),
				ModuleFieldName.COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 0, FilterOperators.OPERATOR_LTE, 10)));
		filterlist.add(Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1),
				ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL"),
				ModuleFieldName.COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 11, FilterOperators.OPERATOR_LTE, 20)));
		filterlist.add(Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1),
				ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL"),
				ModuleFieldName.COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 21, FilterOperators.OPERATOR_LTE, 50)));
		filterlist.add(Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1),
				ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL"),
				ModuleFieldName.COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 51)));
		filterlist.forEach(filters -> {
			List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.TECHNOLOGY), filters, Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT), null);
			assertNotNull(listAggregationResult);
			if (filters.get(ModuleFieldName.COMPLEXITY).get(FilterOperators.OPERATOR_GTE).equals(0)) {
				assertEquals(1, listAggregationResult.size());
				assertEquals("COBOL", listAggregationResult.get(0).getGroup().get(ModuleFieldName.TECHNOLOGY));
				assertEquals(5, listAggregationResult.get(0).getFields().get(ModuleFieldName.ID));
			} else {
				assertEquals(0, listAggregationResult.size());
			}
		});
	}

	/**
	 * Tests aggregation on value Filter By linesofCode >0 and complexity and field by Lines of code
	 */
	@Test
	void testPostAggregatedValuesFilterByComplexityAndLineOfCodeAndFieldByLinesOfCode() {
		List<Map<ModuleFieldName, Map<String, Object>>> filterlist = new ArrayList<>();
		filterlist.add(Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1),
				ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL"),
				ModuleFieldName.COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 0, FilterOperators.OPERATOR_LTE, 10)));
		filterlist.add(Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1),
				ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL"),
				ModuleFieldName.COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 11, FilterOperators.OPERATOR_LTE, 20)));
		filterlist.add(Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1),
				ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL"),
				ModuleFieldName.COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 21, FilterOperators.OPERATOR_LTE, 50)));
		filterlist.add(Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1),
				ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL"),
				ModuleFieldName.COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 51)));
		filterlist.forEach(filters -> {
			List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.TECHNOLOGY),
					filters, Collections.singletonMap(ModuleFieldName.LINES_OF_CODE, AggregationOperator.SUM), null);
			assertNotNull(listAggregationResult);
			if (filters.get(ModuleFieldName.COMPLEXITY).get(FilterOperators.OPERATOR_GTE).equals(0)) {
				assertEquals(1, listAggregationResult.size());
				assertTrue("Lines of codes should be Greater than 0 and Complexity should be in between 0 and 10",
						(Integer) listAggregationResult.get(0).getFields().get(ModuleFieldName.LINES_OF_CODE) > 0);
			} else {
				assertEquals(0, listAggregationResult.size());
			}
		});
	}

	/**
	 * Tests aggregation on value Filter by linesofCode>0 and complexity>=0
	 */
	@Test
	void testPostAggregatedValueCodeLinesOfCodeGreaterThanZeroAndFieldById() {
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.COMPLEXITY),
				Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1),
						ModuleFieldName.COMPLEXITY, Map.of(FilterOperators.OPERATOR_GTE, 0),
						ModuleFieldName.TYPE, Map.of(FilterOperators.OPERATOR_IN, List.of("MAP", "BMS_MAP", "DIALOG", "DIALOG_PRIV_RES", "HELP"))),
				Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(0, listAggregationResult.size());
	}

	/**
	 * Tests aggregation on value Filter By LinesOfCode >0
	 */
	@Test
	void testPostAggregatedValueFilterByLinesOfCodeGreaterthanZeroAndFieldsByLinesOfCodeAndComment() {
		final Map<ModuleFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(ModuleFieldName.LINES_OF_CODE, AggregationOperator.SUM);
		fields.put(ModuleFieldName.LINES_OF_COMMENT, AggregationOperator.SUM);
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.NAME),
				Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1),
						ModuleFieldName.TYPE, Map.of(FilterOperators.OPERATOR_EQ, "PROGRAM")),
				fields,
				Collections.singletonList(ModuleFieldName.LINES_OF_CODE));
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		assertTrue("Lines of Codes should be Greater than 0", (Integer) listAggregationResult.get(0).getFields().get(ModuleFieldName.LINES_OF_CODE) > 0);
	}

	/**
	 * Tests aggregation on value filter By linesofdeadCode >0
	 */
	@Test
	void testPostAggregatedValueFilterByLinesOfDeadCodeGreaterThanZero() {
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.TECHNOLOGY), Map.of(ModuleFieldName.LINES_OF_DEAD_CODE, Map.of(FilterOperators.OPERATOR_GTE, 1)),
				Collections.singletonMap(ModuleFieldName.LINES_OF_DEAD_CODE, AggregationOperator.SUM), null);
		assertNotNull(listAggregationResult);
		assertEquals(0, listAggregationResult.size());
	}

	/**
	 * Tests aggregation on value Field by Id and filterGroup By Id
	 */
	@Test
	void testPostAggregatedValueFilterGroupByIdGreaterThanTwentyFiveAndLessThanFifty() {
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.CONTAINING_MODULE_ID),
				Map.of(ModuleFieldName.TYPE, Map.of(FilterOperators.OPERATOR_IN, Arrays.asList("EXEC", "EXEC_PGM"))),
				Collections.singletonMap(ModuleFieldName.ID, AggregationOperator.COUNT), null);
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		assertTrue("ID should be less than 25", (Integer) listAggregationResult.get(0).getFields().get(ModuleFieldName.ID) <= 25);
	}

	/**
	 * Tests aggregation on value GroupBy as Technology and field by lines of code and Id
	 */
	@Test
	void testPostAggregatedValuesFieldsByLinesOfCodeAndIdAndGroupByTechnology() {
		final Map<ModuleFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(ModuleFieldName.LINES_OF_CODE, AggregationOperator.SUM);
		fields.put(ModuleFieldName.ID, AggregationOperator.COUNT);
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(Collections.singleton(ModuleFieldName.TECHNOLOGY),
				Map.of(ModuleFieldName.LINES_OF_CODE, Map.of(FilterOperators.OPERATOR_GTE, 0),
						ModuleFieldName.REPRESENTATION, Map.of(FilterOperators.OPERATOR_EQ, "PHYSICAL")),
				fields, null);
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
		listAggregationResult
				.forEach(result -> assertTrue("Lines of code should be greater than 0", (Integer) result.getFields().get(ModuleFieldName.LINES_OF_CODE) >= 0));
	}

	/**
	 * Tests aggregation on value Field by Id
	 */
	@Test
	void testPostAggregatedValuesFieldById() {
		final Map<ModuleFieldName, AggregationOperator> fields = new HashMap<>();
		fields.put(ModuleFieldName.ID, AggregationOperator.COUNT);
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = generateAggregationResult(null, null, fields, null);
		assertNotNull(listAggregationResult);
		assertEquals(1, listAggregationResult.size());
	}

	/**
	 * Return AggregationList based on filterGroup, groupBy, filter, fields and order.
	 **/
	private List<AggregationResult<ModuleFieldName>> generateAggregationResult(@Nullable final Set<ModuleFieldName> groupBy,
			@Nullable final Map<ModuleFieldName, Map<String, Object>> filterObject,
			@Nullable final Map<ModuleFieldName, AggregationOperator> fields,
			@Nullable final List<ModuleFieldName> orderBy) {
		AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<>();
		if (groupBy != null) {
			aggregationRequest.setGroupBy(groupBy);
		}
		if (orderBy != null) {
			aggregationRequest.setOrderBy(orderBy);
		}
		if (fields != null) {
			aggregationRequest.setFields(fields);
		}
		if (filterObject != null) {
			aggregationRequest.setFilterObject(filterObject);
		}
		HttpEntity<AggregationRequest<ModuleFieldName>> request = new HttpEntity<>(aggregationRequest, RestTemplateUtil.getHttpHeaders(info));
		ResponseEntity<List<AggregationResult<ModuleFieldName>>> clientOneResponseEntity = restTemplate.exchange(
				info.getUrl() + MODULE_AGGREGATION_URI,
				HttpMethod.POST,
				request,
				responseType,
				ONE);
		assertNotNull(clientOneResponseEntity);
		assertEquals(200, clientOneResponseEntity.getStatusCode().value());
		final List<AggregationResult<ModuleFieldName>> listAggregationResult = clientOneResponseEntity.getBody();
		assertNotNull(listAggregationResult);
		return listAggregationResult;
	}
}
