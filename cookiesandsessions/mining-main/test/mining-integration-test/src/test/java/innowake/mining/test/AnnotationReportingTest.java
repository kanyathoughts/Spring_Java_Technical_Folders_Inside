/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.commons.lang3.time.StopWatch;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.client.service.annotation.AnnotationServiceProvider;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.AnnotationReport;
import innowake.mining.shared.model.AnnotationReportResponse;
import innowake.mining.shared.model.AnnotationReportSearchParameter;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;

/**
 * Integration tests for the Annotation Reporting service.
 */
@TestInstance(Lifecycle.PER_CLASS)
class AnnotationReportingTest extends IntegrationTest {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/1/annotation-reporting?hasLimit=true";
	private static final Logger LOG = LoggerFactory.getLogger(AnnotationReportingTest.class);

	private static final Long ONE = Long.valueOf(1);
	private static final AnnotationType TEST_ANNOTATION_TYPE = AnnotationType.RULE;
	private static final EntityId CUSTOM_MODULE_ID = EntityId.of(2000L);
	private static final String INITIAL_SOURCE_ATTACHMENT = "This is initial source attachment content";

	private final ExecutorService pool = Executors.newWorkStealingPool();
	private final AnnotationServiceProvider annotationServiceProvider = MiningApiClient.annotationService(getConnectionInfo());
	private final RestTemplate restTemplate = new RestTemplate();

	@AfterAll
	void shutdown() {
		pool.shutdownNow();
	}

	@Test
	void testGetAnnotationsForReportingWithoutSearchCriteria() throws JsonProcessingException {
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setAnnotationCategory("");
		annotationReportingSearch.setAnnotationType("");
		annotationReportingSearch.setDescription("");
		annotationReportingSearch.setModuleName("");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());
		final String result = makeHttpCall(request);
		final AnnotationReportResponse response = PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result);
		assertNotEquals(0, response.getNumberOfRecords());
	}

	@Test
	void testGetAnnotationsForReportingAnnotationCategory() throws JsonMappingException, JsonProcessingException {
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setAnnotationCategory("Annotation Category A");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());
		final String result = makeHttpCall(request);
		final AnnotationReportResponse response = PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result);
		assertNotEquals(0, response.getNumberOfRecords());
		final List<AnnotationReport> annoationReportList = response.getAnnotationReportList();
		assertNotNull(annoationReportList);
		annoationReportList.stream().forEach(annotationReport -> {
			assertEquals("Annotation Category A", annotationReport.getCategoryName());
		});
	}

	@Test
	void testGetAnnotationsForReportingAnnotationType() throws JsonProcessingException {
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setAnnotationType("RULE");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());
		final String result = makeHttpCall(request);
		final AnnotationReportResponse response = PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result);
		assertNotEquals(0, response.getNumberOfRecords());
		final List<AnnotationReport> annoationReportList = response.getAnnotationReportList();
		assertNotNull(annoationReportList);
		annoationReportList.stream().forEach(annotationReport -> {
			assertEquals(AnnotationType.RULE, annotationReport.getAnnotationType());
		});
	}

	@Test
	void testGetAnnotationsForReportingLuceneAnnotationDescription() throws JsonProcessingException {
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setDescription("Anno%1");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());
		final String result = makeHttpCall(request);
		final AnnotationReportResponse response = PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result);
		assertNotEquals(0, response.getNumberOfRecords());
		final List<AnnotationReport> annoationReportList = response.getAnnotationReportList();
		assertNotNull(annoationReportList);
		annoationReportList.stream().forEach(annotationReport -> {
			assertTrue(annotationReport.getName().contains("Anno") && annotationReport.getName().contains("1"));
		});
	}

	@Test
	void testGetAnnotationsForReportingLuceneModuleName() throws JsonProcessingException {
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setModuleName("PRG%");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());
		final String result = makeHttpCall(request);
		final AnnotationReportResponse response = PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result);
		assertNotEquals(0, response.getNumberOfRecords());
		final List<AnnotationReport> annoationReportList = response.getAnnotationReportList();
		assertNotNull(annoationReportList);
		annoationReportList.stream().forEach(annotationReport -> {
			assertTrue(annotationReport.getModuleName().contains("PRG"));
		});
	}

	@Test
	void testGetAnnotationsForReportingLuceneModuleId() throws JsonProcessingException {
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setModuleName("PRG1");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());
		final String result = makeHttpCall(request);
		final AnnotationReportResponse response = PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result);
		assertTrue("NumberOfRecords in AnnotationReportResponse should be more than 0", response.getNumberOfRecords() > 0);
		final List<AnnotationReport> annoationReportList = response.getAnnotationReportList();
		assertNotNull(annoationReportList);
		assertNotNull(annoationReportList.get(0));
		assertEquals(2000, annoationReportList.get(0).getModuleId());
	}

	@Test
	void testGetAnnotationsForReportingLuceneModuleTaxonomy() throws JsonProcessingException {
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setModuleTaxonomy("Employee domain");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());
		final String result = makeHttpCall(request);
		final AnnotationReportResponse response = PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result);
		assertNotEquals(0, response.getNumberOfRecords());
		final List<AnnotationReport> annoationReportList = response.getAnnotationReportList();
		assertNotNull(annoationReportList);
		annoationReportList.stream().forEach(annotationReport -> {
			assertTrue(Assert.assertNotNull(annotationReport.getTaxonomy(), "Taxonomy must not be null.").contains("Employee domain"));
		});
	}

	@Test
	void testGetAnnotationsForReportingLuceneNoResult() throws JsonProcessingException {
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setDescription("Anno* +noresult +1");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());
		final String result = makeHttpCall(request);
		final AnnotationReportResponse response = PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result);
		assertEquals(0, response.getNumberOfRecords());
	}

	@Test
	void testGetAnnotationsForReportingUpdatedBy() throws JsonProcessingException {
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setUpdatedBy("admin");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());
		final String result = makeHttpCall(request);
		final AnnotationReportResponse response = PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result);
		assertNotEquals(0, response.getNumberOfRecords());
		final List<AnnotationReport> annoationReportList = response.getAnnotationReportList();
		assertNotNull(annoationReportList);
		assertEquals(8, annoationReportList.size());
		for (int i = 0; i < annoationReportList.size(); i++) {
			assertEquals("admin", annoationReportList.get(i).getUpdatedByUserId(), "Not updated by admin. Problematic AnnotationReport: " + annoationReportList.get(0));
		}
	}

	@Test
	void testGetAnnotationsForReportingHasMoreRecords() throws IOException {
		final int numberOfCreatedAnnotations = 501;
		createAnnotations(numberOfCreatedAnnotations);
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setDescription("MINIMAL ANNOTATION");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());
		final String result = makeHttpCall(request);
		final AnnotationReportResponse response = PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result);
		final int expectedNumberOfRetrievedAnnotations = 500;
		assertEquals(expectedNumberOfRetrievedAnnotations, Assert.assertNotNull(response.getAnnotationReportList()).size());
		assertTrue(response.getNumberOfRecords() >= numberOfCreatedAnnotations);
		assertTrue(response.isHasMoreRecords());
	}

	@Test
	void deactivingLimitRetrievesAllAvailableAnnotations() throws IOException {
		/*
		 * 13:01:57.840 [main] INFO  innowake.mining.test.AnnotationReportingTest - Creating 600 Annotation took 00:00:05.711
		 * 13:01:57.912 [main] INFO  innowake.mining.test.AnnotationReportingTest - Request took 00:00:00.063
		 * 13:01:57.914 [main] INFO  innowake.mining.test.AnnotationReportingTest - JSON deserialization took 00:00:00.001
		 */
		final int numberOfCreatedAnnotations = 600;

		time(() -> {
			createAnnotations(numberOfCreatedAnnotations);
			/* Just return a bogus value */
			return Boolean.TRUE;
		}, String.format("Creating %d Annotation", Integer.valueOf(numberOfCreatedAnnotations)));

		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setDescription("MINIMAL ANNOTATION");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());

		final String result = time(() -> makeUnlimitedHttpCall(request), "Request");
		final AnnotationReportResponse response = time(() -> PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result), "JSON deserialization");

		assertTrue(response.getNumberOfRecords() >= numberOfCreatedAnnotations);
		assertFalse(response.isHasMoreRecords());
	}

	/**
	 * Concurrently create Annotations.
	 *
	 * @param numberOfAnnotations the number of Annotations to create
	 */
	private void createAnnotations(final int numberOfAnnotations) {
		final List<Callable<Object>> tasks = new LinkedList<>();
		for (int i = 0; i < numberOfAnnotations; i++) {
			tasks.add(Executors.callable(() -> {
				try {
					annotationServiceProvider.createAnnotation()
						.setProjectId(ONE)
						.setModuleId(CUSTOM_MODULE_ID)
						.setAnnotation(createMinimalAnnotation())
						.execute();
				} catch (final IOException e) {
					throw new IllegalStateException(e);
				}
			}));
		}
		try {
			pool.invokeAll(tasks);
		} catch (InterruptedException e) {
			throw new IllegalStateException(e);
		}
	}

	private <V> V time(final Callable<V> runnable, final String message) {
		final StopWatch watch = new StopWatch();
		watch.start();
		final V result;
		try {
			result = runnable.call();
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
		watch.stop();
		LOG.info(() -> String.format("%s took %s", message, watch.toString()));
		return result;
	}

	@Nullable
	private String makeHttpCall(final HttpEntity<String> request) {
		return restTemplate.postForObject(getConnectionInfo().getUrl() + ENDPOINT, request, String.class);
	}

	@Nullable
	private String makeUnlimitedHttpCall(final HttpEntity<String> request) {
		return restTemplate.postForObject(getConnectionInfo().getUrl() + RouteConfiguration.API_BASE + "/v1/projects/1/annotation-reporting?hasLimit=false", 
				request, String.class);
	}

	private HttpHeaders getHttpHeader() {
		final HttpHeaders headers = new HttpHeaders();
		headers.set("Authorization", "Bearer " + getConnectionInfo().getToken());
		headers.setContentType(MediaType.APPLICATION_JSON);
		return headers;
	}

	private AnnotationPojoPrototype createMinimalAnnotation() {
		return new AnnotationPojoPrototype()
			.setName("MINIMAL ANNOTATION")
			.setState(WorkingState.IN_ANALYSIS)
			.setType(TEST_ANNOTATION_TYPE)
			.setSourceAttachment(INITIAL_SOURCE_ATTACHMENT)
			.setLocation(new ModuleLocation(10, 20));
	}
}
