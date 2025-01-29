/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.commons.lang3.time.StopWatch;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestTemplate;

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
import innowake.mining.shared.model.AnnotationReportResponse;
import innowake.mining.shared.model.AnnotationReportSearchParameter;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;

/**
 * Integration tests for the Annotation Reporting service.
 */
@Disabled("Long running performance tests for manual execution")
@TestInstance(Lifecycle.PER_CLASS)
class AnnotationReportingPerformanceTest extends IntegrationTest {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/1/annotation-reporting?hasLimit=true";
	private static final Logger LOG = LoggerFactory.getLogger(AnnotationReportingPerformanceTest.class);

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
	void testGetAnnotationsForReportingPerformanceWithLimit() throws IOException {
		/*
		 * 13:08:55.886 [main] INFO  innowake.mining.test.AnnotationReportingTest - Creating 20000 Annotation took 00:06:50.456
		 * 13:08:56.075 [main] INFO  innowake.mining.test.AnnotationReportingTest - Request took 00:00:00.188
		 * 13:08:56.077 [main] INFO  innowake.mining.test.AnnotationReportingTest - JSON deserialization took 00:00:00.002
		 */
		final int numberOfCreatedAnnotations = 20000;

		time(() -> {
			createAnnotations(numberOfCreatedAnnotations);
			/* Just return a bogus value */
			return Boolean.TRUE;
		}, String.format("Creating %d Annotation", Integer.valueOf(numberOfCreatedAnnotations)));
		
		final AnnotationReportSearchParameter annotationReportingSearch = new AnnotationReportSearchParameter();
		annotationReportingSearch.setDescription("MINIMAL ANNOTATION");
		final HttpEntity<String> request = new HttpEntity<>(PojoMapper.jsonWriter().writeValueAsString(annotationReportingSearch), getHttpHeader());

		final String result = time(() -> makeHttpCall(request), "Request");
		final AnnotationReportResponse response = time(() -> PojoMapper.jsonReaderFor(AnnotationReportResponse.class).readValue(result), "JSON deserialization");

		assertEquals(500, Assert.assertNotNull(response.getAnnotationReportList()).size());
		assertTrue(response.getNumberOfRecords() >= numberOfCreatedAnnotations);
		assertTrue(response.isHasMoreRecords());
	}
	
	@Test
	void testGetAnnotationsForReportingPerformanceUnlimited() throws IOException {
		/*
		 * 13:24:03.128 [main] INFO  innowake.mining.test.AnnotationReportingTest - Creating 20000 Annotation took 00:07:39.776
		 * 13:24:04.274 [main] INFO  innowake.mining.test.AnnotationReportingTest - Request took 00:00:01.130
		 * 13:24:04.408 [main] INFO  innowake.mining.test.AnnotationReportingTest - JSON deserialization took 00:00:00.133
		 */
		final int numberOfCreatedAnnotations = 20000;

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

		assertTrue(Assert.assertNotNull(response.getAnnotationReportList()).size() >= numberOfCreatedAnnotations);
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
