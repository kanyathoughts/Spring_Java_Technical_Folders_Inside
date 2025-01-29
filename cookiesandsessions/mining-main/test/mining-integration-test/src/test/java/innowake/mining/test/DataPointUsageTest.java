/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.test;

import static innowake.mining.test.util.RestTemplateUtil.getHttpHeaders;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.http.HttpStatus;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;

/**
 * Integration unit tests for type Annotation with miningUi.annotationsTable filter
 */
class DataPointUsageTest extends IntegrationTest {
	
	public static final String DATA_POINTS_FOR_TYPE_URL = RouteConfiguration.API_BASE +
			"/v2/projects/{projectId}/datapoints/for-type/{typeName}?usages={usages}";
	
	private static final Long ONE = Long.valueOf(1);
	private final RestTemplate restTemplate = new RestTemplate();
	private final ConnectionInfo info = getConnectionInfo();
	private final ParameterizedTypeReference<List<MiningDataPointDefinitionWithPath>> responseType =
			new ParameterizedTypeReference<List<MiningDataPointDefinitionWithPath>>() {};
	
	/**
	 * Updates the customPropertyClasses column for Project
	 *
	 * @throws IOException In case of a communication error.
	 */
	@BeforeEach
	void updateCustomProperties() throws IOException {
		try {
			if (connectionPostgres != null) {
				final Statement stmt = connectionPostgres.createStatement();
				final int result = stmt.executeUpdate("INSERT INTO custom_property_entities (project, entity, property)"
					+ " VALUES ((SELECT uid FROM project WHERE nid = 1), 'Annotation', (SELECT id FROM custom_property WHERE name = 'AnnotationCustomProperties'))");
				assertEquals(1, result);
				refreshMetamodel();
			} else {
				throw new IllegalStateException("Database connection was lost.");
			}
		} catch (SQLException e) {
			throw new IllegalStateException("Sql exception while updaing Custom properties in Project.", e);
		}		
	}
	
	/**
	 * Tests if Annotation Custom Properties are viewable in Annotation table by checking if the corresponding data points are returned.
	 */
	@Test
	void testDataPointUsages() {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(info));
		final ResponseEntity<List<MiningDataPointDefinitionWithPath>> responseEntity = restTemplate.exchange(
			info.getUrl() + DATA_POINTS_FOR_TYPE_URL, HttpMethod.GET,
			request,
			responseType,
			ONE, "Annotation", Usages.MINING_UI_ANNOTATIONS_TABLE);
		assertEquals(HttpStatus.SC_OK, responseEntity.getStatusCodeValue());
		final List<MiningDataPointDefinitionWithPath> datapointDefinition = responseEntity.getBody();
		assertNotNull(datapointDefinition);
		
		final List<MiningDataPointDefinitionWithPath> annotationDataPoints = datapointDefinition.stream().filter(
				dataPoint -> dataPoint.getName().equals("AnnotationCustomProperties")).collect(Collectors.toList());
		assertFalse(annotationDataPoints.isEmpty());
		assertEquals("Annotation_CustomProperties", annotationDataPoints.get(0).getParentTypeName());
		assertEquals("Annotation_CustomProperties_AnnotationCustomProperties", annotationDataPoints.get(0).getReferenceTypeName());
		assertEquals("customProperties.AnnotationCustomProperties", annotationDataPoints.get(0).getPath());
	
		final List<String> annotationCustomDataPoints = datapointDefinition.stream().filter(
				dataPoint -> dataPoint.getParentTypeName().equals("Annotation_CustomProperties_AnnotationCustomProperties"))
				.filter(dp -> {
					dp.getUsages().stream()
							.filter(usage -> usage.equals(Usages.MINING_UI_ANNOTATIONS_TABLE))
							.forEach(usage -> assertNotNull("Usage '" + usage + "' of data point '" + dp.getPath() + "' should define a category", 
									dp.getUsageAttributes().containsKey(usage) ? dp.getUsageAttributes().get(usage).get(TableAttributes.CATEGORY) : null));
					return true;
				}).map(MiningDataPointDefinition::getName).collect(Collectors.toList());
		Collections.sort(annotationCustomDataPoints);
		assertEquals(Arrays.asList(
				"annotationTags",
				"colorTags",
				"customAnnotationProperty",
				"customMetaInfo",
				"ruleTags"
			), annotationCustomDataPoints);
	}

}
