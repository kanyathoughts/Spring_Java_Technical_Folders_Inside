/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;

/**
 * Test for {@linkplain CustomPropertyMetadata} custom Category
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class CustomCategoryDataPointTests extends DatabaseRelatedTest {
	
	private static EntityId projectId = EntityId.of(1L);
	
	@Autowired
	private MockMvc mockMvc;
	
	@Autowired
	private DataPointRegistry dataPointRegistry;
	
	@Autowired
	private ObjectMapper objectMapper;
	
	private static CustomPropertyMetadata customPropertyMetadata3 = new CustomPropertyMetadata();
	
	@BeforeAll
	void init() throws Exception {
		projectId = createProject("Project1");
		final CustomPropertyMetadata customPropertyMetadata1 = new CustomPropertyMetadata();
		customPropertyMetadata1.setName("AnnotationCustomcategory1");
		customPropertyMetadata1.setLabel("AnnotationCustomcategory1");
		customPropertyMetadata1.setFieldType(CustomPropertyFieldType.DEFAULT);
		customPropertyMetadata1.setPluginVisible(false);
		customPropertyMetadata1.setCustomViewIndex(Integer.valueOf(2));
		customPropertyMetadata1.setDataType("STRING");
		customPropertyMetadata1.setCustomCategory("Customcategory1");
		
		mockMvc.perform(post("/api" + MetamodelController.DEFINE_METADATA_DEFAULT_PROPERTY_URL, projectId.getNid(), "Annotation", customPropertyMetadata1.getName())
				.contentType("application/json")
				.content(objectMapper.writeValueAsString(customPropertyMetadata1)))
				.andExpect(status().isOk());

		final CustomPropertyMetadata customPropertyMetadata2 = new CustomPropertyMetadata();
		customPropertyMetadata2.setName("AnnotationCustomcategory2");
		customPropertyMetadata2.setLabel("AnnotationCustomcategory2");
		customPropertyMetadata2.setFieldType(CustomPropertyFieldType.DEFAULT);
		customPropertyMetadata2.setPluginVisible(false);
		customPropertyMetadata2.setCustomViewIndex(Integer.valueOf(2));
		customPropertyMetadata2.setDataType("STRING");
		customPropertyMetadata2.setCustomCategory("Customcategory2");
		
		mockMvc.perform(post("/api" + MetamodelController.DEFINE_METADATA_DEFAULT_PROPERTY_URL, projectId.getNid(), "Annotation",
				customPropertyMetadata2.getName()).contentType("application/json")
				.content(objectMapper.writeValueAsString(customPropertyMetadata2)))
				.andExpect(status().isOk());
		
		customPropertyMetadata3.setName("AnnotationCustomcategory3");
		customPropertyMetadata3.setLabel("AnnotationCustomcategory3");
		customPropertyMetadata3.setFieldType(CustomPropertyFieldType.DEFAULT);
		customPropertyMetadata3.setPluginVisible(false);
		customPropertyMetadata3.setCustomViewIndex(Integer.valueOf(3));
		customPropertyMetadata3.setDataType("STRING");
		
		mockMvc.perform(post("/api" + MetamodelController.DEFINE_METADATA_DEFAULT_PROPERTY_URL, projectId.getNid(), "Annotation",
				customPropertyMetadata3.getName()).contentType("application/json")
				.content(objectMapper.writeValueAsString(customPropertyMetadata3)))
				.andExpect(status().isOk());
	}
	
	@Test
	void customCategoryAnnotationTest() throws Exception {		
		List<MiningDataPointDefinitionWithPath> dataPoints = dataPointRegistry
				.getDataPointsForTypeRecursivelyWithUsage(Optional.ofNullable(projectId).map(EntityId::getNid), "Annotation", Collections.singletonList("miningUi.annotationsTable"))
				.stream().filter(points -> {
					final Map<String, String> usageAttributes = points.getUsageAttributes().get("miningUi.annotationsTable");
					return usageAttributes != null && usageAttributes.get("category").equals("Customcategory1");
				}).collect(Collectors.toList());
		
		assertEquals(1, dataPoints.size());
		assertEquals("AnnotationCustomcategory1", dataPoints.get(0).getName());
		
		dataPoints = dataPointRegistry
				.getDataPointsForTypeRecursivelyWithUsage(Optional.ofNullable(projectId).map(EntityId::getNid), "Annotation", Collections.singletonList("miningUi.annotationsTable"))
				.stream().filter(points -> {
					final Map<String, String> usageAttributes = points.getUsageAttributes().get("miningUi.annotationsTable");
					return usageAttributes != null && usageAttributes.get("category").equals("Customcategory2");
				}).collect(Collectors.toList());
		
		assertEquals(1, dataPoints.size());
		assertEquals("AnnotationCustomcategory2", dataPoints.get(0).getName());
		
		customPropertyMetadata3.setCustomCategory("Customcategory1");
		
		mockMvc.perform(post("/api" + MetamodelController.DEFINE_METADATA_DEFAULT_PROPERTY_URL, projectId.getNid(), "Annotation",
				customPropertyMetadata3.getName()).contentType("application/json")
				.content(objectMapper.writeValueAsString(customPropertyMetadata3)))
				.andExpect(status().isOk());
		
		dataPoints = dataPointRegistry
				.getDataPointsForTypeRecursivelyWithUsage(Optional.ofNullable(projectId).map(EntityId::getNid), "Annotation", Collections.singletonList("miningUi.annotationsTable"))
				.stream().filter(points -> {
					final Map<String, String> usageAttributes = points.getUsageAttributes().get("miningUi.annotationsTable");
					return usageAttributes != null && usageAttributes.get("category").equals("Customcategory1");
				}).collect(Collectors.toList());
		
		assertEquals(2, dataPoints.size());
		assertEquals(Arrays.asList("AnnotationCustomcategory1", "AnnotationCustomcategory3"),
				dataPoints.stream().map(MiningDataPointDefinitionWithPath::getName).collect(Collectors.toList()));
		
		mockMvc.perform(delete("/api" + MetamodelController.DEFINE_METADATA_DEFAULT_PROPERTY_URL, projectId.getNid(), "Annotation", "AnnotationCustomcategory1"))
				.andExpect(status().isOk());
		
		assertEquals(0, dataPointRegistry.getDataPointsForTypeRecursivelyWithUsage(Optional.ofNullable(projectId).map(EntityId::getNid), "Annotation", Collections.emptyList())
				.stream().filter(points -> points.getName().equals("AnnotationCustomcategory1")).collect(Collectors.toList()).size());
	}
	
	private EntityId createProject(final String projectName) {
		return projectService.create(new ProjectPojoPrototype()
											.setName(projectName)
											.setClient(EntityId.of(1L)), false)
				.identity();
	}
}
