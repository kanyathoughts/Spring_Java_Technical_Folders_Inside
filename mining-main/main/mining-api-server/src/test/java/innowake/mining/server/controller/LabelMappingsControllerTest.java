/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.controller;

import static innowake.mining.shared.datapoints.LabelMappings.LabelType.MODULE_DEPENDENCIES_RELATIONSHIP_LABELS;
import static innowake.mining.shared.datapoints.LabelMappings.LabelType.ANNOTATION_METADATA_REASON_LABELS;
import static innowake.mining.shared.datapoints.LabelMappings.LabelType.MODULE_TYPE_LABELS;
import static innowake.mining.shared.datapoints.LabelMappings.LabelType.PROJECT_LABELS;
import static innowake.mining.shared.datapoints.LabelMappings.LabelType.TECHNOLOGY_LABELS;
import static innowake.mining.shared.datapoints.LabelMappings.LabelType.TYPE_LABELS;
import static innowake.mining.shared.datapoints.LabelMappings.LabelType.ANNOTATION_STATES;
import static innowake.mining.shared.datapoints.LabelMappings.LabelType.DEFINED_LOCATION_LABELS;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.datapoints.LabelMappings;
import innowake.mining.shared.datapoints.LabelMappings.LabelType;

/**
 * Mocked tests verifying the behavior of {@link LabelMappingsController}.
 */
@AutoConfigureMockMvc
class LabelMappingsControllerTest extends DatabaseRelatedTest {

	private final MockMvc mvc;

	@Autowired
	public LabelMappingsControllerTest(final MockMvc mvc) {
		this.mvc = mvc;
	}

	/**
	 * Tests label mapping based on different types of labels.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void getLabelMapping() throws Exception {
		final MvcResult result = mvc.perform(get("/api" + LabelMappingsController.API_SERVER_LABEL_MAPPING_URL).contentType(MediaType.APPLICATION_JSON))
				.andDo(print()).andExpect(status().isOk()).andReturn();

		final String labelMappings = result.getResponse().getContentAsString();
		final Map<String, Map<String, String>> mappings = PojoMapper.jsonReaderFor(Map.class).readValue(labelMappings);

		final Map<LabelType, Map<String, String>> expectedMappings = LabelMappings.getLabelMappings();
		final Map<String, String> projectLabels = new TreeMap<>(expectedMappings.get(PROJECT_LABELS));
		assertFalse(projectLabels.isEmpty(), "Project label map must not be empty");
		final Map<String, String> technologyLabels = new TreeMap<>(expectedMappings.get(TECHNOLOGY_LABELS));
		assertFalse(technologyLabels.isEmpty(), "Technology label map must not be empty");
		final Map<String, String> typeLabels = new TreeMap<>(expectedMappings.get(TYPE_LABELS));
		assertFalse(typeLabels.isEmpty(), "Type label map must not be empty");
		final Map<String, String> metadataLabels = new TreeMap<>(expectedMappings.get(ANNOTATION_METADATA_REASON_LABELS));
		assertFalse(metadataLabels.isEmpty(), "Type label map must not be empty");
		final Map<String, String> relationshipLabels = new TreeMap<>(expectedMappings.get(MODULE_DEPENDENCIES_RELATIONSHIP_LABELS));
		assertFalse(relationshipLabels.isEmpty(), "Relationship label map must not be empty");
		final Map<String, String> annotationStateLabels = new TreeMap<>(expectedMappings.get(ANNOTATION_STATES));
		assertFalse(annotationStateLabels.isEmpty(), "Annotation State label map must not be empty");
		final Map<String, String> definedLocations = new TreeMap<>(expectedMappings.get(DEFINED_LOCATION_LABELS));
		assertFalse(definedLocations.isEmpty(), "Defined Location label map must not be empty");
		final Map<String, String> moduleTypes = new TreeMap<>(expectedMappings.get(MODULE_TYPE_LABELS));
		assertFalse(moduleTypes.isEmpty(), "Module type label map must not be empty");

		assertEquals(technologyLabels, new TreeMap<>(mappings.get(TECHNOLOGY_LABELS.toString())), "Technology label map must match");
		assertEquals(typeLabels, new TreeMap<>(mappings.get(TYPE_LABELS.toString())), "Type label map must match");
		assertEquals(projectLabels, new TreeMap<>(mappings.get(PROJECT_LABELS.toString())), "Project label map must match");
		assertEquals(metadataLabels, new TreeMap<>(mappings.get(ANNOTATION_METADATA_REASON_LABELS.toString())), "Annotation MetaData label map must match");
		assertEquals(relationshipLabels, new TreeMap<>(mappings.get(MODULE_DEPENDENCIES_RELATIONSHIP_LABELS.toString())), "Relationship label map must match");
		assertEquals(annotationStateLabels, new TreeMap<>(mappings.get(ANNOTATION_STATES.toString())), "Annotation State label map must match");
		assertEquals(definedLocations, new TreeMap<>(mappings.get(DEFINED_LOCATION_LABELS.toString())), "Defined Location label map must match");
		assertEquals(moduleTypes, new TreeMap<>(mappings.get(MODULE_TYPE_LABELS.toString())), "Module type label map must match");
	}
}
