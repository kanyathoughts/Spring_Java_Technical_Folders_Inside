/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.server.controller.ProjectController.FUNCTIONAL_BLOCK_REACHABILITY_CONFIG;
import static innowake.mining.server.controller.ProjectController.PROJECT_BY_ID_URL;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisConfig;
import org.junit.jupiter.api.Test;

import innowake.mining.shared.entities.ProjectPojo;

import java.util.List;

/**
 * Tests for {@link ProjectController}.
 */
class ProjectControllerTest extends BaseProjectControllerTest {

	/**
	 * Tests {@link ProjectController#findById(HttpServletRequest, HttpServletResponse, EntityId)} for a project that is linked with a client which was marked as
	 * to be deleted.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testFindByRecordIdForToBeDeletedClient() throws Exception {
		testClientMarkedAsDeleted(PROJECT_BY_ID_URL, ProjectPojo::getId);
	}
	
	/**
	 * Tests {@link ProjectController#findById} for a project that is linked with a client which was marked as
	 * to be deleted.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testFindByIdForToBeDeletedClient() throws Exception {
		testClientMarkedAsDeleted(PROJECT_BY_ID_URL, ProjectPojo::getId);
	}

	@Test
	void testSaveAndGetReachabilityAnalysisConfig() throws Exception {
		final ReachabilityAnalysisConfig config = new ReachabilityAnalysisConfig(List.of(ModuleType.JCL_JOB, ModuleType.ECL_JOB),
				List.of(ModuleType.SQL_TABLE, ModuleType.RESOURCE_FILE));
		final ObjectMapper objectMapper = new ObjectMapper();
		mvc.perform(post("/api" + FUNCTIONAL_BLOCK_REACHABILITY_CONFIG, 1L)
						.contentType(APPLICATION_JSON)
						.content(objectMapper.writeValueAsString(config)))
				.andExpect(status().isOk());

		final var response = mvc.perform(get("/api" + FUNCTIONAL_BLOCK_REACHABILITY_CONFIG, 1L)
						.contentType(APPLICATION_JSON))
				.andExpect(status().isOk())
				.andReturn();

		final ReachabilityAnalysisConfig responseConfig = objectMapper.readValue(response.getResponse().getContentAsString(), ReachabilityAnalysisConfig.class);
		assertEquals(config.getUpperBoundModuleTypes(), responseConfig.getUpperBoundModuleTypes());
		assertEquals(config.getLowerBoundModuleTypes(), responseConfig.getLowerBoundModuleTypes());
	}

	@Test
	void testGetReachabilityAnalysisConfigReset() throws Exception {
		final ObjectMapper objectMapper = new ObjectMapper();
		final var response = mvc.perform(get("/api" + FUNCTIONAL_BLOCK_REACHABILITY_CONFIG, 1L)
						.contentType(APPLICATION_JSON)
						.param("reset", "true"))
				.andExpect(status().isOk())
				.andReturn();

		final var config = ReachabilityAnalysisConfig.defaultConfig();
		final ReachabilityAnalysisConfig responseConfig = objectMapper.readValue(response.getResponse().getContentAsString(), ReachabilityAnalysisConfig.class);
		assertEquals(config.getUpperBoundModuleTypes(), responseConfig.getUpperBoundModuleTypes());
		assertEquals(config.getLowerBoundModuleTypes(), responseConfig.getLowerBoundModuleTypes());
	}
}
