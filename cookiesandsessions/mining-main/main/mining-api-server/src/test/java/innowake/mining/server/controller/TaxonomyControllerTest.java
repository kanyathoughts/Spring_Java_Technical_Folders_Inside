/* Copyright (c) 2022 Deloitte. All rights reserved.*/
package innowake.mining.server.controller;

import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;
import innowake.mining.shared.model.taxonomy.assignment.TaxonomyAssignmentsSetRequest;

@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class TaxonomyControllerTest extends DatabaseRelatedTest {

	@Autowired
	private MockMvc mvc;

	/**
	 * Tests {@link TaxonomyController#updateTaxonomyAssignmentJob(HttpServletRequest, HttpServletResponse, EntityId, TaxonomyAssignmentsSetRequest)}
	 * for a selected modules.
	 * 
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void taxonomyAssignmentJobControllerTest() throws Exception {
		final TaxonomyAssignmentsSetRequest request = new TaxonomyAssignmentsSetRequest(new ModuleMatcher(null, null), Collections.emptyList());
		final String content = new ObjectMapper().writeValueAsString(request);

		mvc.perform(
				put("/api" + TaxonomyController.ASSIGN_TAXONOMY_JOB_URL, 1L)
				.content(content)
				.contentType(APPLICATION_JSON))
				.andDo(print())
				.andExpect(status().isOk());
	}

	@Test
	void testCreateTaxonomyWithNullName() throws Exception {
		/* This test case is to check if the taxonomy type creation is successful */
		final String content = "{\"name\":\"TestTypeCreated\",\"project\":1,\"categoryId\":4}";

		final String response =  mvc.perform(post("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
				.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isCreated()).andReturn().getResponse().getContentAsString();

		TaxonomyTypePojo taxonomyTypePojo = new ObjectMapper().readValue(response, TaxonomyTypePojo.class);

		/* This test case is to check if the taxonomy creation is successful */
		final String taxonomies = "{\"name\":null,\"project\":1,\"type\":\"" + taxonomyTypePojo.getId() + "\"}";
		mvc.perform(post("/api/v1/projects/{projectId}/taxonomies", 1L)
								.contentType(APPLICATION_JSON)
								.content(taxonomies))
				.andExpect(status().isBadRequest());
	}

	@Test
	void testCreateTaxonomyWithEmptyName() throws Exception {
		/* This test case is to check if the taxonomy type creation is successful */
		final String content = "{\"name\":\"NewTestTypeCreated\",\"project\":1,\"categoryId\":4}";

		final String response =  mvc.perform(post("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
				.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isCreated()).andReturn().getResponse().getContentAsString();

		TaxonomyTypePojo taxonomyTypePojo = new ObjectMapper().readValue(response, TaxonomyTypePojo.class);

		/* This test case is to check if the taxonomy creation is successful */
		final String taxonomies = "{\"name\":\"\",\"project\":1,\"type\":\""+taxonomyTypePojo.getId()+"\"}";
		mvc.perform(post("/api/v1/projects/{projectId}/taxonomies", 1L)
								.contentType(APPLICATION_JSON)
								.content(taxonomies))
				.andExpect(status().isBadRequest());
	}
}
