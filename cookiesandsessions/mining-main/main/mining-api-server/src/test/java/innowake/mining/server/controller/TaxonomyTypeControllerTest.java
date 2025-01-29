/* Copyright (c) 2024 Deloitte. All rights reserved.*/
package innowake.mining.server.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.entities.TaxonomyTypePojo;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Tests the {@linkplain TaxonomyTypeController} class.
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class TaxonomyTypeControllerTest extends DatabaseRelatedTest {

	@Autowired
	private MockMvc mvc;

	@Test
	void testTaxonomyTypeCreation() throws Exception {
		/* This test case is to check if the taxonomy type creation is successful */
		final String content = "{\"name\":\"Test Created\",\"project\":1,\"categoryId\":4}";

		mvc.perform(post("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
						.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isCreated());
	}

	@ParameterizedTest(name = "Test failed for {2} with content = {0}, expected status = {1}")
	@MethodSource("provideInvalidTaxonomyTypeCreationData")
	void testTaxonomyTypeCreationWithInvalidData(final String content, final HttpStatus expectedStatus, @SuppressWarnings("unused") final String reason) throws Exception {
		/* This test case is to check if the create request fails when the request data is invalid */
		mvc.perform(post("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
						.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().is(expectedStatus.value()));
	}

	@Test
	void testCreationWithExistingTaxonomyTypeName() throws Exception {
		/* This test case is to check if the request fails when the taxonomy type with the same name already exists */
		final String content = "{\"name\":\"testCreateWithExistingTaxonomyType\",\"project\":1,\"categoryId\":4}";

		mvc.perform(post("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
						.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isCreated());
		mvc.perform(post("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
						.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isBadRequest());
	}

	@Test
	void testTaxonomyTypeUpdate() throws Exception {
		/* To test the update first create a taxonomy type and then update it */
		String content = "{\"name\":\"Test\",\"project\":1,\"categoryId\":4}";
		final String response =
				mvc.perform(post("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
								.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isCreated()).andReturn().getResponse().getContentAsString();

		/* updating the taxonomy type and checking if the update is successful or not*/
		final TaxonomyTypePojo taxonomyTypePojo = new ObjectMapper().readValue(response, TaxonomyTypePojo.class);
		content = "{\"id\":\""+ taxonomyTypePojo.getId() +"\",\"name\":\"Test Updated\",\"project\":1,\"categoryId\":4}";

		mvc.perform(put("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
						.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isOk());
	}

	@ParameterizedTest(name = "Test failed for {2} with content = {0}, expected status = {1}")
	@MethodSource("provideInvalidTaxonomyTypeUpdateData")
	void testTaxonomyTypeUpdateWithInvalidData(final String content, final HttpStatus expectedStatus, @SuppressWarnings("unused") final String reason) throws Exception {
		/* This test case is to update the taxonomy type with invalid data and check if the request fails */
		mvc.perform(put("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
						.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().is(expectedStatus.value()));
	}

	@Test
	void testTaxonomyFindAll() throws Exception {
		/* first create some taxonomy types */
		final List<String> expectedTaxonomyTypeNames = Arrays.asList("Test1", "Test2", "Test3", "Test4");
		final String[] contents = {"{\"name\":\"Test1\",\"project\":1,\"categoryId\":4}",
				"{\"name\":\"Test2\",\"project\":1,\"categoryId\":4}",
				"{\"name\":\"Test3\",\"project\":1,\"categoryId\":4}",
				"{\"name\":\"Test4\",\"project\":1,\"categoryId\":4}"};
		for (final String content : contents) {
			mvc.perform(post("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
							.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isCreated());
		}

		/* Now retrieve all the taxonomy types and check if the created taxonomy types are present or not */
		final String response = mvc.perform(get("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1)
						.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isOk()).andReturn().getResponse().getContentAsString();

		final TaxonomyTypePojo[] taxonomyTypePojoList = new ObjectMapper().readValue(response, TaxonomyTypePojo[].class);
		final List<String> actualTaxonomyTypeNames = Arrays.stream(taxonomyTypePojoList).map(TaxonomyTypePojo::getName).collect(Collectors.toList());
		assert actualTaxonomyTypeNames.containsAll(expectedTaxonomyTypeNames);
	}

	@Test
	void testFindAllWithNonExistentProject() throws Exception {
		/* This test case is to check if the request fails when the project ID is invalid */
		mvc.perform(get("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, -1)
						.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isNotFound());
	}

	@Test
	void testTaxonomyDelete() throws Exception {
		/* To test deletion first we have to create a taxonomy type */
		final String content = "{\"name\":\"TestDeletion\",\"project\":1,\"categoryId\":4}";
		mvc.perform(post("/api" + TaxonomyTypeController.TAXONOMY_TYPE_COLLECTIONS_URL, 1).content(content)
						.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isCreated());

		/* Now delete the taxonomy type and check if the deletion is successful or not */
		mvc.perform(delete("/api" + TaxonomyTypeController.TAXONOMY_TYPE_BY_NAME_URL, 1, "TestDeletion").content(content)
						.contentType(APPLICATION_JSON)).andDo(print()).andExpect(status().isOk());
	}

	private static Stream<Arguments> provideInvalidTaxonomyTypeCreationData() {
		return Stream.of(
				Arguments.of("{\"name\":\"TestCreationWithInvalidData\",\"project\":-1,\"categoryId\":4}", HttpStatus.BAD_REQUEST, "Invalid Project ID"),
				Arguments.of("{\"name\":\"TestCreationWithInvalidData\",\"project\":1,\"categoryId\":-1}", HttpStatus.BAD_REQUEST, "Invalid Category ID"),
				Arguments.of("{\"name\":\"\",\"project\":1,\"categoryId\":4}", HttpStatus.BAD_REQUEST, "Invalid Name"),
				Arguments.of("{\"name\":\"TestCreationWithInvalidData\"}", HttpStatus.BAD_REQUEST, "Invalid Request"),
				Arguments.of("{\"project\":1,\"categoryId\":4}", HttpStatus.BAD_REQUEST, "Null Names")
		);
	}

	private static Stream<Arguments> provideInvalidTaxonomyTypeUpdateData() {
		return Stream.of(
				Arguments.of("{\"id\":-1,\"name\":\"Test Updated\",\"project\":1,\"categoryId\":4}", HttpStatus.BAD_REQUEST, "Invalid ID"),
				Arguments.of("{\"id\":1,\"name\":\"Test Updated\",\"project\":-1,\"categoryId\":4}", HttpStatus.BAD_REQUEST, "Invalid Project ID"),
				Arguments.of("{\"id\":-1,\"name\":\"Test Updated\",\"project\":1,\"categoryId\":4}", HttpStatus.BAD_REQUEST, "Non-Existent Taxonomy Type"),
				Arguments.of("{\"id\":1,\"name\":\"Test Updated\",\"project\":1,\"categoryId\":-1}", HttpStatus.BAD_REQUEST, "Invalid Category ID"),
				Arguments.of("{\"id\":1,\"name\":\"\",\"project\":1,\"categoryId\":4}", HttpStatus.BAD_REQUEST, "Invalid Name"),
				Arguments.of("{\"id\":1,\"name\":\"Test Updated\",\"project\":1}", HttpStatus.BAD_REQUEST, "Invalid Request")
		);
	}
}
