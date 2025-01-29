/* Copyright (c) 2023 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.properties.KeycloakApplicationConfiguration;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SavedSearchService;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SavedSearchPojo;
import innowake.mining.shared.entities.SavedSearchPojoPrototype;
import innowake.mining.shared.model.SavedSearchCountResponse;
import innowake.mining.shared.model.ScopeEnum;

/**
 * Contains tests for {@link SavedSearchController}
 */
@AutoConfigureMockMvc
@TestMethodOrder(OrderAnnotation.class)
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class SavedSearchControllerTests extends DatabaseRelatedTest {
	
	@Autowired
	private MockMvc mvc;
	@Autowired
	private SavedSearchService savedSearchService;
	@MockBean
	@Nullable
	private KeycloakApplicationConfiguration config;
	
	private EntityId projectId = EntityId.VOID;
	private static final String CREATE_SAVEDSEARCH_URL = "/api" + SavedSearchController.SAVED_SEARCH_COLLECTION_URL;
	private static final String FIND_ALL_SAVEDSEARCH_URL = "/api" + SavedSearchController.SAVED_SEARCH_BY_USAGE;
	private static final String DELETE_UPDATE_SAVEDSARCH_URL = "/api" + SavedSearchController.SAVED_SEARCH_BY_ID;
	public static final String SAVED_SEARCH_COUNTS_BY_PROJECT_ID = "/api" + SavedSearchController.SAVED_SEARCH_COUNTS_BY_PROJECT_ID;

	@BeforeAll
	void initialize() {
		projectId = loadProjectAndClient("Mock Project 1", EntityId.of(1L));
	}

	/**
	 * Tests for creating new and duplicate {@link SavedSearch} of same name and scope
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	@Order(1)
	void testCreateNewAndDuplicateSavedSearch() throws Exception {
		final var savedSearch = newSavedSearch("Individual Saved Search", "miningUi.annotationsTable", projectId, "saved search",
				Collections.emptyList(), null, ScopeEnum.INDIVIDUAL, null);
		/* Creating a saved search named Individual Saved Search with scope Individual */
		mvc.perform(post(CREATE_SAVEDSEARCH_URL, projectId.getUid()).contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(savedSearch)))
				.andExpect(status().isCreated()).andExpect(jsonPath("$.name").value("Individual Saved Search"));
		savedSearch.setName("Project Saved Search");
		savedSearch.setScope(ScopeEnum.PROJECT);
		/* Creating a saved search named Project Saved Search with scope Project */
		mvc.perform(post(CREATE_SAVEDSEARCH_URL, projectId.getUid()).contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(savedSearch)))
				.andExpect(status().isCreated()).andExpect(jsonPath("$.name").value("Project Saved Search"));
		savedSearch.setName("Client Saved Search");
		savedSearch.setScope(ScopeEnum.CLIENT);
		/* Creating a saved search named Client Saved Search with scope Client */
		mvc.perform(post(CREATE_SAVEDSEARCH_URL, projectId.getUid()).contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(savedSearch)))
				.andExpect(status().isCreated()).andExpect(jsonPath("$.name").value("Client Saved Search"));

		/* Trying to create a duplicate saved search named Client Saved Search with scope Client */
		assertEquals(
				"Constraint violation. Reason: Saved search with name Client Saved Search, scope CLIENT and usage miningUi.annotationsTable already exists",
				Objects.requireNonNull(mvc.perform(
								post(CREATE_SAVEDSEARCH_URL, projectId.getUid()).contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(savedSearch)))
						.andExpect(status().isBadRequest()).andReturn().getResolvedException()).getMessage());
	}

	/**
	 * Tests for creating a new {@link SavedSearch} scoped Global on project 0
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	@Order(2)
	void testCreateGlobalSavedSearch() throws Exception {		
		var savedSearch = newSavedSearch("Global Saved Search Non Project 0", "miningUi.modulesTable", projectId, "saved search",
				Collections.emptyList(), null, ScopeEnum.GLOBAL, null);
		/* Trying to create a Global saved search non Project 0 */
		mvc.perform(post(CREATE_SAVEDSEARCH_URL, projectId.getUid()).contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(savedSearch)))
				.andExpect(status().isForbidden()).andExpect(result -> result.getResponse().getContentAsString()
						.contains("Saved search with scope GLOBAL can only be created/updated for project 0"));
		/* Creating a Global saved search Project 0 */
		savedSearch = newSavedSearch("Global Saved Search Project 0", "miningUi.modulesTable", EntityId.of(0l), "saved search",
				Collections.emptyList(), null, ScopeEnum.GLOBAL, null);
		mvc.perform(post(CREATE_SAVEDSEARCH_URL, projectId.getUid()).contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(savedSearch)))
				.andExpect(status().isCreated()).andExpect(jsonPath("$.name").value("Global Saved Search Project 0"));
	}

	/**
	 * Tests for listing all {@link SavedSearch} using usage module
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	@Order(3)
	void testFindByUsage() throws Exception {
		final MvcResult result = mvc.perform(get(FIND_ALL_SAVEDSEARCH_URL, projectId.getUid(), "miningUi.annotationsTable").contentType(MediaType.APPLICATION_JSON))
				.andDo(print()).andExpect(status().isOk()).andReturn();
		final String savedSearches = result.getResponse().getContentAsString();
		final List<SavedSearchPojo> savedSearchList = PojoMapper.jsonReaderFor(new TypeReference<List<SavedSearchPojo>>() {}).readValue(savedSearches);
		assertEquals(Integer.valueOf(7), savedSearchList.size());
	}
	
	/**
	 * Tests for deletion of a {@link SavedSearch}
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	@Order(4)
	void testDeleteSavedSearch() throws Exception {
		final var savedSearch = savedSearchService.findAny(q -> q.ofProject(EntityId.of(0l))
																 .withName("Global Saved Search Project 0")
																 .withUsage("miningUi.modulesTable")
																 .withScope(ScopeEnum.GLOBAL));
		if (savedSearch.isPresent()) {
			mvc.perform(delete(DELETE_UPDATE_SAVEDSARCH_URL, projectId.getUid(), savedSearch.get().getId())).andExpect(status().isNoContent());
		} else {
			fail("Saved Search to be deleted was not found.");
		}

	}

	/**
	 * Tests for update of a {@link SavedSearch} with scope Project to Client
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	@Order(5)
	void testUpdateSavedSearch() throws Exception {
		var savedSearch = newSavedSearch("Project Saved Search", "miningUi.modulesTable", projectId, "saved search",
				Collections.emptyList(), null, ScopeEnum.PROJECT, null);
		/* Create saved search named Project Saved Search with scope Project */
		mvc.perform(post(CREATE_SAVEDSEARCH_URL, projectId.getUid()).contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(savedSearch)))
				.andExpect(status().isCreated());
		final var createdSavedSearch = savedSearchService.findAny(q -> q.ofProject(projectId)
																		.withName("Project Saved Search")
																		.withUsage("miningUi.modulesTable")
																		.withScope(ScopeEnum.PROJECT));
		assertNotNull(createdSavedSearch);
		/* Update the saved search named Project Saved Search with name Client Saved Search and scope Client */
		savedSearch = newSavedSearch("Client Saved Search", "miningUi.modulesTable", projectId, "saved search",
				Collections.emptyList(), null, ScopeEnum.CLIENT, null);
		mvc.perform(post(CREATE_SAVEDSEARCH_URL, projectId.getUid()).contentType("application/json").content(PojoMapper.jsonWriter().writeValueAsString(savedSearch)))
				.andExpect(status().isCreated()).andExpect(jsonPath("$.name").value("Client Saved Search")).andExpect(jsonPath("$.scope").value("CLIENT"));
	}

	/**
	 * Tests for listing all {@link SavedSearch} using ProjectId
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	@Order(6)
	void testFindByProjectId() throws Exception {
		final MvcResult result = mvc.perform(get(SAVED_SEARCH_COUNTS_BY_PROJECT_ID, projectId.getUid()).contentType(MediaType.APPLICATION_JSON))
				.andDo(print()).andExpect(status().isOk()).andReturn();
		final String savedSearches = result.getResponse().getContentAsString();
		final List<SavedSearchCountResponse> savedSearchCountResponseList = PojoMapper.jsonReaderFor(new TypeReference<List<SavedSearchCountResponse>>() {}).readValue(savedSearches);
		assertEquals(10, savedSearchCountResponseList.size());
	}
	
	private EntityId loadProjectAndClient(final String name, final EntityId clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName(name)
				.setClient(clientId)
				.setNatures(new HashSet<>(Collections.emptyList()))
			).identity();
	}

	private static SavedSearchPojoPrototype newSavedSearch(final String name, final String usage, @Nullable final EntityId projectId, final String savedSearch,
			final List<String> modifiers, @Nullable final String createdByUserId, final ScopeEnum scope, @Nullable final EntityId clientId) {
		return new SavedSearchPojoPrototype()
				.setId(-1l)
				.setName(name)
				.setUsage(usage)
				.setSavedSearch(savedSearch)
				.setModifiers(modifiers)
				.setScope(scope)
				.setClient(clientId)
				.setProject(projectId)
				.setCreatedByUserId(createdByUserId);
	}
}
