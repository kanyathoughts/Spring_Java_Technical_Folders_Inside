/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.server.controller.UniversalSearchController.UNIVERSAL_SEARCH_URL;
import static innowake.mining.shared.universalsearch.UniversalSearchLink.Type.CODE_VIEWER;
import static innowake.mining.shared.universalsearch.UniversalSearchLink.Type.MODULE_DETAILS;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DataDictionaryVariableAttribute;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.universalsearch.UniversalSearchResult;

/**
 * Contains tests for {@link UniversalSearchController}
 */
@AutoConfigureMockMvc
@ActiveProfiles(value =  Profiles.NO_AUTH , inheritProfiles = false )
class UniversalSearchControllerTests extends DatabaseRelatedTest {
	
	@Autowired
	private MockMvc mvc;
	
	@Autowired
	private ObjectMapper objectMapper;
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private DataDictionaryService dataDictionaryService;
	
	final static EntityId PROJECT_ID = EntityId.of(1L);
	
	@BeforeEach
	void cleanUp() throws IOException {
		resetTestData();
	}
	
	@Test
	void testSearchModuleNameFound() throws Exception {
		/* Create modules associated with the project */
		final ModulePojo module = createModule("testModule1", PROJECT_ID);
		final String query = "Module";
		
		final MvcResult mvcResult = mvc.perform(get("/api" + UNIVERSAL_SEARCH_URL, PROJECT_ID.getNid()).param("query", query)
											.contentType(MediaType.APPLICATION_JSON)).andDo(print())
											.andExpect(status().isOk()).andReturn();
		final String content = mvcResult.getResponse().getContentAsString();
		final List<UniversalSearchResult> results = objectMapper.readValue(content, new TypeReference<List<UniversalSearchResult>>() {});
		assertEquals(1, results.size());
		final UniversalSearchResult searchResult = results.get(0);
		
		validateModule(module, searchResult);
	}
	
	@Test
	void testSearchMultipleModuleNamesFound() throws Exception {
		/* Create modules associated with the project */
		final ModulePojo module1 = createModule("testModule2", PROJECT_ID, "path1");
		final ModulePojo module2 = createModule("testModule23", PROJECT_ID, "path2");
		final String query = "Module2";
		
		final MvcResult mvcResult = mvc.perform(get("/api" + UNIVERSAL_SEARCH_URL, PROJECT_ID.getNid()).param("query", query)
											.contentType(MediaType.APPLICATION_JSON)).andDo(print())
											.andExpect(status().isOk()).andReturn();
		final String content = mvcResult.getResponse().getContentAsString();
		final List<UniversalSearchResult> results = objectMapper.readValue(content, new TypeReference<List<UniversalSearchResult>>() {}).stream()
				.sorted(Comparator.comparing(UniversalSearchResult::getSubTitle))
				.collect(Collectors.toList());
		assertEquals(2, results.size());
		validateModule(module1, results.get(0));
		validateModule(module2, results.get(1));
	}

	@Test
	void testSearchDataDictionaryElementFound() throws Exception {
		/* Create data dictionary associated with the project */
		final ModulePojo module =  createModule("testModule3", PROJECT_ID);
		final ModuleLocation moduleLocation = new ModuleLocation(6, 256);
		final DataDictionaryPojo dataDictionaryEntry = createDataDictionaryEntry("datadictionary3", module.identity(), moduleLocation);
		final String query = "dictionary";
		
		final MvcResult mvcResult = mvc.perform(get("/api" + UNIVERSAL_SEARCH_URL, PROJECT_ID.getNid()).param("query", query)
				.contentType(MediaType.APPLICATION_JSON)).andDo(print())
				.andExpect(status().isOk()).andReturn();
		final String content = mvcResult.getResponse().getContentAsString();
		final List<UniversalSearchResult> results = objectMapper.readValue(content, new TypeReference<List<UniversalSearchResult>>() {});
		assertEquals(1, results.size());
		final UniversalSearchResult searchResult = results.get(0);
		
		validateDataDictionary(module, moduleLocation, dataDictionaryEntry, searchResult);
	}
	
	@Test
	void testSearchDataDictionaryElementAndModuleFound() throws Exception {
		/* Create data dictionary associated with the project */
		final ModulePojo module =  createModule("testModule4", PROJECT_ID);
		final ModuleLocation moduleLocation = new ModuleLocation(6, 256);
		final DataDictionaryPojo dataDictionaryEntry = createDataDictionaryEntry("module4datadictionary", module.identity(), moduleLocation);
		final String query = "ule4";
		
		final MvcResult mvcResult = mvc.perform(get("/api" + UNIVERSAL_SEARCH_URL, PROJECT_ID.getNid()).param("query", query)
				.contentType(MediaType.APPLICATION_JSON)).andDo(print())
				.andExpect(status().isOk()).andReturn();
		final String content = mvcResult.getResponse().getContentAsString();
		final List<UniversalSearchResult> results = objectMapper.readValue(content, new TypeReference<List<UniversalSearchResult>>() {});
		assertEquals(2, results.size());
		
		for (final UniversalSearchResult searchResult: results) {
			if (searchResult.getProvidedBy().equals("data-dictionary")) {
				validateDataDictionary(module, moduleLocation, dataDictionaryEntry, searchResult);
			} else {
				validateModule(module, searchResult);
			}
		}
	}
	
	@Test
	void testDistributionOfResultBetweenProviders() throws Exception {
		/* Create modules and data dictionaries associated with the project */
		final List<ModulePojo> modules = new ArrayList<>();
		for (int i = 0; i < 10; i++) {
			modules.add(createModule("test_Module_5" + i, PROJECT_ID , "path_" + i));
		}
		final ModuleLocation moduleLocation1 = new ModuleLocation(6, 256);
		final ModuleLocation moduleLocation2 = new ModuleLocation(16, 126);
		createDataDictionaryEntry("test_DataDictionary_5_1", modules.get(0).identity(), moduleLocation1);
		createDataDictionaryEntry("test_DataDictionary_5_2", modules.get(1).identity(), moduleLocation2);
		final String query = "_5";
		
		final MvcResult mvcResult = mvc.perform(get("/api" + UNIVERSAL_SEARCH_URL, PROJECT_ID.getNid()).param("query", query)
				.contentType(MediaType.APPLICATION_JSON)).andDo(print())
				.andExpect(status().isOk()).andReturn();
		final String content = mvcResult.getResponse().getContentAsString();
		final List<UniversalSearchResult> results = objectMapper.readValue(content, new TypeReference<List<UniversalSearchResult>>() {});
		assertEquals(10, results.size());
		assertEquals(2, results.stream().filter(result -> result.getProvidedBy().equals("data-dictionary")).count());
		assertEquals(8, results.stream().filter(result -> result.getProvidedBy().equals("module-name-path")).count());
	}

	@Test
	void testSearchNotFound() throws Exception {
		final String query = "MMRS71";
		final MvcResult mvcResult = mvc.perform(get("/api" + UNIVERSAL_SEARCH_URL, PROJECT_ID.getNid()).param("query", query)
											.contentType(MediaType.APPLICATION_JSON)).andDo(print())
											.andExpect(status().isOk()).andReturn();
		final String content = mvcResult.getResponse().getContentAsString();
		final List<UniversalSearchResult> results = objectMapper.readValue(content, new TypeReference<List<UniversalSearchResult>>() {});
		assertTrue(results.isEmpty());
	}
	
	private ModulePojo createModule(final String moduleName, final EntityId projectId) {
		return createModule(moduleName, projectId, "test path");
	}
	
	private ModulePojo createModule(final String moduleName, final EntityId projectId, final String path) {
		return moduleService.getModule(moduleService.create(new ModulePojoPrototype()
			.setProject(projectId)
			.setName(moduleName)
			.setTechnology(Technology.COBOL)
			.setDescription("test module")
			.setType(Type.PROGRAM)
			.setPath(path)
			.setStorage(Storage.FILE)
			.setIdentification(Identification.IDENTIFIED)
			.setOrigin(Origin.CUSTOM)
			.setRepresentation(Representation.PHYSICAL)
			.setCreator(Creator.DISCOVERY)));
	}
	
	private DataDictionaryPojo createDataDictionaryEntry(final String dataElementName, final EntityId moduleId, final ModuleLocation moduleLocation) {
		final Map<DataDictionaryVariableScope, Map<String, String>> scopes = new HashMap<>();
		scopes.put(DataDictionaryVariableScope.SQL_DATABASE, null);
		scopes.put(DataDictionaryVariableScope.FILE, Collections.singletonMap(DataDictionaryVariableAttribute.FILE_DATASET.name(), "My data set name"));
		scopes.put(DataDictionaryVariableScope.CICS_UI, null);
		scopes.put(DataDictionaryVariableScope.OTHER, Collections.singletonMap("SCOPE_1", "TEST SOURCE"));
		scopes.put(DataDictionaryVariableScope.PARAMETER, null);
		
		return dataDictionaryService.create(new DataDictionaryPojoPrototype()
				.setModule(moduleId)
				.setLocation(moduleLocation)
				.setName(dataElementName)
				.setScopes(scopes)
				.setDescription("MY description")
				.setFormat("PICX")
				.setLength(58L)
				.setParentGroup("XTAX-PRD")
				.setCreatedByUserId("admin")
				.setPicClause("TEST PIC CLAUSE")
				.setDefinedLocation(DefinedLocation.PROGRAM)
				.setIsBusiness(true)
				.setState(WorkingState.CANDIDATE)
				.setFieldTransformation("TEST TRANSFORMATION")
				.setSourceInput("TEST INPUT")
				.setTargetOutput("TEST OUTPUT")
				.setUsage("COMP1")
				.setFieldLevel(56L));
	}
	
	private void validateDataDictionary(final ModulePojo module, final ModuleLocation moduleLocation, final DataDictionaryPojo dataDictionaryEntry,
			final UniversalSearchResult searchResult) {
		assertEquals("data-dictionary", searchResult.getProvidedBy());
		assertEquals(0, searchResult.getRank());
		assertEquals(dataDictionaryEntry.getName(), searchResult.getTitle());
		assertEquals(module.getPath().orElse(null), searchResult.getSubTitle());
		assertEquals(dataDictionaryEntry.getDescription(), searchResult.getContext());
		assertEquals("Data Dictionary", searchResult.getType());
		assertEquals(CODE_VIEWER, searchResult.getLinks().get(0).getType());
		assertTrue(searchResult.getLinks().get(0).getProperties().containsKey("moduleId"));
		assertEquals(dataDictionaryEntry.getModule().getNid().toString(), searchResult.getLinks().get(0).getProperties().get("moduleId"));
		assertTrue(searchResult.getLinks().get(0).getProperties().containsKey("offset"));
		assertEquals(moduleLocation.getOffset().toString(), searchResult.getLinks().get(0).getProperties().get("offset"));
		assertTrue(searchResult.getLinks().get(0).getProperties().containsKey("length"));
		assertEquals(moduleLocation.getLength().toString(), searchResult.getLinks().get(0).getProperties().get("length"));
	}
	
	private void validateModule(final ModulePojo module, final UniversalSearchResult searchResult) {
		assertEquals("module-name-path", searchResult.getProvidedBy());
		assertEquals(0, searchResult.getRank());
		assertEquals(module.getName(), searchResult.getTitle());
		assertEquals(module.getPath().orElse(null), searchResult.getSubTitle());
		assertEquals(module.getDescription().orElse(null), searchResult.getContext());
		assertEquals("Module", searchResult.getType());
		assertEquals(MODULE_DETAILS, searchResult.getLinks().get(0).getType());
		assertTrue(searchResult.getLinks().get(0).getProperties().containsKey("moduleId"));
		assertEquals(module.getId().toString(), searchResult.getLinks().get(0).getProperties().get("moduleId"));
	}

}
