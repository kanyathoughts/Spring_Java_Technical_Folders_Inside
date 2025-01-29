/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableMap;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.FieldInfoPojo;
import innowake.mining.shared.entities.FieldInfoPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;

/**
 * Tests for {@link DataSchemaController}
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class DataSchemaControllerTests extends DatabaseRelatedTest {
	
	@Autowired
	private ClientService clientService;
	
	@Autowired
	private MockMvc mvc;
	
	@Autowired
	private FieldInfoService fieldInfoService;
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private ObjectMapper objectMapper;
	
	private Long projectId = Long.valueOf(-1);	
	private Long projectIdWithNoModule = Long.valueOf(-1);
	private EntityId mockModuleId = EntityId.of(-1l);
	private static final String COMMENT = "comment";
	private final Map<String, String> properties = ImmutableMap.of(COMMENT, "Mock Comment");
	private static final String UPDATE_SCHEMA_FIELD_URL = "/api" + DataSchemaController.UPDATE_SCHEMA_FIELD_URL;
	private static final String SCHEMA_FIELDS_URL = "/api" + DataSchemaController.SCHEMA_FIELDS_URL;
	private static final Integer ONE = Integer.valueOf(1);
	private static final Integer NEGATIVE_ONE = Integer.valueOf(-1);
	
	@BeforeAll
	void initialize() {
		loadTestData();
	}
	
	/**
	 * Tests for update and get comment for FieldInfo when ordinal exists for the given moduleId.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testUpdateAndGetCommentForFieldInfo() throws Exception {		
		mvc.perform(put(UPDATE_SCHEMA_FIELD_URL, projectId, mockModuleId.getNid(), ONE)
				.contentType(MediaType.APPLICATION_JSON)
				.content(objectMapper.writeValueAsString(properties)))
				.andExpect(status().isOk())
				.andDo(MockMvcResultHandlers.print());
		final MvcResult result = mvc.perform(get(SCHEMA_FIELDS_URL, projectId, mockModuleId.getNid()).contentType(MediaType.APPLICATION_JSON)).andDo(print())
				.andExpect(status().isOk()).andReturn();
		final String fieldInfo = result.getResponse().getContentAsString();
		final List<FieldInfoPojo> fieldInfoList = objectMapper.readValue(fieldInfo, new TypeReference<List<FieldInfoPojo>>() {});
		assertEquals(ONE, fieldInfoList.size());
		assertEquals(properties.get(COMMENT), fieldInfoList.get(0).getComment().orElse(null));
	}
	
	/**
	 * Tests to check error while updating comment of FieldInfo when moduleId is of different project.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testUpdateCommentForFieldInfoForErrorWhenModuleIdOfDifferentProjectId() throws Exception {		
		mvc.perform(put(UPDATE_SCHEMA_FIELD_URL, projectIdWithNoModule, mockModuleId.getNid(), ONE)
				.contentType(MediaType.APPLICATION_JSON)
				.content(objectMapper.writeValueAsString(properties)))
				.andExpect(status().isNotFound());
	}
	
	/**
	 * Tests to check error while updating comment of FieldInfo when ordinal not present for the moduleId.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testUpdateCommentForFieldInfoForErrorWhenWrongOrdinal() throws Exception {
		mvc.perform(put(UPDATE_SCHEMA_FIELD_URL, projectId, mockModuleId.getNid(), NEGATIVE_ONE)
				.contentType(MediaType.APPLICATION_JSON)
				.content(objectMapper.writeValueAsString(properties)))
				.andExpect(status().isNotFound());
	}
	
	/**
	 * Tests to check error while updating comment of FieldInfo when moduleId does not exists.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testUpdateCommentForFieldInfoForErrorWhenNoModule() throws Exception {
		mvc.perform(put(UPDATE_SCHEMA_FIELD_URL, projectId, NEGATIVE_ONE, ONE)
				.contentType(MediaType.APPLICATION_JSON)
				.content(objectMapper.writeValueAsString(properties)))
				.andExpect(status().isNotFound());
	}
	
	private Long loadProjectAndClient(final String name, final Long clientId) {
		return projectService.create(new ProjectPojoPrototype()
				.setName(name)
				.setClient(clientService.get(EntityId.of(clientId), true).identity())
				.setNatures(new HashSet<>(Collections.emptyList()))
			).getId();
	}
	
	private void loadTestData() {
		projectId = assertNotNull(loadProjectAndClient("Mock Project 1", Long.valueOf(1)));
		mockModuleId = createMockModule("Mock Module", "/cobol/programs/mock.cbl", ModuleType.COBOL_PROGRAM, projectId);
		fieldInfoService.create(new FieldInfoPojoPrototype()
										.setModule(mockModuleId)
										.setOrdinal(1)
										.setName("Mock Field"));
		projectIdWithNoModule = assertNotNull(loadProjectAndClient("Mock Project 2", Long.valueOf(1)));
	}
	
	private EntityId createMockModule(final String name, @Nullable final String path, final ModuleType moduleType, final Long projectId1) {
		final ModulePojoPrototype cobolProgram = new ModulePojoPrototype();
		cobolProgram.setProject(EntityId.of(projectId1));
		cobolProgram.setName(name);
		cobolProgram.setOrigin(Origin.CUSTOM);
		cobolProgram.setStorage(Storage.FILE);
		cobolProgram.setIdentification(Identification.IDENTIFIED);
		cobolProgram.setPath(path);
		cobolProgram.setTechnology(moduleType.getTechnology());
		cobolProgram.setType(moduleType.getType());
		cobolProgram.setCreator(Creator.DISCOVERY);
		return moduleService.create(cobolProgram);
	}
}
