/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.server.controller.ReferenceController.MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL;
import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.List;

import javax.persistence.PersistenceException;

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
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Test {@link ReferenceController} endpoints and authorization
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class ReferenceControllerTests extends DatabaseRelatedTest {

	@Autowired
	private MockMvc mvc;

	@Autowired
	private ObjectMapper objectMapper;
	
	@Autowired
	private ModuleService moduleService;
	
	final static EntityId PROJECT_ID_1 = EntityId.of(1L);
	final static EntityId PROJECT_ID_2 = EntityId.of(2L);
	
	@Test
	void testFindReferencesByModuleIdsWithValidModules() throws Exception {
		/* Create modules associated with the project */
		final var module_1 =  createModule("Test Module 1", PROJECT_ID_1);
		final var module_2 =  createModule("Test Module 2", PROJECT_ID_1);
		final ModuleRelationshipPojo reference = createReference(module_1.identity(), module_2.identity());
		
		final MvcResult intialResult = mvc.perform(get("/api" + MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL, PROJECT_ID_1.getNid(), module_1.getId(), 
											module_2.getId()).param("relationship", "CALLS").contentType(MediaType.APPLICATION_JSON)).andDo(print())
											.andExpect(status().isOk()).andReturn();
		final var references = objectMapper.readValue(intialResult.getResponse().getContentAsString(), new TypeReference<List<ModuleRelationshipPojo>>() {});
		assertEquals(1, references.size());
		assertEquals(reference.getId(), references.get(0).getId());
	}
	
	@Test
	void testFindReferencesByModuleIdsWithInvalidModules() throws Exception {
		/* Create module associated with the project */
		final var module_1 =  createModule("Test Module 1", PROJECT_ID_1);
		/* Create module associated with different project */
		final var module_2 =  createModule("Test Module 2", PROJECT_ID_2);
		
		mvc.perform(get("/api" + MODULE_REFERENCES_BY_FROM_AND_TO_MODULE_IDS_URL, PROJECT_ID_1.getNid(), module_1.getId(), 
				module_2.getId()).param("relationship", "CALLS").contentType(MediaType.APPLICATION_JSON)).andDo(print())
				.andExpect(status().isNotFound())
				.andReturn();
	}
	
	@Test
	void testReferenceWithModuleInOtherProject() {
		final var module_1 = createModule("Test Module 1", PROJECT_ID_1).identity();
		final var module_2 = createModule("Test Module 2", PROJECT_ID_2).identity();
		assertThrows(PersistenceException.class, () -> createReference(module_1, module_2));
	}
	
	private ModuleRelationshipPojo createReference(final EntityId fromId, final EntityId toId) {
		final int fromOffset = 0;
		final int fromLength = 42;
		final int toOffset = 666;
		final int toLength = 1337;
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype()
				.setSrcModule(fromId)
				.setSrcLocation(new ModuleLocation(fromOffset, fromLength))
				.setDstModule(toId)
				.setDstLocation(new ModuleLocation(toOffset, toLength))
				.setRelationship(RelationshipType.CALLS);

		return moduleService.getRelationship(moduleService.createRelationship(reference));
	}

	private ModulePojo createModule(final String moduleName, final EntityId projectId) {
		final ModulePojoPrototype testModule = new ModulePojoPrototype();
		testModule.setProject(projectId);
		testModule.setName(moduleName);
		testModule.setTechnology(Technology.COBOL);
		testModule.setType(Type.PROGRAM);
		testModule.setStorage(Storage.FILE);
		testModule.setIdentification(Identification.IDENTIFIED);
		testModule.setOrigin(Origin.CUSTOM);
		testModule.setRepresentation(Representation.PHYSICAL);
		testModule.setCreator(Creator.DISCOVERY);
		final var id = moduleService.create(testModule);
		
		return moduleService.getModule(id);
	}

}
