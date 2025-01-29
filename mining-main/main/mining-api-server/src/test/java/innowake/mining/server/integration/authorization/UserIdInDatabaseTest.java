/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.CREATED;
import static org.springframework.http.HttpStatus.OK;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.request;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Tests validating the behavior of the user information saved in the database.
 * <p>
 * ATTENTION: This test class only resets the DB data once before running all the tests.
 */
class UserIdInDatabaseTest extends AbstractUserNameTest {
	
	private static final EntityId PROJECT_ONE = EntityId.of(1L);

	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private MockMvc mockMvc;

	@BeforeEach
	@Override
	void init() {
		setAuthentication(Arrays.asList(admin()));
		mockResponseExpectOk(TEST_USER_ID);
		super.init();
	}
	
	@Test
	void annoationV1() throws Exception {
		/* Create */
		final EntityId moduleId = createModule(PROJECT_ONE);
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(new AnnotationPojoPrototype()
				.setName("MINIMAL ANNOTATION")
				.setState(WorkingState.IN_ANALYSIS)
				.setType(AnnotationType.RULE)
				.setLocation(new ModuleLocation(10, 20)));
		final MvcResult postResult = mockMvc.perform(
					request(POST, "/api/v1/projects/{projectId}/modules/{moduleId}/annotations", PROJECT_ONE.getNid(), moduleId.getNid())
					.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation)
				)
				.andDo(print())
				.andExpect(status().is(CREATED.value()))
				.andReturn();
		
		final String createdJson = postResult.getResponse().getContentAsString();
		final AnnotationPojo createdEntity = PojoMapper.jsonReaderFor(AnnotationPojo.class).readValue(createdJson);
		assertEquals(TEST_USER_ID, createdEntity.getCreatedByUserId());
		assertEquals("", createdEntity.getUpdatedByUserName().orElse(""));
		
		/* Update */
		final String jsonForUpdate = PojoMapper.jsonWriter().writeValueAsString(new AnnotationPojoPrototype().withId(createdEntity.identity()));
		final MvcResult putResult = mockMvc.perform(
				request(PUT, "/api/v1/projects/{projectId}/annotations/{annotationId}", PROJECT_ONE.getNid(), createdEntity.getId())
				.contentType(MediaType.APPLICATION_JSON).content(jsonForUpdate)
			)
			.andDo(print())
			.andExpect(status().is(OK.value()))
			.andReturn();

		final String updatedJson = putResult.getResponse().getContentAsString();
		final AnnotationPojo updatedEntity = PojoMapper.jsonReaderFor(AnnotationPojo.class).readValue(updatedJson);
		assertEquals(TEST_USER_ID, updatedEntity.getCreatedByUserId());
		assertEquals(TEST_USER_ID, updatedEntity.getUpdatedByUserId().orElse(""));
	}
	
	@Test
	void dataDictionaryEntryV1() throws Exception {
		/* Create */
		final EntityId moduleId = createModule(PROJECT_ONE);
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(new DataDictionaryPojoPrototype()
				.setLocation(new ModuleLocation(1, 10))
				.setName("MyElmement")
				.setDescription("MY description")
				.setCreatedByUserId("test")
				.setModule(moduleId));
		final MvcResult postResult = mockMvc.perform(
					request(POST, "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary", PROJECT_ONE.getNid(), moduleId.getNid())
					.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation)
				)
				.andDo(print())
				.andExpect(status().is(CREATED.value()))
				.andReturn();
		
		final String createdJson = postResult.getResponse().getContentAsString();
		final DataDictionaryPojo createdEntity = PojoMapper.jsonReaderFor(DataDictionaryPojo.class).readValue(createdJson);
		assertEquals(TEST_USER_ID, createdEntity.getCreatedByUserId());
		assertEquals("", createdEntity.getUpdatedByUserName().orElse(""));
		
		/* Update */
		final String jsonForUpdate = PojoMapper.jsonWriter().writeValueAsString(new DataDictionaryPojoPrototype().withId(createdEntity.identity()));
		final MvcResult putResult = mockMvc.perform(
				request(PUT, "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/{dataDictionaryEntryId}",
						PROJECT_ONE.getNid(), moduleId.getNid(), createdEntity.getId())
				.contentType(MediaType.APPLICATION_JSON).content(jsonForUpdate)
			)
			.andDo(print())
			.andExpect(status().is(OK.value()))
			.andReturn();

		final String updatedJson = putResult.getResponse().getContentAsString();
		final DataDictionaryPojo updatedEntity = PojoMapper.jsonReaderFor(DataDictionaryPojo.class).readValue(updatedJson);
		assertEquals(TEST_USER_ID, updatedEntity.getCreatedByUserId());
		assertEquals(TEST_USER_ID, updatedEntity.getUpdatedByUserId().orElseThrow());
	}

	private EntityId createModule(final EntityId projectId) {
		final ModulePojoPrototype proto = new ModulePojoPrototype()
				.setName("Test Module")
				.setProject(projectId)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API);
		return moduleService.create(proto);
	}

}
