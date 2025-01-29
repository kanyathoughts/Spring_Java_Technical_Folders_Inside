/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.hazelcast.cp.lock.FencedLock;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.server.controller.module.ModuleController;
import innowake.mining.server.locking.ProjectLockService;
import innowake.mining.server.locking.ProjectLockService.ProjectLock;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.ProjectLockCategory;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.Arrays;
import java.util.Optional;
import java.util.UUID;

import static org.junit.Assert.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.http.HttpMethod.DELETE;
import static org.springframework.http.HttpMethod.POST;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.http.HttpStatus.CREATED;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.HttpStatus.OK;
import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.request;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Tests tryLock Annotation for controller methods performing user interaction limited to INSERT, UPDATE and DELETE
 */
@TestInstance(Lifecycle.PER_CLASS)
@AutoConfigureMockMvc
@WithMockUser
class TryLockTest extends AbstractUserNameTest {

	private static final EntityId PROJECT_ID = EntityId.of(1L);
	
	@Autowired
	private ModuleService moduleService;
	
	@MockBean
	@Nullable
	private ProjectLockService projectLockService;

	@Autowired
	private MockMvc mockMvc;
	
	@Autowired
	private TaxonomyService taxonomyService;
	
	@BeforeEach
	@Override
	void init() {
		setAuthentication(Arrays.asList(admin()));
		mockResponseExpectOk(TEST_USER_ID);
		super.init();
	}
	
	@Test
	void testTryLockOnCreateAndUpdateModule() throws Exception {
		final FencedLock fencedLock = Mockito.mock(FencedLock.class);
		final ProjectLock projectLock = projectLockService.new ProjectLock(PROJECT_ID, ProjectLockCategory.MODULES, fencedLock);
		when(Assert.assertNotNull(projectLockService).tryLock(any(), any(), any())).thenReturn(projectLock);

		mockMvc.perform(post("/api" + ModuleController.MODULE_COLLECTION_URL, 1).contentType("application/json")
				.content(PojoMapper.jsonWriter().writeValueAsString(createModule(null, Technology.COBOL))))
				.andExpect(status().isCreated());

		final Optional<ModulePojo> moduleFromDb = moduleService.findModules(q -> q.ofProject(PROJECT_ID).withName("TestModuleApi")).stream().findFirst();
		assertTrue(moduleFromDb.isPresent());
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.MODULES, "Applied Lock on Create Module");
		

		mockMvc.perform(put("/api" + ModuleController.MODULE_BY_ID_URL, 1, moduleFromDb.get().getId())
				.contentType("application/json")
				.content(PojoMapper.jsonWriter().writeValueAsString(createModule(moduleFromDb.get().getId(), Technology.NATURAL))))
				.andExpect(status().isOk());
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.MODULES, "Applied Lock on Update Module");
	}
	
	@Test
	void testTryLockOnDeleteModule() throws Exception {
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(createModule(null, Technology.COBOL));
		final MvcResult postResult = mockMvc.perform(post("/api" + ModuleController.MODULE_COLLECTION_URL, PROJECT_ID.getNid()).contentType("application/json")
				.content(jsonForCreation))
				.andExpect(status().isCreated()).andReturn();
		final String createdJson = postResult.getResponse().getContentAsString();
		final ModulePojo createdEntity = PojoMapper.jsonReaderFor(ModulePojo.class).readValue(createdJson);
		
		mockMvc.perform(
				request(DELETE, "/api/v1/projects/{projectId}/modules/{moduleId}", PROJECT_ID.getNid(), createdEntity.getId())
				.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
				.andDo(print())
				.andExpect(status().is(NO_CONTENT.value()));
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.MODULES, "Applied Lock on Delete Module");
	}
	
	@Test
	void testTryLockOnClearRequiresReview() throws Exception {
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(createModule(null, Technology.COBOL));
		mockMvc.perform(post("/api" + ModuleController.MODULE_COLLECTION_URL, PROJECT_ID.getNid()).contentType("application/json")
				.content(jsonForCreation))
				.andExpect(status().isCreated()).andReturn();
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.MODULES, "Applied Lock on Create Module");
		
		mockMvc.perform(
				request(DELETE, "/api/v1/projects/{projectId}/modules/requiresReview", PROJECT_ID.getNid())
				.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
				.andDo(print())
				.andExpect(status().is(OK.value()));
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.MODULES,
				"Applied Lock on Clear review status for all modules on a project");
	}
	
	@Test
	void testTryLockForAnnotationUpdate() throws Exception {
		final FencedLock fencedLock = Mockito.mock(FencedLock.class);
		final ProjectLock projectLock = projectLockService.new ProjectLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS, fencedLock);
		when(Assert.assertNotNull(projectLockService).tryLock(any(), any(), any())).thenReturn(projectLock);
		
		final EntityId moduleId = getModuleId(PROJECT_ID);
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(createAnnotation());
		final MvcResult postResult = mockMvc.perform(
					request(POST, "/api/v1/projects/{projectId}/modules/{moduleId}/annotations", PROJECT_ID.getNid(), moduleId.getNid())
					.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
					.andDo(print())
					.andExpect(status().is(CREATED.value()))
					.andReturn();
		
		final String createdJson = postResult.getResponse().getContentAsString();
		final AnnotationPojo createdEntity = PojoMapper.jsonReaderFor(AnnotationPojo.class).readValue(createdJson);
		assertUsers(createdEntity, TEST_USER_ID, "");
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS, "Applied Lock on Create Annotation");
		
		/* Update Annotation*/
		final String jsonForUpdate = PojoMapper.jsonWriter().writeValueAsString(new AnnotationPojoPrototype().withId(createdEntity.identity()));
		final MvcResult putResult = mockMvc.perform(
				request(PUT, "/api/v1/projects/{projectId}/annotations/{annotationId}", PROJECT_ID.getNid(), createdEntity.getId())
				.contentType(MediaType.APPLICATION_JSON).content(jsonForUpdate))
				.andDo(print())
				.andExpect(status().is(OK.value()))
				.andReturn();

		final String updatedJson = putResult.getResponse().getContentAsString();
		final AnnotationPojo updatedEntity = PojoMapper.jsonReaderFor(AnnotationPojo.class).readValue(updatedJson);
		assertUsers(updatedEntity, TEST_USER_ID, TEST_USER_ID);
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS, "Applied Lock on Update an annotation");
	}
	
	@Test
	void testTryLockForAnnotationDelete() throws Exception {
		final FencedLock fencedLock = Mockito.mock(FencedLock.class);
		final ProjectLock projectLock = projectLockService.new ProjectLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS, fencedLock);
		when(Assert.assertNotNull(projectLockService).tryLock(any(), any(), any())).thenReturn(projectLock);
		
		final EntityId moduleId = getModuleId(PROJECT_ID);
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(createAnnotation());
		final MvcResult postResult = mockMvc.perform(
					request(POST, "/api/v1/projects/{projectId}/modules/{moduleId}/annotations", PROJECT_ID.getNid(), moduleId.getNid())
					.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
					.andDo(print())
					.andExpect(status().is(CREATED.value()))
					.andReturn();
		
		final String createdJson = postResult.getResponse().getContentAsString();
		final AnnotationPojo createdEntity = PojoMapper.jsonReaderFor(AnnotationPojo.class).readValue(createdJson);
		assertUsers(createdEntity, TEST_USER_ID, "");
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS, "Applied Lock on Create Annotation");
		
		/* Delete Annotation*/
		final Long annoationId = createdEntity.getId();
		mockMvc.perform(
				request(DELETE, "/api/v1/projects/{projectId}/annotations/{annotationId}", PROJECT_ID.getNid(), annoationId)
				.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation)
			)
			.andDo(print())
			.andExpect(status().is(NO_CONTENT.value()));
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS, "Applied Lock on Delete Annotation");
	}
	
	@Test
	void testTryLockForUpdateDataDictionaryEntry() throws Exception {
		final FencedLock fencedLock = Mockito.mock(FencedLock.class);
		final ProjectLock projectLock = projectLockService.new ProjectLock(PROJECT_ID, ProjectLockCategory.DATA_DICTIONARIES, fencedLock);
		when(Assert.assertNotNull(projectLockService).tryLock(any(), any(), any())).thenReturn(projectLock);
		
		/* Create DataDictionaryEntry */
		final EntityId moduleId = getModuleId(PROJECT_ID);
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(createDataDictionaryEntry(moduleId));
		final MvcResult postResult = mockMvc.perform(
					request(POST, "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary", PROJECT_ID.getNid(), moduleId.getNid())
					.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
					.andDo(print())
					.andExpect(status().is(CREATED.value()))
					.andReturn();
		
		final String createdJson = postResult.getResponse().getContentAsString();
		final DataDictionaryPojo createdEntity = PojoMapper.jsonReaderFor(DataDictionaryPojo.class).readValue(createdJson);
		assertEquals(TEST_USER_ID, createdEntity.getCreatedByUserId());
		createdEntity.getUpdatedByUserId().ifPresent(u -> assertEquals("", u));
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.DATA_DICTIONARIES,
				"Applied Lock on Create new data dictionary entry");
		
		/* Update DataDictionaryEntry */
		final Long entityId = createdEntity.getId();
		final String jsonForUpdate = PojoMapper.jsonWriter().writeValueAsString(createdEntity);
		final MvcResult putResult = mockMvc.perform(
				request(PUT, "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/{dataDictionaryEntryId}",
						PROJECT_ID.getNid(), moduleId.getNid(), entityId)
				.contentType(MediaType.APPLICATION_JSON).content(jsonForUpdate)
			)
			.andDo(print())
			.andExpect(status().is(OK.value()))
			.andReturn();

		final String updatedJson = putResult.getResponse().getContentAsString();
		final DataDictionaryPojo updatedEntity = PojoMapper.jsonReaderFor(DataDictionaryPojo.class).readValue(updatedJson);
		assertEquals(TEST_USER_ID, updatedEntity.getCreatedByUserId());
		assertEquals(TEST_USER_ID, updatedEntity.getUpdatedByUserId().orElseThrow());
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.DATA_DICTIONARIES,
				"Applied Lock on Update data dictionary entry");
	}
	
	@Test
	void testTryLockForDeleteDataDictionaryEntry() throws Exception {
		final FencedLock fencedLock = Mockito.mock(FencedLock.class);
		final ProjectLock projectLock = projectLockService.new ProjectLock(PROJECT_ID, ProjectLockCategory.DATA_DICTIONARIES, fencedLock);
		when(Assert.assertNotNull(projectLockService).tryLock(any(), any(), any())).thenReturn(projectLock);
		
		/* Create DataDictionaryEntry*/
		final EntityId moduleId = getModuleId(PROJECT_ID);
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(createDataDictionaryEntry(moduleId));
		final MvcResult postResult = mockMvc.perform(
					request(POST, "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary", PROJECT_ID.getNid(), moduleId.getNid())
					.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
					.andDo(print())
					.andExpect(status().is(CREATED.value()))
					.andReturn();
		
		final String createdJson = postResult.getResponse().getContentAsString();
		final DataDictionaryPojo createdEntity = PojoMapper.jsonReaderFor(DataDictionaryPojo.class).readValue(createdJson);
		assertEquals(TEST_USER_ID, createdEntity.getCreatedByUserId());
		assertFalse(createdEntity.getUpdatedByUserId().isPresent());
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.DATA_DICTIONARIES,
				"Applied Lock on Create new data dictionary entry");
		
		/* Delete DataDictionaryEntry*/
		final Long entityId = createdEntity.getId();
		mockMvc.perform(
				request(DELETE, "/api/v1/projects/{projectId}/modules/{moduleId}/data-dictionary/{dataDictionaryEntryId}",
						PROJECT_ID.getNid(), moduleId.getNid(), entityId)
				.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
				.andDo(print())
				.andExpect(status().is(NO_CONTENT.value()));
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.DATA_DICTIONARIES,
				"Applied Lock on Delete data dictionary entry");
	}
	
	@Test
	void testTryLockForCreateAndUpdateTaxonomy() throws Exception {
		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(PROJECT_ID));
		var type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("UpdateTaxonomyTryLock").setProject(PROJECT_ID).setCategoryId(taxonomyCategory));
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype().setName("UpdateTaxonomy TryLock").setProject(PROJECT_ID).setType(type);
		
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(taxonomy);

		final MvcResult postResult = mockMvc.perform(
				request(POST, "/api/v1/projects/{projectId}/taxonomies", PROJECT_ID.getNid())
				.contentType(APPLICATION_JSON)
				.content(jsonForCreation))
				.andDo(print())
				.andExpect(status().is(CREATED.value()))
				.andReturn();
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.TAXONOMIES, "Applied Lock on Create new taxonomy");
		
		final String createdJson = postResult.getResponse().getContentAsString();
		final TaxonomyPojo createdTaxonomy = new ObjectMapper().readValue(createdJson, TaxonomyPojo.class);
		
		TaxonomyPojoPrototype proto = new TaxonomyPojoPrototype().withId(createdTaxonomy.identity()).setName("updated");
		
		/* Update Taxonomy*/
		final UUID taxonomyId = createdTaxonomy.getUid();
		final String jsonForUpdate = PojoMapper.jsonWriter().writeValueAsString(proto);
		mockMvc.perform(
				request(PUT, "/api/v1/projects/{projectId}/taxonomies/{taxonomyId}", PROJECT_ID.getNid(), taxonomyId)
				.contentType(MediaType.APPLICATION_JSON)
				.content(jsonForUpdate))
				.andDo(print())
				.andExpect(status().is(OK.value()))
				.andReturn();
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.TAXONOMIES, "Applied Lock on Update taxonomy");
	}
	
	@Test
	void testTryLockForDeleteTaxonomy() throws Exception {
		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype().setName("Business Taxonomies").setProject(PROJECT_ID));
		var type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DeleteTaxonomyTryLock").setProject(PROJECT_ID).setCategoryId(taxonomyCategory));
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype().setName("DeleteTaxonomy TryLock").setProject(PROJECT_ID).setType(type);
		
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(taxonomy);

		final MvcResult postResult = mockMvc.perform(
				request(POST, "/api/v1/projects/{projectId}/taxonomies", PROJECT_ID.getNid())
				.contentType(APPLICATION_JSON)
				.content(jsonForCreation))
				.andDo(print())
				.andExpect(status().is(CREATED.value()))
				.andReturn();
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.TAXONOMIES, "Applied Lock on Create new taxonomy");
		
		final String createdJson = postResult.getResponse().getContentAsString();
		final TaxonomyPojo createdTaxonomy = new ObjectMapper().readValue(createdJson, TaxonomyPojo.class);
		
		/* Delete Taxonomy*/
		final Long taxonomyId = createdTaxonomy.getId();
		final String jsonForDelete = PojoMapper.jsonWriter().writeValueAsString(createdTaxonomy);
		mockMvc.perform(
				request(DELETE, "/api/v1/projects/{projectId}/taxonomies/{taxonomyId}", PROJECT_ID.getNid(), taxonomyId)
				.contentType(MediaType.APPLICATION_JSON).content(jsonForDelete))
				.andDo(print())
				.andExpect(status().is(NO_CONTENT.value()));
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.TAXONOMIES, "Applied Lock on Delete Taxonomy");
	}
	
	@Test
	void testTryLockForAnnotationCategory() throws Exception {
		final FencedLock fencedLock = Mockito.mock(FencedLock.class);
		final ProjectLock projectLock = projectLockService.new ProjectLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS, fencedLock);
		when(Assert.assertNotNull(projectLockService).tryLock(any(), any(), any())).thenReturn(projectLock);
		
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(createAnnotation());
		final MvcResult postResult = mockMvc.perform(
					request(POST, "/api/v1/projects/{projectId}/annotation-categories", PROJECT_ID.getNid())
					.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
					.andDo(print())
					.andExpect(status().is(CREATED.value()))
					.andReturn();
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS,
				"Applied Lock on Create a new Annotation Category");
		
		/* Update Annotation Catagory*/
		final String createdJson = postResult.getResponse().getContentAsString();
		final AnnotationCategory createdEntity = PojoMapper.jsonReaderFor(AnnotationCategory.class).readValue(createdJson);
		final Long annoationCategoryId = createdEntity.getId();
		final String jsonForUpdate = PojoMapper.jsonWriter().writeValueAsString(createdEntity);
		mockMvc.perform(
				request(PUT, "/api/v1/projects/{projectId}/annotation-categories/{annotationCategoryId}", PROJECT_ID.getNid(), annoationCategoryId)
				.contentType(MediaType.APPLICATION_JSON).content(jsonForUpdate))
				.andDo(print())
				.andExpect(status().is(OK.value()))
				.andReturn();

		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS,
				"Applied Lock on Update an Annotation Category");
		
		/* Delete Annotation Category*/
		mockMvc.perform(
				request(DELETE, "/api/v1/projects/{projectId}/annotation-categories/{annotationCategoryId}", PROJECT_ID.getNid(), annoationCategoryId)
				.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation)
			)
			.andDo(print())
			.andExpect(status().is(OK.value()));
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS,
				"Applied Lock on Delete an Annotation Category");
	}
	
	@Test
	void testTryLockForReference() throws Exception {
		final FencedLock fencedLock = Mockito.mock(FencedLock.class);
		final ProjectLock projectLock = projectLockService.new ProjectLock(PROJECT_ID, ProjectLockCategory.ANNOTATIONS, fencedLock);
		when(Assert.assertNotNull(projectLockService).tryLock(any(), any(), any())).thenReturn(projectLock);
		
		final EntityId rootJob = createTestModule("rootJob_" + PROJECT_ID);
		final String jsonForCreation = PojoMapper.jsonWriter().writeValueAsString(createReference(rootJob));
		final MvcResult postResult = mockMvc.perform(
					request(POST, "/api/v1/projects/{projectId}/modules/{moduleId}/references", PROJECT_ID.getNid(), rootJob.getNid())
					.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
					.andDo(print())
					.andExpect(status().is(CREATED.value()))
					.andReturn();
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.REFERENCE,
				"Applied Lock on Create a new reference");
		
		final String createdJson = postResult.getResponse().getContentAsString();
		final ModuleRelationshipPojo createdReference = PojoMapper.jsonReaderFor(ModuleRelationshipPojo.class).readValue(createdJson);
		
		/* Delete Taxonomy*/
		final UUID referenceId = createdReference.getId();
		mockMvc.perform(
				request(DELETE, "/api/v1/projects/{projectId}/modules/{moduleId}/references/{referenceId}", PROJECT_ID.getNid(), rootJob.getNid(), referenceId)
				.contentType(MediaType.APPLICATION_JSON).content(jsonForCreation))
				.andDo(print())
				.andExpect(status().is(NO_CONTENT.value()));
		
		/* Verify that the projectLockService.tryLock() method was called with the expected arguments */
		Mockito.verify(projectLockService, Mockito.times(1)).tryLock(PROJECT_ID, ProjectLockCategory.REFERENCE, "Applied Lock on Delete a reference");
	}

	private AnnotationPojoPrototype createAnnotation() {
		return new AnnotationPojoPrototype()
				.setName("MINIMAL ANNOTATION")
				.setState(WorkingState.IN_ANALYSIS)
				.setType(AnnotationType.RULE)
				.setLocation(new ModuleLocation(10, 20));
	}
	
	private ModuleRelationshipPojoPrototype createReference(final EntityId moduleId) {
		final EntityId stepA = createTestModule("stepA_" + PROJECT_ID);
		
		final ModuleLocation moduleLocation = new ModuleLocation(1, 2);

		return new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(moduleId)
				.setDstModule(stepA)
				.setSrcLocation(moduleLocation)
				.setDstLocation(moduleLocation);
	}
	
	private DataDictionaryPojoPrototype createDataDictionaryEntry(final EntityId module) {
		return new DataDictionaryPojoPrototype()
				.setLocation(new ModuleLocation(1, 10))
				.setName("MyElmement")
				.setDescription("MY description")
				.setCreatedByUserId("test")
				.setModule(module);
	}
	
	private EntityId getModuleId(final EntityId projectId) {
		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setName("Test Module")
				.setProject(projectId)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.API);

		return moduleService.create(module);
	}
	
	private ModulePojoPrototype createModule(@Nullable final Long id, final Technology technology ) {
		final ModulePojoPrototype module = new ModulePojoPrototype().setName("TestModuleApi")
				.setProject(EntityId.of(1L))
				.setTechnology(technology)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM);
		/* must set id here or else the JSON serialization fails */
		if (id != null) {
			module.setNid(id);
		}
		return module;
	}
	
	private EntityId createTestModule(final String name) {
		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setName(name)
				.setProject(PROJECT_ID)
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setDescription("Creating Module " + name)
				.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	private void assertUsers(AnnotationPojo annotation, String expectCreated, String expectUpated) {
		assertEquals(expectCreated, annotation.getCreatedByUserId());
		assertEquals(expectUpated, annotation.getUpdatedByUserId().orElse(""));
	}
}
