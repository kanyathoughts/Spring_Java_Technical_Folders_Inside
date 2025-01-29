/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import innowake.mining.server.controller.AnnotationController;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.server.config.*;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.*;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Optional;
import java.util.HashMap;
import java.util.List;

/**
 * Integration tests for subclasses of {@link AnnotationEvent}. 
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class AnnotationsEventIntegrationTest extends DatabaseResettingTest {

	final static String MODULE_NAME = "Test Module 1";

	final static String EXISTING_ANNOTATION_NAME = "Existing Annotation";

	final static String INSERTED_ANNOTATION_NAME = "Inserted Annotation";
	
	private EntityId moduleId = EntityId.VOID;
	
	private EntityId projectId = EntityId.VOID;
	
	private final String PROJECT_NAME_1 = "Demo Project A";
	
	@Autowired
	private MockMvc mvc;

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private AnnotationService annotationService;
	
	@Autowired
	private StoringEventListener eventListener;

	@BeforeEach
	void beforeEach() {
		projectId = getProjectId(PROJECT_NAME_1);
		moduleId = createModule();
		annotationService.create(createAnnotation(EXISTING_ANNOTATION_NAME, moduleId));
	}

	@Test
	void testCreateAnnotationRaisesEvent() throws Exception {
		final AnnotationPojoPrototype annotation = createAnnotation(INSERTED_ANNOTATION_NAME, moduleId);
		final String annotationJson = PojoMapper.jsonWriter().writeValueAsString(annotation);
		
		mvc.perform(post("/api" + AnnotationController.ANNOTATION_COLLECTION_BY_MODULE_URL, projectId.getNid(), moduleId.getNid())
				.contentType("application/json").content(annotationJson))
				.andExpect(status().isCreated())
				.andReturn();

		final Optional<AnnotationCreatedEvent> optional = eventListener.getEvents()
				.stream()
				.filter(AnnotationCreatedEvent.class::isInstance)
				.map(AnnotationCreatedEvent.class::cast)
				.findFirst();

		assertTrue(optional.isPresent());
	}

	@Test
	void testDeleteAnnotationRaisesEvent() throws Exception {
		final AnnotationPojo annotation = findAnnotation(projectId);

		mvc.perform(delete("/api" + AnnotationController.ANNOTATION_BY_ID_URL, projectId.getNid(), annotation.getId()))
				.andExpect(status().isNoContent());

		final Optional<AnnotationDeletedEvent> optional = eventListener.getEvents()
				.stream()
				.filter(AnnotationDeletedEvent.class::isInstance)
				.map(AnnotationDeletedEvent.class::cast)
				.findFirst();

		final AnnotationDeletedEvent event = optional.orElseGet(Assertions::fail);
		final EntityId id = event.getAnnotationId().orElseGet(Assertions::fail);

		assertEquals(annotation.identity(), id);
	}

	@Test
	void testUpdateAnnotationRaisesEvent() throws Exception {
		final AnnotationPojo annotation = findAnnotation(projectId);
		final String annotationJson = PojoMapper.jsonWriter().writeValueAsString(annotation);
		
		mvc.perform(put("/api" + AnnotationController.ANNOTATION_BY_ID_URL, projectId.getNid(), annotation.getId())
				.contentType("application/json").content(annotationJson))
				.andExpect(status().isOk());

		final Optional<AnnotationUpdatedEvent> optional = eventListener.getEvents()
				.stream()
				.filter(AnnotationUpdatedEvent.class::isInstance)
				.map(AnnotationUpdatedEvent.class::cast)
				.findFirst();

		assertTrue(optional.isPresent());
	}
	
	private EntityId getSystemProjectId() {
		return getProjectId("SYSTEM");
	}
	
	private EntityId getProjectId(final String name) {
		return projectService
				.find(q -> q.withName(name))
				.stream()
				.findFirst()
				.map(x -> x.identity())
				.orElseThrow();
	}

	private EntityId createModule() {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(getProjectId(PROJECT_NAME_1));
		module.setName(MODULE_NAME);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.API);

		return moduleService.create(module);
	}

	private AnnotationPojoPrototype createAnnotation(final String name, final EntityId moduleId) {
		final ModuleLocation moduleLocation = new ModuleLocation();
		moduleLocation.setLength(1);
		moduleLocation.setOffset(3);

		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		annotation.setName(name);
		annotation.setModule(moduleId);
		annotation.setState(WorkingState.CANDIDATE);
		annotation.setType(AnnotationType.DEAD_CODE);
		annotation.setCreatedByUserId("1");
		annotation.setLocation(moduleLocation);
		annotation.setCustomProperties(new HashMap<>());
		annotation.setCategoryId(findAnyCategory().getId());
		annotation.setCreatedByUserId("Test");
		annotation.setUpdatedByUserId("Test");
		annotation.setSourceAttachment("Source Attachement");
		annotation.setEnglishTranslation("English Translation");
		annotation.setReasons(List.of("reason 1"));

		return annotation;
	}

	private AnnotationPojo findAnnotation(final EntityId projectId) {
		final Optional<AnnotationPojo> annotation = this.annotationService.findAny(q -> 
			q.ofProject(projectId)
			.withName(EXISTING_ANNOTATION_NAME));
		return annotation.orElseGet(Assertions::fail);
	}
	
	private AnnotationCategory findAnyCategory() {
		return annotationService.findCategory(q -> q.ofProject(getSystemProjectId())).orElseThrow();
	}	
}