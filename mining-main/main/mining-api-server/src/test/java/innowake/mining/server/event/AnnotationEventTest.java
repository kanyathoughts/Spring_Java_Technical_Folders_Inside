/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
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
 * Tests various subclasses of {@link AnnotationEvent}.
 */
class AnnotationEventTest extends DatabaseResettingTest {
	
protected static Long PROJECT_ID = 1L;
	
	@Autowired
	private ModuleService moduleService;

	@Autowired
	private AnnotationService annotationService;
	
	@Test
	void testInstantiationAnnotationCreatedEvent() {
		final EntityId projectId = EntityId.of(PROJECT_ID);
		final AnnotationCreatedEvent event = new AnnotationCreatedEvent(projectId, EntityId.of(Long.valueOf(3)));

		assertTrue(event.getProjectId().isPresent());
		assertEquals(projectId, event.getProjectId().orElseThrow());

		assertTrue(event.getAnnotationId().isPresent());
	}

	@Test
	void testInstantiationAnnotationCreatedEventWithAnnotationId() {
		final EntityId projectId = EntityId.of(PROJECT_ID);
		final EntityId annotationId = getAnnotation(createAnnotation(createModule(projectId))).identity();

		final AnnotationCreatedEvent event = new AnnotationCreatedEvent(projectId, annotationId);

		assertTrue(event.getProjectId().isPresent());
		assertEquals(projectId, event.getProjectId().get());

		assertTrue(event.getAnnotationId().isPresent());
		assertEquals(annotationId, event.getAnnotationId().get());
	}
	
	@Test
	void testAnnotationUpdatedInstantiation() {
		final EntityId projectId = EntityId.of(PROJECT_ID);
		final AnnotationUpdatedEvent event = new AnnotationUpdatedEvent(projectId);

		assertTrue(event.getProjectId().isPresent());
		assertEquals(projectId, event.getProjectId().get());

		assertFalse(event.getAnnotationId().isPresent());
	}

	@Test
	void testAnnotationUpdatedEventInstantiationWithAnnotationId() {
		final EntityId projectId = EntityId.of(PROJECT_ID);
		final EntityId annotationId = getAnnotation(createAnnotation(createModule(projectId))).identity();

		final AnnotationUpdatedEvent event = new AnnotationUpdatedEvent(projectId, annotationId);

		assertTrue(event.getProjectId().isPresent());
		assertEquals(projectId, event.getProjectId().get());

		assertTrue(event.getAnnotationId().isPresent());
		assertEquals(annotationId, event.getAnnotationId().get());
	}
	
	@Test
	void testAnnotationDeletedEventInstantiation() {
		final EntityId projectId = EntityId.of(PROJECT_ID);
		final AnnotationPojo annotation = getAnnotation(createAnnotation(createModule(projectId)));
		
		final AnnotationDeletedEvent event = new AnnotationDeletedEvent(projectId, annotation.getModule(), annotation.identity());

		assertTrue(event.getProjectId().isPresent());
		assertEquals(projectId, event.getProjectId().get());

		assertTrue(event.getAnnotationId().isPresent());
		assertEquals(annotation.identity(), event.getAnnotationId().get());
		assertEquals(annotation.getModule(), event.getModuleId().get());
	}
	
	protected EntityId createModule(final EntityId projectId) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setOrigin(Origin.CUSTOM);
		module.setName("Test Module");
		module.setIdentification(Identification.IDENTIFIED);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	protected EntityId createAnnotation(final EntityId moduleId) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		final ModuleLocation moduleLocation = new ModuleLocation(4, 100);
		annotation.setName("Test");
		annotation.setModule(moduleId);
		annotation.setType(AnnotationType.RULE);
		annotation.setState(WorkingState.APPROVED);
		annotation.setLocation(moduleLocation);
		annotation.setSourceAttachment("Some Source");
		annotation.setCreatedByUserId("Test");
		return annotationService.create(annotation);
	}
	
	protected AnnotationPojo getAnnotation(final EntityId id) {
		return annotationService.findAny(b -> b.byId(id)).orElseThrow();
	}
}
