/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.event.AnnotationCreatedEvent;
import innowake.mining.server.event.AnnotationDeletedEvent;
import innowake.mining.server.event.AnnotationEvent;
import innowake.mining.server.event.AnnotationUpdatedEvent;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.EntityId;

import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.MiningPojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.WorkingState;

/** Tests the setting of DeadCode. */
class UnreachableDependencyServiceTest extends DatabaseResettingTest {

	@Autowired
	private UnreachableDependencyService unreachableDependencyService;
	
	@Autowired
	private AnnotationService annotationService;

	@Autowired
	private ModuleService moduleService;

	private EntityId projectId = EntityId.VOID;
	
	private EntityId moduleId = EntityId.VOID;
	
	private final String DEAD_CODE_ANNO_1 = "DeadCode Annotation 1";
	
	private final String DEAD_CODE_ANNO_2 = "DeadCode Annotation 2";
	
	private final String PROJECT_NAME_1 = "Demo Project A";

	@BeforeEach()
	public void beforeEach() {
		projectId = getProjectId();
		moduleId = createTestData();
	}
	
	/**
	 * Tests that when the projectId and moduleId are known and a {@linkplain AnnotationType.DEAD_CODE} annotation 
	 * encompasses 2 relationships that both of the relationships are marked as from_dead_code == true.  
	 * The 3rd and 4th relationships should be marked from_dead_code == false as the relationships are outside of 
	 * the annotation.
	 */
	@Test
	void testSetDeadCodeForModule() {
		assertPreConditions(5, 2);

		unreachableDependencyService.setDeadCodeForModule(projectId, moduleId);

		assertPostConditions(5, 3);
	}
	
	/**
	 * Tests that when the projectId and moduleId are known and a non-{@linkplain AnnotationType.DEAD_CODE} annotation 
	 * encompasses relationships in the module that those relationships remain as from_dead_code == false.
	 */
	@Test
	void testSetDeadCodeForModuleWithNonDeadCodeAnnotation() {
		createRelationshipWithNonDeadCodeAnnotation();
		assertPreConditions(6, 4);
		
		unreachableDependencyService.setDeadCodeForModule(projectId, moduleId);
		
		assertPostConditions(6, 3);
	}

	/**
	 * Tests that when only the projectId is known and a {@linkplain AnnotationType.DEAD_CODE} annotation 
	 * encompasses 2 relationships that both of the relationships are marked as from_dead_code == true.  
	 */
	@Test
	void testSetDeadCodeForProject() {
		assertPreConditions(5, 2);

		unreachableDependencyService.setDeadCodeForProject(projectId);

		assertPostConditions(5, 3);
	}
	
	/**
	 * Tests that when only the projectId is known and a non-{@linkplain AnnotationType.DEAD_CODE} annotation 
	 * encompasses relationships in the module that those relationships remain as from_dead_code == false.
	 */
	@Test
	void testSetDeadCodeForProjectWithNonDeadCodeAnnotation() {
		createRelationshipWithNonDeadCodeAnnotation();
		assertPreConditions(6, 4);
		
		unreachableDependencyService.setDeadCodeForProject(projectId);
		
		assertPostConditions(6, 3);
	}

	/**
	 * Tests that when the annotationId is known and is a {@linkplain AnnotationType.DEAD_CODE} annotation
	 * encompassing a relationship in the module that the relationship is marked as from_dead_code == true.
	 *
	 */
	@Test
	void testSetDeadCodeForAnnotation() {
		assertPreConditions(5, 2);
		
		final AnnotationPojo annotation = getDeadCodeAnnotation(DEAD_CODE_ANNO_1);
		unreachableDependencyService.setDeadCodeForAnnotation(projectId, annotation.identity());

		assertPostConditions(5, 2);
	}
	
	/**
	 * Tests that when the annotationId is known and is a non-{@linkplain AnnotationType.DEAD_CODE} annotation
	 * encompassing a relationship in the module that the relationship is remains as from_dead_code == false.
	 *
	 */
	@Test
	void testSetDeadCodeForAnnotationWithNonDeadCodeAnnotation() {
		createRelationshipWithNonDeadCodeAnnotation();
		assertPreConditions(6, 4);
		
		final AnnotationPojo annotation = getRuleAnnotation();
		unreachableDependencyService.setDeadCodeForAnnotation(projectId, annotation.identity());

		assertPostConditions(6, 0);
	}
	
	/** 
	 * Tests that when an {@linkplain AnnotationCreatedEvent} is handled and has the projectId and 
	 * annotationId are both known, the relationship is marked as from_dead_code == true for the
	 * specified annotation.
	 */
	@Test
	void testOnAnnotationCreated() {
		assertPreConditions(5, 2);
		
		final AnnotationPojo annotation = getDeadCodeAnnotation(DEAD_CODE_ANNO_1);
		final AnnotationCreatedEvent event = new AnnotationCreatedEvent(projectId, annotation.identity());
		
		unreachableDependencyService.onAnnotationCreated(event);
		
		assertPostConditions(5, 2);
	}
	
	/**
	 * Tests scenario when an annotation is updated to a non-Dead Code type and the annotation id is known.
	 * The relationships should be marked as from_dead_code == false for the updated annotation.
	 */
	@Test
	void testOnAnnotationUpdated() {
		assertPreConditions(5, 2);
		
		final AnnotationPojo annotation = getDeadCodeAnnotation(DEAD_CODE_ANNO_1);
		final AnnotationPojoPrototype annotationPrototype = new AnnotationPojoPrototype()
				.setUid(annotation.getUid())
				.setType(AnnotationType.RULE)
				.setName("Not Dead Code");
		annotationService.update(annotationPrototype);
		final AnnotationUpdatedEvent event = new AnnotationUpdatedEvent(projectId, annotation.identity());
		unreachableDependencyService.onAnnotationUpdated(event);
		
		assertPostConditions(5, 1);
	}
	
	/**
	 * Tests scenario when an annotation is updated to a new location and the annotation id is known.
	 * The relationships should be marked as from_dead_code == false for the updated annotation that
	 * no longer encompass the relationships.
	 */
	@Test
	void testOnAnnotationUpdatedChangedLocation() {
		assertPreConditions(5, 2);
		
		final AnnotationPojo annotation = getDeadCodeAnnotation(DEAD_CODE_ANNO_1);
		final AnnotationPojoPrototype annotationPrototype = new AnnotationPojoPrototype()
				.setUid(annotation.getUid())
				.setLocation(new ModuleLocation(150, 10));
		annotationService.update(annotationPrototype);
		final AnnotationUpdatedEvent event = new AnnotationUpdatedEvent(projectId, annotation.identity());
		unreachableDependencyService.onAnnotationUpdated(event);
		
		assertPostConditions(5, 1);
	}
	
	/**
	 * Tests scenario when an annotation is updated but the annotation id is unknown.  Existing
	 * dead code annotations for the project should be re-evaluated, and any dead code annotations
	 * need to mark the relationship as from_dead_code == true.
	 */
	@Test
	void testOnAnnotationUpdatedAnnotationIdUnknown() {
		assertPreConditions(5, 2);
				
		final AnnotationUpdatedEvent event = new AnnotationUpdatedEvent(projectId);
		unreachableDependencyService.onAnnotationUpdated(event);
		
		assertPostConditions(5, 3);
	}

	@Test
	void testOnAnnotationDeleted() {
		assertPreConditions(5, 2);
		
		final AnnotationPojo annotation = getDeadCodeAnnotation(DEAD_CODE_ANNO_1);
		annotationService.delete(q -> q.byId(annotation.identity()));
		final AnnotationDeletedEvent event = new AnnotationDeletedEvent(projectId, annotation.getModule(), annotation.identity());
		
		unreachableDependencyService.onAnnotationDeleted(event);
		
		assertPostConditions(5, 1);
	}
	
	@Test
	void testOnAnnotationDeletedAnnotationIdUnknown() {
		assertPreConditions(5, 2);
		
		final AnnotationPojo annotation = getDeadCodeAnnotation(DEAD_CODE_ANNO_2);
		annotationService.delete(q -> q.byId(annotation.identity()));
		
		final AnnotationDeletedEvent event = new AnnotationDeletedEvent(projectId);
		
		unreachableDependencyService.onAnnotationDeleted(event);
		
		/* In this case the Annotation that was deleted encompassed only 
		 * 1 relationship, leaving 2 dead relationships. */ 
		assertPostConditions(5, 2);
	}
	
	@Test
	void testOnAnnotationEvent() {
		assertPreConditions(5, 2);
		
		final AnnotationPojo annotation = getDeadCodeAnnotation(DEAD_CODE_ANNO_1);
		final AnnotationEvent event = new AnnotationEvent(projectId, annotation.identity());
		
		unreachableDependencyService.onAnnotationEvent(event);
		
		assertPostConditions(5, 3);
	}
	
	@Test
	void testOnAnnotationEventAnnotationIdUnknown() {
		assertPreConditions(5, 2);
		
		final AnnotationEvent event = new AnnotationEvent(projectId);
		unreachableDependencyService.onAnnotationEvent(event);
		
		assertPostConditions(5, 3);
	}
	
	private EntityId getProjectId() {
		projectId = projectService
				.find(q -> q.withName(PROJECT_NAME_1))
				.stream()
				.findFirst()
				.map(MiningPojo::identity)
				.orElse(EntityId.VOID);
		
		return projectId;
	}

	private long countFromDeadCode(final List<ModuleRelationshipPojo> relationships) {
		return relationships.stream()
				.map(moduleRelationshipPojo -> {
					final Map<String, Object> properties = moduleRelationshipPojo.getProperties().orElse(Collections.emptyMap());
					final var fromDeadCode = properties.get("dead_code");
					return fromDeadCode instanceof Boolean && ((Boolean) fromDeadCode).booleanValue();
				})
				.filter(x -> x)
				.count();
	}
	
	private EntityId createTestData() {
		final EntityId sourceModule = createModule("Module_1_", projectId);
		final EntityId targetModule = createModule("Module_2_", projectId);
		final EntityId includedModule = createModule("CopyBook_1", projectId);

		createAnnotation(DEAD_CODE_ANNO_1, AnnotationType.DEAD_CODE, WorkingState.APPROVED,
				"This is Function source attachment \\n content", sourceModule, new ModuleLocation(4, 100));
		
		createAnnotation(DEAD_CODE_ANNO_2, AnnotationType.DEAD_CODE, WorkingState.APPROVED,
				"This is Function source attachment \\n content", sourceModule, new ModuleLocation(405, 10));


		createRelationship(new ModuleLocation(300, 400), RelationshipType.CALLS, sourceModule, targetModule);
		createRelationship(new ModuleLocation(10, 10), RelationshipType.ACCESSES, sourceModule, targetModule);
		createRelationship(new ModuleLocation(20, 15), RelationshipType.REFERENCES, sourceModule, targetModule);
		createRelationship(new ModuleLocation(405, 10), RelationshipType.CALLS, sourceModule, targetModule);
		
		/* Creates a RelationshipDirection.OUT relationship */
		createRelationship(new ModuleLocation(3, 5), RelationshipType.INCLUDES, includedModule, sourceModule);
		
		return sourceModule;
	}

	private void createRelationship(final ModuleLocation moduleLocation, final RelationshipType relationshipType, final EntityId moduleId, final EntityId toModuleId) {
		final ModuleRelationshipPojoPrototype relationship = new ModuleRelationshipPojoPrototype();
		relationship.setSrcModule(moduleId);
		relationship.setDstModule(toModuleId);
		relationship.setRelationship(relationshipType);
		relationship.setSrcLocation(moduleLocation);
		relationship.setProperties(Map.of("dead_code", false));
		moduleService.createRelationship(relationship);
	}
	
	private EntityId createAnnotation(final String name, final AnnotationType annotationType, final WorkingState workingState,
			final String sourceAttachment, final EntityId moduleId, final ModuleLocation location) {
		final AnnotationPojoPrototype annotation = new AnnotationPojoPrototype();
		final ModuleLocation moduleLocation = location;
		annotation.setName(name);
		annotation.setModule(moduleId);
		annotation.setType(annotationType);
		annotation.setState(workingState);
		annotation.setLocation(moduleLocation);
		annotation.setSourceAttachment(sourceAttachment);
		annotation.setCreatedByUserId("Test");
		return annotationService.create(annotation);
	}
	
	private void createRelationshipWithNonDeadCodeAnnotation() {
		createRelationship(new ModuleLocation(200, 10), RelationshipType.CALLS, moduleId, moduleId);
		createAnnotation("Rule Annotation", AnnotationType.RULE, WorkingState.CANDIDATE, "This is Function source attachment \\n content", moduleId, 
				new ModuleLocation(200, 10));
		createAnnotation("Functional Annotation", AnnotationType.FUNCTIONAL, WorkingState.CANDIDATE, "This is Function source attachment \\n content", moduleId, 
				new ModuleLocation(200, 10));
	}

	private EntityId createModule(final String name, final EntityId projectId) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setProject(projectId);
		module.setTechnology(Technology.COBOL);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setOrigin(Origin.CUSTOM);
		module.setName(name + projectId.getNid());
		module.setIdentification(Identification.IDENTIFIED);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	private AnnotationPojo getDeadCodeAnnotation(final String name) {
		final List<AnnotationPojo> annotationList = annotationService.find(q -> 
														q.ofProject(projectId)
														.withName(name)
														.withType(AnnotationType.DEAD_CODE));
		assertNotNull(annotationList);
		assertEquals(1, annotationList.size());
		
		return annotationList.get(0);
	}
	
	private AnnotationPojo getRuleAnnotation() {
		final List<AnnotationPojo> annotationList = annotationService.find(q -> 
					q.ofProject(projectId)
					.withType(AnnotationType.RULE));

		assertNotNull(annotationList);
		assertEquals(1, annotationList.size());
		
		return annotationList.get(0);
	}

	private void assertPreConditions(final int numRelationships, final int numAnnotations) {
		final List<ModuleRelationshipPojo> preRelationships = moduleService.findRelationship(q -> q
				.ofProject(projectId));
		
		assertNotNull(preRelationships);
		assertEquals(numRelationships, preRelationships.size(), String.format("There should be %d total relationships present", numRelationships));
		assertEquals(0, countFromDeadCode(preRelationships));

		final List<AnnotationPojo> annotationList = annotationService.find(q -> q.ofProject(projectId));
		assertNotNull(annotationList);
		assertEquals(numAnnotations, annotationList.size(), String.format("There should be %d total relationships present", numAnnotations));
	}
	
	private void assertPostConditions(final int numRelationships, final int numDeadRelationships) {
		final List<ModuleRelationshipPojo> postRelationships = moduleService.findRelationship(q -> q.ofProject(projectId));
		
		assertEquals(numRelationships, postRelationships.size(), String.format("There should be %d total relationships present", numRelationships));
		assertEquals(numDeadRelationships, countFromDeadCode(postRelationships), String.format("%d of the Reference should have fromDeadCode == true based on offsets and length", numDeadRelationships));
	}
}
