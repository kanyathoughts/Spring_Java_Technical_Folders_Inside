/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.data;

import static org.apache.commons.lang.math.NumberUtils.LONG_ONE;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
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

/**
 * Test AnnotationCategory create and delete using AnnotationCategoryDao
 */
class AnnotationCategoryDaoTest extends DatabaseRelatedTest {

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private AnnotationService annotationService;

	@Test
	void testAnnotationCategories() {
		final EntityId projectId = projectService.create(new ProjectPojoPrototype()
				.setName("Test_Project")
				.setClient(EntityId.of(LONG_ONE))
				.setNatures(Collections.emptySet())
			).identity();
		
		final EntityId rootJob = createTestModule(projectId, "rootJob_" + projectId);
		final EntityId stepA = createTestModule(projectId, "stepA_" + projectId);
		
		final ModuleLocation moduleLocation = new ModuleLocation(1, 2);
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(rootJob)
				.setSrcLocation(moduleLocation)
				.setDstModule(stepA)
				.setDstLocation(moduleLocation));

		AnnotationCategory annotationCategory = annotationService.getCategory(projectId,
				annotationService.createCategory(projectId, "Test Category", Collections.emptyList()));

		/* Test Annotation Category is created or not */
		assertNotNull(annotationCategory);
		assertEquals("Test Category", annotationCategory.getName());

		final AnnotationPojo annotation = annotationService.get(annotationService.create(new AnnotationPojoPrototype()
				.setModule(rootJob)
				.setName("Test")
				.setCategoryId(annotationCategory.getId())
				.setState(WorkingState.CANDIDATE)
				.setType(AnnotationType.DEAD_CODE)
				.setSourceAttachment(new BinaryString("ABC"))
				.setLocation(new ModuleLocation(0, 2))
				.setCreatedByUserId("")
				.setUpdatedByUserId("")
			));

		assertNotNull(annotation);

		assertEquals(annotationCategory.getId(), annotation.getCategoryId().get());

		annotationService.deleteCategory(projectId, annotationCategory.getId());

		final AnnotationPojo annotation2 = annotationService.get(q -> q.ofProject(projectId).byId(annotation.identity()));

		assertNotNull(annotation2);

		/*Test AnnotationCategoryLink of Annotation is null*/
		assertTrue(annotation2.getCategoryId().isEmpty());
	}

	private EntityId createTestModule(final EntityId projectId, final String name) {
		return moduleService.create(new ModulePojoPrototype() 
			.setName(name)
			.setProject(projectId)
			.setTechnology(Technology.COBOL)
			.setType(Type.PROGRAM)
			.setStorage(Storage.FILE)
			.setIdentification(Identification.IDENTIFIED)
			.setOrigin(Origin.CUSTOM)
			.setDescription("Creating Module " + name)
			.setCreator(Creator.DISCOVERY));
	}
}
