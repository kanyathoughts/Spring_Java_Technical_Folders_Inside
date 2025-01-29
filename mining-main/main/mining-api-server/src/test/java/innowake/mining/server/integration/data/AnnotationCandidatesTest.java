package innowake.mining.server.integration.data;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.orientechnologies.orient.core.db.document.ODatabaseDocument;

import innowake.mining.data.core.annotation.AnnotationCandidates;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.AnnotationCategory;
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
 * Test storing of Annotations using the AnnotationCandidates class.
 */
class AnnotationCandidatesTest extends DatabaseRelatedTest {
	
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private MiningDataCoreService core;
	
	private static final EntityId DEFAULT_PROJECT = EntityId.of(0L);
	private static final EntityId PROJECT_ID = EntityId.of(Long.valueOf(1l));
	private static final String ANNOTATION_NAME = "Business Rule [System Identified]";
		
	private EntityId createTestModule() {
		/* Create test module */
		final ModulePojoPrototype mod = new ModulePojoPrototype();
		mod.setProject(PROJECT_ID);
		mod.setName("TEST_MODULE");
		mod.setTechnology(Technology.COBOL);
		mod.setType(Type.PROGRAM);
		mod.setOrigin(Origin.CUSTOM);
		mod.setStorage(Storage.FILE);
		mod.setIdentification(Identification.MISSING);
		mod.setPath("src/cobol/TEST_MODULE.cbl");
		mod.setContent("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		mod.setCreator(Creator.DISCOVERY);
		return moduleService.create(mod);
	}
	
	/**
	 * Creates a test Annotation and stores it using
	 * {@link innowake.mining.data.core.annotation.AnnotationCandidates#store(Long, List, ODatabaseDocument, java.util.Map) AnnotationCandidates.store}.
	 * Checks whether the Annotation exists with the correct name and the correct
	 * Category assigned to it.
	 */
	@Test
	void testStore() {
		final EntityId moduleId = createTestModule();

		/* Create and obtain AnnotationCategory */
		final AnnotationCandidates candidates = new AnnotationCandidates();
		final Long businessRuleCategoryId = candidates.getDefaultAnnotationCategory(core, "Business Rule");

		/* Create and store Annotation */
		final var annotation = createTestAnnotation(businessRuleCategoryId, moduleId);
		final var identifiedAnnotations = new ArrayList<AnnotationPojoTemplate>();
		identifiedAnnotations.add(annotation);
		candidates.store(moduleId, identifiedAnnotations, core);

		final AnnotationCategory existingAnnotationCategory = annotationService.findCategory(q -> q.ofProjectWithDefault(DEFAULT_PROJECT)
																				.byId(businessRuleCategoryId))
																				.orElseThrow(() -> new MiningEntityNotFoundException("Annotation category with id: " + businessRuleCategoryId + " must exists for project with nid: 0"));
		assertNotNull(existingAnnotationCategory);
		/* Test if created AnnotationCategory is correct */
		assertEquals("Business Rule", existingAnnotationCategory.getName());
		assertEquals(businessRuleCategoryId, existingAnnotationCategory.getId());

		/* Test if created Annotation is correct */
		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofModule(moduleId));
		assertEquals(1, annotations.size());
		final AnnotationPojo existingAnnotation = annotations.get(0);
		assertEquals(existingAnnotation.getCategoryId().get(), existingAnnotationCategory.getId());
		assertEquals(existingAnnotation.getCategoryName().get(), existingAnnotationCategory.getName());
		assertEquals(ANNOTATION_NAME, existingAnnotation.getName());

		moduleService.deleteModule(moduleId, true);
	}

	private AnnotationPojoTemplate createTestAnnotation(final Long annotationCategoryId, final EntityId moduleId) {
		final var annotation = new AnnotationPojoTemplate();
		annotation
			.setCategoryId(annotationCategoryId)
			.setState(WorkingState.CANDIDATE)
			.setType(AnnotationType.RULE)
			.setName(ANNOTATION_NAME)
			.setModule(moduleId)
			.setLocation(new ModuleLocation(0, 10));
		return annotation;
	}
}
