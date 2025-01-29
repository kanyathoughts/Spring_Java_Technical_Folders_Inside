/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;

/**
 * Tests for module's requires review on discovery with incremental changes
 */
@WithMockUser
class IncrementalDiscoveryRequiresReviewTest extends BaseIncrementalDiscoveryTest {
	
	private static final String ASSERT_FAILURE_MSG_FOR_REQUIRES_REVIEW_TRUE = "Requires review must be true after module with metadata changed";
	private static final String TESTCBL = "TESTCBL";
	@Autowired
	private DataDictionaryService dataDictionaryService;
	@Autowired
	private TaxonomyService taxonomyService;
	@Autowired
	private AnnotationService annotationService;
	
	/**
	 * Test case to validate module's requires review field based on description.
	 *
	 * @throws IOException if the files are not found
	 */
	@Test
	void testRequiresReviewOnDescription() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).withName(TESTCBL))
													.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for name: " + TESTCBL));
			ModulePojoPrototype proto = new ModulePojoPrototype().setUid(module.getUid()).setDescription("Updated desccription for the module");
			moduleService.update(proto);
		};
		final Consumer<EntityId> secondRunDiscovery = validateRequiresReviewForTrue(ASSERT_FAILURE_MSG_FOR_REQUIRES_REVIEW_TRUE);

		doTest("sourceContentChangeV0", "sourceContentChangeV1", firstRunDiscovery, secondRunDiscovery);
	}
	
	/**
	 * Test case to validate module's requires review field based on data dictionary.
	 *
	 * @throws IOException if the files are not found
	 */
	@Test
	void testRequiresReviewOnDataDictionary() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final EntityId moduleId = moduleService.findAnyModuleId(b -> b.ofProject(projectId).withName(TESTCBL))
					.orElseThrow(() -> new MiningEntityNotFoundException("Module with path must exist: " + TESTCBL));
			createDataDictionaryForModule(moduleId);
		};
		final Consumer<EntityId> secondRunDiscovery = validateRequiresReviewForTrue(ASSERT_FAILURE_MSG_FOR_REQUIRES_REVIEW_TRUE);

		doTest("sourceContentChangeV0", "sourceContentChangeV1", firstRunDiscovery, secondRunDiscovery);
	}
	
	/**
	 * Test case to validate module's requires review field based on annotation.
	 *
	 * @throws IOException if the files are not found
	 */
	@Test
	void testRequiresReviewOnAnnotation() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final EntityId moduleId = moduleService.findAnyModuleId(b -> b.ofProject(projectId).withName(TESTCBL))
					.orElseThrow(() -> new MiningEntityNotFoundException("Module with path must exist: " + TESTCBL));
			createAnnotationForModule(projectId, moduleId);
		};
		final Consumer<EntityId> secondRunDiscovery = validateRequiresReviewForTrue(ASSERT_FAILURE_MSG_FOR_REQUIRES_REVIEW_TRUE);

		doTest("sourceContentChangeV0", "sourceContentChangeV1", firstRunDiscovery, secondRunDiscovery);
	}
	
	/**
	 * Test case to validate module's requires review field based on taxonomy.
	 *
	 * @throws IOException if the files are not found
	 */
	@Test
	void testRequiresReviewOnTaxonomy() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final EntityId moduleId = moduleService.findAnyModuleId(b -> b.ofProject(projectId).withName(TESTCBL))
					.orElseThrow(() -> new MiningEntityNotFoundException("Module with path must exist: " + TESTCBL));
			createTaxonomyForModule(projectId, moduleId);
		};
		final Consumer<EntityId> secondRunDiscovery = validateRequiresReviewForTrue(ASSERT_FAILURE_MSG_FOR_REQUIRES_REVIEW_TRUE);

		doTest("sourceContentChangeV0", "sourceContentChangeV1", firstRunDiscovery, secondRunDiscovery);
	}
	
	/**
	 * Test case to validate module's requires review is false when there is no change in the content of the module
	 *
	 * @throws IOException if the files are not found
	 */
	@Test
	void testRequiresReviewOnNoChangeInModule() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).withName(TESTCBL))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for name: " + TESTCBL));
			createMetaDataForModule(projectId, module);
		};
		final Consumer<EntityId> secondRunDiscovery = validateRequiresReviewForFalse("Requires review must be false if the module is not changed");

		doTest("sourceContentChangeV1", "sourceContentChangeV1", firstRunDiscovery, secondRunDiscovery);
	}
	
	/**
	 * Test case to validate module's requires review remains to the previous value when the module is not changed 
	 *
	 * @throws IOException if the files are not found
	 */
	@Test
	void testRequiresReviewRetainsPreviousValue() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).withName(TESTCBL))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for name: " + TESTCBL));
			ModulePojoPrototype proto = new ModulePojoPrototype().setRequiresReview(true).setUid(module.getUid()).setDescription("New desciption");
			moduleService.update(proto);
		};
		final Consumer<EntityId> secondRunDiscovery = validateRequiresReviewForTrue("Requires review must be true if the module's previous value is true");

		doTest("sourceContentChangeV1", "sourceContentChangeV1", firstRunDiscovery, secondRunDiscovery);
	}
	
	/**
	 * Test case to validate the backup and restore of meta data
	 * 
	 * <p>
	 * Run discover code and metrics, then create meta data for a Module in the project and changing the source object of the project Run discover code and
	 * metrics for the second time, validate meta data is restored for the module.
	 * </p>
	 *
	 * @throws IOException if the files are not found
	 */
	@Test
	void validateMetadataOnRestore() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).withName(TESTCBL))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for name: " + TESTCBL));
			createMetaDataForModule(projectId, module);
		};

		doTest("sourceContentChangeV0", "sourceContentChangeV1", firstRunDiscovery, this::validateMetaDataForModule);
	}
	
	/**
	 * Test case to validate module's requires review remains false even though the module is changed as it has no metadata
	 *
	 * @throws IOException if the files are not found
	 */
	@Test
	void testRequiresReviewOnNoMetadata() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {};
		final Consumer<EntityId> secondRunDiscovery = validateRequiresReviewForFalse("Requires review must be false if the module has no metadata");

		doTest("sourceContentChangeV0", "sourceContentChangeV1", firstRunDiscovery, secondRunDiscovery);
	}

	private void createDataDictionaryForModule(final EntityId moduleId) {
		final DataDictionaryPojoPrototype prototype = new DataDictionaryPojoPrototype()
				.setName("MYVSAMK-RECORD")
				.setModule(moduleId)
				.setLocation(new ModuleLocation(1, 10))
				.setDescription("A record ")
				.setCreatedByUserId("test");
		dataDictionaryService.create(prototype);
	}
	
	private void createMetaDataForModule(final EntityId projectId, final ModulePojo module) {
		ModulePojoPrototype proto = new ModulePojoPrototype().setUid(module.getUid()).setDescription("Updated module description");
		moduleService.update(proto);
		createDataDictionaryForModule(module.identity());
		createTaxonomyForModule(projectId, module.identity());
		createAnnotationForModule(projectId, module.identity());
	}

	private void createAnnotationForModule(final EntityId projectId, final EntityId moduleId) {
		final Long annotationCategory = annotationService.createCategory(projectId, "Annotation Category B", Collections.emptyList());
		annotationService.create(new AnnotationPojoPrototype()
				.setName("Annotation 1")
				.setCreatedByUserId("admin")
				.setState(WorkingState.IN_ANALYSIS)
				.setType(AnnotationType.RULE)
				.setCategoryId(annotationCategory)
				.setLocation(new ModuleLocation(47, 11))
				.setModule(moduleId));
	}

	private void createTaxonomyForModule(final EntityId projectId, final EntityId moduleId) {
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype().setName("DataDomain").setProject(projectId));
		final TaxonomyPojoPrototype taxonomy = new TaxonomyPojoPrototype()
				.setName("TaxonomyPojoPrototype 1")
				.setProject(projectId)
				.setType(type);
		final EntityId taxonId = taxonomyService.create(taxonomy);
		taxonomyService.createModuleLink(moduleId.getUid(), taxonId);
	}

	private void validateMetaDataForModule(final EntityId projectId) {
		final List<EntityId> moduleIds = moduleService.findModulesWithMetaData(projectId);
		final ModulePojo metaDataModule = moduleService.getModule(moduleIds.get(0));
		assertEquals(TESTCBL, metaDataModule.getName());
		assertEquals("Updated module description", metaDataModule.getDescription().orElse(null));
		validateDataDictionaryRestored(moduleIds.get(0));
		validateTaxonomyRestored(projectId, moduleIds.get(0));
		validateAnnotationRestored(projectId);
	}

	private void validateAnnotationRestored(final EntityId projectId) {
		final List<AnnotationCategory> annotationCategories = annotationService.findCategories(q -> q.ofProjectWithDefault(projectId));
		/* We don't care about the first 8 AnnotationCategories created by the migration V1.2.94 */
		annotationCategories.sort((a, b) -> a.getId().compareTo(b.getId()));
		final AnnotationCategory annotationCategory = annotationCategories.get(annotationCategories.size() - 1);
		assertEquals("Annotation Category B", annotationCategory.getName());
		
		final AnnotationPojo annotation = annotationService.get(q -> q.ofProject(projectId).withName("Annotation 1"));
		assertEquals("Annotation 1", annotation.getName());
		assertEquals(projectId.getNid(), annotation.getProjectNid());
	}

	private void validateTaxonomyRestored(final EntityId projectId, final EntityId moduleId) {
		final TaxonomyPojo taxonomy = taxonomyService.find(q -> q.ofProject(projectId).ofModule(moduleId)).get(0);
		assertEquals("TaxonomyPojoPrototype 1", taxonomy.getName());
		assertEquals(projectId, taxonomy.getProject());
		assertEquals("DataDomain", taxonomy.getType().getName());
	}

	private void validateDataDictionaryRestored(final EntityId moduleId) {
		final DataDictionaryPojo dataDictionaryEntry = dataDictionaryService.find(q -> q.ofModule(moduleId)).get(0);
		assertEquals("test", dataDictionaryEntry.getCreatedByUserId());
		assertEquals("MYVSAMK-RECORD", dataDictionaryEntry.getName());
		assertEquals("A record ", dataDictionaryEntry.getDescription());
		assertNotNull(dataDictionaryEntry.getLocation());
	}
	
	private Consumer<EntityId> validateRequiresReviewForTrue(final String assertFailureMessage) {
		final Consumer<EntityId> secondRunDiscovery = projectId -> {
			final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).withName(TESTCBL))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for name: " + TESTCBL));
			assertTrue(module.isRequiresReview(), assertFailureMessage);
		};
		return secondRunDiscovery;
	}
	
	private Consumer<EntityId> validateRequiresReviewForFalse(final String assertFailureMessage) {
		final Consumer<EntityId> secondRunDiscovery = projectId -> {
			final ModulePojo module = moduleService.findAnyModule(b -> b.ofProject(projectId).withName(TESTCBL))
					.orElseThrow(() -> new MiningEntityNotFoundException("Could not find module for name: " + TESTCBL));
			assertFalse(module.isRequiresReview(), assertFailureMessage);
		};
		return secondRunDiscovery;
	}
}
