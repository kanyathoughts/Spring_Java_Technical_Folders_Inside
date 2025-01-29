/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.*;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.job.identification.AbstractIdentificationTest;
import innowake.mining.server.job.identification.IdentifyCandidatesJob;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.lang.NestedMap;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

import static org.junit.jupiter.api.Assertions.*;

@WithMockUser
class AnnotationImportServiceTest extends AbstractIdentificationTest {

	private final List<AnnotationPojo> annotations = new ArrayList<>();
	private final Map<String, Long> categories = new HashMap<>();
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private AnnotationImportService annotationImportService;
	@Autowired
	private JobManager jobManager;

	private void setUp() {
		final var entity = "Annotation";
		final var brBranch = createCobolProgram(PROJECT_ID_1, "BrBranch", "BrBranch.cbl", RESOURCE_PATH);
		submitJob(jobManager, new IdentifyCandidatesJob(PROJECT_ID_1, new ModuleMatcher(List.of(brBranch), null)));
		annotations.addAll(annotationService.find(q -> q.ofProject(PROJECT_ID_1).ofModule(brBranch)));
		assertEquals(13, annotations.size());
		final var stringProp = new CustomPropertyMetadata();
		stringProp.setName("stringProp");
		stringProp.setLabel("String Prop");
		stringProp.setDataType("String");
		stringProp.setFieldType(CustomPropertyFieldType.DEFAULT);
		stringProp.setPluginVisible(false);
		stringProp.setAutoCompletionKey("stringProp");
		customPropertiesService.defineProperty(PROJECT_ID_1, entity, "stringProp", stringProp);

		final var tagProp = new CustomPropertyMetadata();
		tagProp.setName("tagProp");
		tagProp.setLabel("Tag Prop");
		tagProp.setDataType("EMBEDDEDLIST");
		tagProp.setFieldType(CustomPropertyFieldType.TAG);
		tagProp.setPluginVisible(false);
		tagProp.setAutoCompletionKey("tagProp");
		customPropertiesService.defineProperty(PROJECT_ID_1, entity, "tagProp", tagProp);

		final String defaultClassName = customPropertiesService.getDefaultClassName(PROJECT_ID_1.getNid(), entity);
		final AnnotationPojoPrototype prototype = new AnnotationPojoPrototype()
				.withId(annotations.get(0).identity())
				.setCustomProperties(new NestedMap()
						.set(defaultClassName, "tagProp", List.of("Initial Value1", "Initial Value2"))
						.set(defaultClassName, "stringProp", "Initial Value"));

		annotationService.update(prototype);
		categories.putAll(annotationService.findCategories(q -> q.ofProjectWithDefault(PROJECT_ID_1)).stream()
					.collect(Collectors.toMap(AnnotationCategory::getName, AnnotationCategory::getId)));
	}

	@Test
	void testValidImportAnnotations() {
		if (annotations.isEmpty()) {
			setUp();
		}
		final var linesFromCsv = new ArrayList<Map<String, String>>();
		final var testIds = new ArrayList<Long>();
		for (int i = 2; i < 6; i++) {
			linesFromCsv.add(Map.of(AnnotationImportService.ANNOTATION_ID, annotations.get(i).getId().toString(),
					AnnotationImportService.ANNOTATION_DESCRIPTION, annotations.get(i).getName() + "Modified",
					AnnotationImportService.ANNOTATION_TYPE, "DEAD_CODE",
					AnnotationImportService.ANNOTATION_CATEGORY, "Close",
					AnnotationImportService.ANNOTATION_STATE, WorkingState.FOR_REVIEW.name()));
			testIds.add(annotations.get(i).getId());
		}
		final var requestingUser = "test";
		final var importAnnotationsJobResult = annotationImportService.importAnnotations(Optional.empty(), PROJECT_ID_1, linesFromCsv, requestingUser);
		assertEquals(importAnnotationsJobResult.getTotalLines(), linesFromCsv.size());
		final var testAnnotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).byNids(testIds));
		assertEquals(4, testAnnotations.size());
		for (final var annotation : testAnnotations) {
			assertTrue(annotation.getName().contains("Modified"));
			assertEquals(WorkingState.FOR_REVIEW, annotation.getState());
			assertEquals(AnnotationType.DEAD_CODE, annotation.getType());
			assertEquals(categories.get("Close"), annotation.getCategoryId().orElse(null));
			assertEquals(requestingUser, annotation.getUpdatedByUserId().orElse(null));
		}
	}

	@Test
	void testCustomPropertyUpdate() {
		if (annotations.isEmpty()) {
			setUp();
		}
		final var linesFromCsv = new ArrayList<Map<String, String>>();
		final var testIds = new ArrayList<Long>();
		for (int i = 0; i < 2; i++) {
			linesFromCsv.add(Map.of("Annotation Id", annotations.get(i).getId().toString(),
					"String Prop", "New Value",
					"Tag Prop", "New Value1, New Value2"));
			testIds.add(annotations.get(i).getId());
		}
		final var requestingUser = "test";
		final var result = annotationImportService.importAnnotations(Optional.empty(), PROJECT_ID_1, linesFromCsv, requestingUser);
		assertEquals(0, result.getErrors().size());
		final var testAnnotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).byNids(testIds));
		assertEquals(2, testAnnotations.size());
		final String defaultClassName = customPropertiesService.getDefaultClassName(PROJECT_ID_1.getNid(), "Annotation");
		for (final var annotation : testAnnotations) {
			assertEquals(requestingUser, annotation.getUpdatedByUserId().orElse(null));
			final var customProperties = annotation.getCustomProperties().getSub(defaultClassName);
			final var stringProp = customProperties.get("stringProp");
			assertEquals("New Value", stringProp);
		}

		testAnnotations.sort((a1, a2) -> (int) (a1.identity().getNid() - a2.identity().getNid()));

		/* Non string props were not updated */
		final var cpMap1 = testAnnotations.get(0).getCustomProperties().getSub(defaultClassName);
		final var cpMap2 = testAnnotations.get(1).getCustomProperties().getSub(defaultClassName);
		final var tagProp1 = cpMap1.getOrDefault("tagProp", null);
		final var tagProp2 = cpMap2.getOrDefault("tagProp", null);
		if (tagProp1 == null) {
			assertEquals(List.of("Initial Value1", "Initial Value2"), tagProp2);
		} else {
			assertEquals(List.of("Initial Value1", "Initial Value2"), tagProp1);
			assertNull(tagProp2);
		}
	}

	@Test
	void testImportAnnotationsWithInvalidAnnotationId() {
		if (annotations.isEmpty()) {
			setUp();
		}
		final var linesFromCsv = new ArrayList<Map<String, String>>();
		/* Annotation with ID 123456789 does not exist */
		linesFromCsv.add(Map.of(AnnotationImportService.ANNOTATION_ID, "123456789",
				AnnotationImportService.ANNOTATION_DESCRIPTION, "Test Annotation",
				AnnotationImportService.ANNOTATION_TYPE, "DEAD_CODE",
				AnnotationImportService.ANNOTATION_CATEGORY, "Close",
				AnnotationImportService.ANNOTATION_STATE, WorkingState.FOR_REVIEW.name()));
		/* Invalid Type */
		linesFromCsv.add(Map.of(AnnotationImportService.ANNOTATION_ID, annotations.get(6).getId().toString(),
				AnnotationImportService.ANNOTATION_DESCRIPTION, "Test Annotation",
				AnnotationImportService.ANNOTATION_TYPE, "DEAD",
				AnnotationImportService.ANNOTATION_CATEGORY, "Close",
				AnnotationImportService.ANNOTATION_STATE, WorkingState.FOR_REVIEW.name()));
		/* Invalid Category */
		linesFromCsv.add(Map.of(AnnotationImportService.ANNOTATION_ID, annotations.get(7).getId().toString(),
				AnnotationImportService.ANNOTATION_DESCRIPTION, "Test Annotation",
				AnnotationImportService.ANNOTATION_TYPE, "DEAD_CODE",
				AnnotationImportService.ANNOTATION_CATEGORY, "Closed",
				AnnotationImportService.ANNOTATION_STATE, WorkingState.FOR_REVIEW.name()));
		/* Invalid State */
		linesFromCsv.add(Map.of(AnnotationImportService.ANNOTATION_ID, annotations.get(8).getId().toString(),
				AnnotationImportService.ANNOTATION_DESCRIPTION, "Test Annotation",
				AnnotationImportService.ANNOTATION_TYPE, "DEAD_CODE",
				AnnotationImportService.ANNOTATION_CATEGORY, "Close",
				AnnotationImportService.ANNOTATION_STATE, "FOR_ANALYSIS"));
		/* Category Type Mismatch */
		linesFromCsv.add(Map.of(AnnotationImportService.ANNOTATION_ID, annotations.get(9).getId().toString(),
				AnnotationImportService.ANNOTATION_DESCRIPTION, "Test Annotation",
				AnnotationImportService.ANNOTATION_TYPE, AnnotationType.RULE.name(),
				AnnotationImportService.ANNOTATION_CATEGORY, "Close",
				AnnotationImportService.ANNOTATION_STATE, WorkingState.FOR_REVIEW.name()));
		/* Valid entry */
		linesFromCsv.add(Map.of(AnnotationImportService.ANNOTATION_ID, annotations.get(10).getId().toString(),
				AnnotationImportService.ANNOTATION_DESCRIPTION, "Test Annotation",
				AnnotationImportService.ANNOTATION_TYPE, AnnotationType.RULE.name(),
				AnnotationImportService.ANNOTATION_CATEGORY, "Validation Rule",
				AnnotationImportService.ANNOTATION_STATE, WorkingState.REJECTED.name()));
		final var requestingUser = "test";
		final var result = annotationImportService.importAnnotations(Optional.empty(), PROJECT_ID_1, linesFromCsv, requestingUser);
		assertEquals(5, result.getErrors().size());
		final var updatedAnnotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).byNids(annotations.subList(6, 11).stream()
																												.map(AnnotationPojo::getId)
																												.collect(Collectors.toList()))).stream()
															.collect(Collectors.toMap(AnnotationPojo::getId, annotation -> annotation));
		final var validUpdate = updatedAnnotations.get(annotations.get(10).getId());
		assertEquals("Test Annotation", validUpdate.getName());
		assertEquals(AnnotationType.RULE, validUpdate.getType());
		assertEquals(categories.get("Validation Rule"), validUpdate.getCategoryId().orElse(null));
		assertEquals(WorkingState.REJECTED, validUpdate.getState());
		assertEquals(requestingUser, validUpdate.getUpdatedByUserId().orElse(null));
		for (int i = 6; i < 10; i++) {
			final var invalidUpdate = updatedAnnotations.get(annotations.get(i).getId());
			assertEquals(annotations.get(i).getName(), invalidUpdate.getName());
			assertEquals(annotations.get(i).getType(), invalidUpdate.getType());
			assertEquals(annotations.get(i).getCategoryId().orElse(null), invalidUpdate.getCategoryId().orElse(null));
			assertEquals(annotations.get(i).getState(), invalidUpdate.getState());
			assertEquals(annotations.get(i).getUpdatedByUserId().orElse(null), invalidUpdate.getUpdatedByUserId().orElse(null));
		}
		LinkedList<String> errors = result.getErrors();
		assertEquals("Line 2: Annotation with ID 123456789 not found", errors.get(0));
		assertEquals("Line 3: Annotation Type DEAD is invalid", errors.get(1));
		assertEquals("Line 4: Category Closed not found", errors.get(2));
		assertEquals("Line 5: State FOR_ANALYSIS is invalid", errors.get(3));
		assertEquals("Line 6: Annotation Type RULE is invalid for category Close", errors.get(4));
	}

	@Override
	protected List<String> getAdditionalPgScriptFile() {
		return List.of("test-data-custom-properties");
	}

}
