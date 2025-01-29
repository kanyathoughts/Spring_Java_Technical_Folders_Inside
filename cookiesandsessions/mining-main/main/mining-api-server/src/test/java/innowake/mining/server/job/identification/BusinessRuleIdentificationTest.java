/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static innowake.mining.data.core.annotation.impl.BusinessRuleIdentifier.ANNOTATION_NAME;
import static innowake.mining.shared.model.Technology.NATURAL;
import static innowake.mining.shared.model.Type.PROGRAM;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.javatuples.Triplet;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.model.AnnotationMetaDataReasonEnum;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * Business rule specific candidate Identification job tests, It will also test the Annotation metadata. 
 */
@Disabled("Flaky (addresed in WMIN-11291)")
@WithMockUser
class BusinessRuleIdentificationTest extends AbstractIdentificationTest {

	@Autowired
	private JobManager jobManager;

	@Autowired
	private AnnotationService annotationService;
	
	private static final String NO_ANNOTATION_MSG = "There must be no annotations present before the candidate identification job ran";
	
	private static final String BUSINESS_RULE = "Business Rule";
	private static final String FIELD_COMPUTATION_RULE = "Field Computation Rule";
	private static final String TECHNICAL_RULE = "Technical Rule";
	private static final String VALIDATION_RULE = "Validation Rule";
	private static final String ERROR_PROCESSING_RULE = "Error Processing Rule";
	
	private static final String BUSINESS_RULE_NAME = "Business Rule Candidate [System identified]";
	private static final String VALIDATION_RULE_NAME = "Data Validation Rule Candidate [System identified]";
	private static final String FIELD_COMPUTATION_RULE_NAME = "Field Computation Rule Candidate [System identified]";

	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have IF_ELSE_CONDITION metadata
	 *
	 */
	@Test
	void identifyIfElseBRCandidates() {
		final String path = RESOURCE_PATH + "IFELSE.cbl";
		createCobolProgram(PROJECT_ID_1, "IFELSE", "IFELSE.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<Triplet<Integer, String, AnnotationType>> brCandidateLocations = new ArrayList<>();
		brCandidateLocations.add(T(Integer.valueOf(569), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(709), VALIDATION_RULE_NAME, AnnotationType.RULE));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertEquals(brCandidateLocations.size(), annotationsAfter.size(), "Number of created BR candidate annotations must match");

		assertAnnotationData(annotationsAfter, brCandidateLocations, Arrays.asList(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION));
	}

	/**
	 * Check If the Annotation candidates have updated business rule values
	 *
	 */
	@Test
	void updateBusinessRulesTest() {
		final String path = RESOURCE_PATH + "CICSPGM2.cbl";
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "CICSPGM2", "CICSPGM2.cbl", RESOURCE_PATH);
		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofDataDictionaryEntry(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());
		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofModule(moduleId).withCategory(BUSINESS_RULE));
		assertEquals(1, annotationsAfter.size(), "This has 2 business rules with no annotation referencing entry before update and 3 total" +
				", After update, should have only 1 business rule" );
		final List<AnnotationPojo> nonBusinessRules = annotationsAfter.stream()
				.filter(annotation -> ! BUSINESS_RULE.equals(annotation.getCategoryName()
						.orElseThrow(() -> new IllegalStateException("Category name must be present")))).collect(Collectors.toList());
		assertTrue("Only business rules should be contained in the annotation list.", nonBusinessRules.isEmpty());
	}
	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have business rules or not
	 *
	 */
	@Test
	void identifyAllCandidates() {
		final String path = RESOURCE_PATH + "BABKCMP.cbl";
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "BABKCMP", "BABKCMP.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());
		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<AnnotationPojo> businessRulesAfter = annotationService.find(q -> q.ofModule(moduleId).withCategory(BUSINESS_RULE));
		assertEquals(6, businessRulesAfter.size(), "Number of created BR candidate annotations must match");
		final List<AnnotationPojo> fieldComputationRulesAfter = annotationService.find(q -> q.ofModule(moduleId).withCategory(FIELD_COMPUTATION_RULE));
		assertEquals(31, fieldComputationRulesAfter.size(), "Number of created BR candidate annotations must match");
		final List<AnnotationPojo> technicalRulesAfter = annotationService.find(q -> q.ofModule(moduleId).withCategory(TECHNICAL_RULE));
		assertEquals(0, technicalRulesAfter.size(), "Number of created BR candidate annotations must match");
		final List<AnnotationPojo> validationRulesAfter = annotationService.find(q -> q.ofModule(moduleId).withCategory(VALIDATION_RULE));
		assertEquals(54, validationRulesAfter.size(), "Number of created BR candidate annotations must match");
		final List<AnnotationPojo> errorRulesAfter = annotationService.find(q -> q.ofModule(moduleId).withCategory(ERROR_PROCESSING_RULE));
		assertEquals(2, errorRulesAfter.size(), "Number of created BR candidate annotations must match");	
	}
	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have MULTI_EXPRESSION_IF_ELSE_CONDITION metadata
	 *
	 */
	@Test
	void identifyMultiExpIfElseBRCandidates() {
		final String path = RESOURCE_PATH + "MULEXIFELSE.cbl";
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "MULEXIFELSE", "MULEXIFELSE.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, moduleId);

		final List<Triplet<Integer, String, AnnotationType>> brCandidateLocations = new ArrayList<>();
		brCandidateLocations.add(T(Integer.valueOf(557), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(738), VALIDATION_RULE_NAME, AnnotationType.RULE));
		
		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotationData(annotationsAfter, brCandidateLocations, Arrays.asList(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION,
				AnnotationMetaDataReasonEnum.MULTI_EXPRESSION_IF_ELSE_CONDITION));
	}
	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have LOOP_CONDITION metadata
	 *
	 */
	@Test
	void identifyLoopCondBRCandidates() {
		final String path = RESOURCE_PATH + "LOOP.cbl";
		createCobolProgram(PROJECT_ID_1, "LOOP", "LOOP.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<Triplet<Integer, String, AnnotationType>> brCandidateLocations = new ArrayList<>();
		brCandidateLocations.add(T(Integer.valueOf(232), VALIDATION_RULE_NAME, AnnotationType.RULE));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotationData(annotationsAfter, brCandidateLocations, Arrays.asList(AnnotationMetaDataReasonEnum.LOOP_CONDITION));
	}
	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have COBOL_EVALUATE_CONDITION and NESTED_CONDITION metadata
	 *
	 */
	@Test
	void identifyNestedAndCobolEvalBRCandidates() {
		final String path = RESOURCE_PATH + "CBLEVALUATE.cbl";
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "CBLEVALUATE", "CBLEVALUATE.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, moduleId);

		final List<Triplet<Integer, String, AnnotationType>> brCandidateLocations = new ArrayList<>();
		brCandidateLocations.add(T(Integer.valueOf(405), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(775), VALIDATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1119), VALIDATION_RULE_NAME, AnnotationType.RULE));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotationData(annotationsAfter, brCandidateLocations, Arrays.asList(AnnotationMetaDataReasonEnum.COBOL_EVALUATE_CONDITION,
				AnnotationMetaDataReasonEnum.NESTED_CONDITION));
	}
	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have COMPUTATION metadata
	 *
	 */
	@Test
	void identifyComputationBRCandidates() {
		final String path = RESOURCE_PATH + "COMPUTATION.cbl";
		createCobolProgram(PROJECT_ID_1, "COMPUTATION", "COMPUTATION.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<Triplet<Integer, String, AnnotationType>> brCandidateLocations = new ArrayList<>();
		brCandidateLocations.add(T(Integer.valueOf(414), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(578), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(735), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(903), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1289), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1454), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1545), FIELD_COMPUTATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1575), FIELD_COMPUTATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1598), FIELD_COMPUTATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1630), FIELD_COMPUTATION_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1110), VALIDATION_RULE_NAME, AnnotationType.RULE));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotationData(annotationsAfter, brCandidateLocations, Arrays.asList(AnnotationMetaDataReasonEnum.COMPUTATION, AnnotationMetaDataReasonEnum.BV_TRANSFORMATION, 
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));
	}
	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have INPUT_FROM_EXTERNAL_DATA_SOURCE metadata
	 *
	 */
	@Test
	void identifyDBAndFileInputBRCandidates() {
		final String path = RESOURCE_PATH + "EXTINPUT.cbl";
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "EXTINPUT", "EXTINPUT.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, moduleId);

		final List<Triplet<Integer, String, AnnotationType>> brCandidateLocations = new ArrayList<>();
		brCandidateLocations.add(T(Integer.valueOf(753), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(900), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1158), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1413), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1713), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(1898), BUSINESS_RULE_NAME, AnnotationType.RULE));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		final List<AnnotationPojo> annotations = annotationsAfter.stream().filter(ann -> ANNOTATION_NAME.equals(ann.getName())).collect(Collectors.toList());
		assertAnnotationData(annotations, brCandidateLocations, Arrays.asList(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION, AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));
	}
	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have OUTPUT_TO_EXTERNAL_DATA_SOURCE metadata
	 *
	 */
	@Test
	void identifyDBAndFileOutputBRCandidates() {
		final String path = RESOURCE_PATH + "EXTOUTPUT.cbl";
		createCobolProgram(PROJECT_ID_1, "EXTOUTPUT", "EXTOUTPUT.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<Triplet<Integer, String, AnnotationType>> brCandidateLocations = new ArrayList<>();

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		final List<AnnotationPojo> annotations = annotationsAfter.stream().filter(ann -> ANNOTATION_NAME.equals(ann.getName())).collect(Collectors.toList());
		assertAnnotationData(annotations, brCandidateLocations, Arrays.asList(AnnotationMetaDataReasonEnum.OUTPUT_TO_EXTERNAL_DATA_SOURCE));
	}
	
	/**
	 * Check If the Annotation candidates identified for a Natural program have IF_ELSE_CONDITION metadata
	 *
	 */
	@Test
	void identifyNaturalBRCandidates() {
		final String path = RESOURCE_PATH + "BrIf.nsp";
		final EntityId moduleId = createModule(PROJECT_ID_1, "BrIf", "BrIf.nsp", RESOURCE_PATH, NATURAL, PROGRAM);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, moduleId);

		final List<Triplet<Integer, String, AnnotationType>> brCandidateLocations = new ArrayList<>();
		brCandidateLocations.add(T(Integer.valueOf(347), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(551), BUSINESS_RULE_NAME, AnnotationType.RULE));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotationData(annotationsAfter, brCandidateLocations, Arrays.asList(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION));
	}
	
	/**
	 * Check If the Annotation candidates identified for a Pl1 program  have IF_ELSE_CONDITION metadata
	 *
	 */
	@Test
	void identifyPl1BRCandidates() {
		final String path = RESOURCE_PATH + "BrBranch.pl1";
		createModule(PROJECT_ID_1, "BrBranch", "BrBranch.pl1", RESOURCE_PATH, Technology.PL1, PROGRAM);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<Triplet<Integer, String, AnnotationType>> brCandidateLocations = new ArrayList<>();
		brCandidateLocations.add(T(Integer.valueOf(527), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(630), BUSINESS_RULE_NAME, AnnotationType.RULE));
		brCandidateLocations.add(T(Integer.valueOf(741), BUSINESS_RULE_NAME, AnnotationType.RULE));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotationData(annotationsAfter, brCandidateLocations, Arrays.asList(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION));
	}
	
	private void assertAnnotationData(final List<AnnotationPojo> annotations, final List<Triplet<Integer, String, AnnotationType>> brCandidateLocations,
			final List<AnnotationMetaDataReasonEnum> expectedReasons) {
		assertEquals(brCandidateLocations.size(), annotations.size(), "Number of created BR candidate annotations must match");
		for (int i = 0; i < annotations.size(); i++) {
			final AnnotationPojo annotation = annotations.get(i);

			final ModuleLocation location = annotation.getLocation().orElse(null);
			final Triplet<Integer, String, AnnotationType> triplet = brCandidateLocations.get(i);
			assertNotNull("BR candidate annotation FromModuleLocation must not be null", location);
			/* check location */
			assertEquals(triplet.getValue0(), location.getOffset(), "Location of candidates need to be the same");
			/* check name */
			assertEquals(triplet.getValue1(), annotation.getName(), "BR candidate annotation name must match");
			/* check type */
			assertEquals(triplet.getValue2(), annotation.getType(), "BR candidate annotation type must match");

			if ( ! annotation.getReasons().isEmpty()) {
				/* Test Annotation Metadata */
				final List<AnnotationMetaDataReasonEnum> reasons = annotation.getReasons().stream().map(
						metaData -> AnnotationMetaDataReasonEnum.valueOfIgnoreCase(metaData)).collect(Collectors.toList());
				assertTrue(expectedReasons + " must contain all: " + reasons, expectedReasons.containsAll(reasons));
			}
		}
	}
	
	private void submitIdentifyCandidatesJob(final EntityId projectId, final String path) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Collections.emptyList(), Arrays.asList(path))));
	}

	private void submitIdentifyCandidatesJob(final EntityId projectId, final EntityId moduleId) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
	}
	
	/**
	 * @param offset The offset value
	 * @param name The expected name value
	 * @param type The expected type
	 * @return A {@link Triplet} with the {@code offset}, {@code name} and {@code type)}
	 */
	protected static final Triplet<Integer, String, AnnotationType> T(final int offset, final String name, final AnnotationType type) {
		return Triplet.with(Integer.valueOf(offset), name, type);
	}
}
