/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static innowake.mining.data.core.annotation.impl.BusinessRuleIdentifier.ANNOTATION_NAME;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import com.google.common.collect.Sets;

import innowake.lib.job.api.management.JobManager;
import innowake.mining.data.core.annotation.AnnotationCandidates;
import innowake.mining.data.core.annotation.AnnotationPojoTemplate;
import innowake.mining.server.service.MiningDataCoreService;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.model.AnnotationCategory.RuleAnnotationCategory;
import innowake.mining.shared.model.AnnotationMetaDataReasonEnum;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.WorkingState;
import innowake.mining.shared.model.taxonomy.assignment.ModuleMatcher;

/**
 * BRE Stage 2 Business rule specific candidate Identification job tests, It will also test the Annotation metadata.
 */
@Disabled("Flaky (addresed in WMIN-11291)")
@WithMockUser
class StageTwoRuleIdentificationTest extends AbstractIdentificationTest {

	@Autowired
	private JobManager jobManager;

	@Autowired
	private AnnotationService annotationService;
	
	@Autowired
	private DataDictionaryService dataDictionaryService;
	
	@Autowired
	private MiningDataCoreService core;

	private static final String NO_ANNOTATION_MSG = "There must be no annotations present before the candidate identification job ran";

	private static final String MULTIPLE_IF_TRANSLATION = "If\n	Any of the following conditions are true:\n"
			+ "	TESTFIELD = '1'\n\t and A = '2'\n" + "Then\n"
			+ "	READ <i>TEST-FILE</i> \n" + "	If\n" + "	 	End of File Equal To false \n" + "	Then\n	 	 DISPLAY TESTFIELD ";

	private static final String MULTIPLE_IF_WITH_AT_END = "If\n	Any of the following conditions are true:\n"
			+ "	TESTFIELD = '3'\n\t or TESTFIELD = '4'\n" + "Then\n"
			+ "	EXEC SQL DECLARE <i>C1</i> CURSOR FOR SELECT TESTFIELD FROM IW_SQL_TEST ";

	private static final String LOOP_TRANSLATION = "While\n\tD Equal to 'Y'\nDo\n <i>PROC0100-PROCESS</i> THRU <i>PROC0100-EXIT</i> ";

	private static final String LOOP_IN_BRANCH_TRANSLATE = "If\n" + "	MYSYSIN-STATUS Equal to '00'\n" + "Then\n" + "	Do" +
	" <i>CHECK-MYSYSIN-COMMAND</i>\n"+ "	If\n" + "		MYSYSIN-STATUS Equal to '00'\n" + "	Then\n" +
			"		Do" + " <i>CONTROL-MYDATIN</i>\n	";

	private static final String EVALUATE_TRANSLATION = "If\n	B Equal to 5\n" + "Then\n	DISPLAY '1'\n" + "Otherwise\n	If\n		D Equal to 5\n"
			+ "	Then\n" + "		DISPLAY '1'\n	Otherwise\n		DISPLAY 'OTHER'\n	";

	private static final String ON_EXCEPTION_TRANSLATION = "If\n" + "	 Exception occured\n" + 
	"Then\n	Display 'XML DOCUMENT ERROR ' <i>XML-CODE</i>";
	private static final String ON_OVERFLOW_TRANSLATION = "If\n	 Overflow occured\n" + "Then\n	Display 'OVERFLOW!'";
	
	private static final String MULTIPLE_END_STATEMENTS = "If WS-END-DOC-X(IDX) Equal to SPACES Then move '9' to WS-END-DOC-X(IDX)";

	/**
	 * Check If the Annotation candidates identified for a Cobol program have IF_ELSE_CONDITION metadata
	 *
	 */
	@Test
	void identifyIfElseBRCandidates() {
		final String fileName = "BRE2_IFELSE.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_IFELSE", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<AnnotationMetaDataReasonEnum> reasons = Arrays.asList(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED);
		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(755, 183), reasons);
		metadataMap.put(new ModuleLocation(1020, 220), reasons);
		metadataMap.put(new ModuleLocation(1320, 127), reasons);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(820, 31), new ModuleLocation(869, 28)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(755, 183), Map.of(false, Set.of("WS-EOF"), true, Set.of("TESTFIELD")));
		linkedVariablesMap.put(new ModuleLocation(820, 31), Map.of(false, Set.of("WS-EOF"), true, Set.of("TESTFIELD")));
		linkedVariablesMap.put(new ModuleLocation(869, 28), Map.of(true, Set.of("TESTFIELD")));
		linkedVariablesMap.put(new ModuleLocation(1020, 220), Map.of(true, Set.of("TESTFIELD")));
		linkedVariablesMap.put(new ModuleLocation(1320, 127), Map.of(true, Set.of("TESTFIELD")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		final List<AnnotationPojo> ruleAnnotations = annotationsAfter.stream().filter(ann -> ann.getType() == AnnotationType.RULE)
				.collect(Collectors.toList());

		assertAnnotations(ruleAnnotations, locationMap, metadataMap, linkedVariablesMap);
	}
	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have IF_ELSE_CONDITION metadata
	 *
	 */
	@Test
	void identifyTransferBusinessVariable() {
		final String fileName = "BRE2_IFELSE_CHECK.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_IFELSE_CHECK", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<AnnotationMetaDataReasonEnum> reasons = Arrays.asList(AnnotationMetaDataReasonEnum.BV_TRANSFORMATION,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED);
		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(418, 96), reasons);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(350, 21), new ModuleLocation(384, 21)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(418, 96), Map.of(false, Set.of("A"), true, Set.of("C")));
		linkedVariablesMap.put(new ModuleLocation(350, 21), Map.of(false, Set.of("A"), true, Set.of("C", "B")));
		linkedVariablesMap.put(new ModuleLocation(384, 21), Map.of(false, Set.of("D"), true, Set.of("B")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		final List<AnnotationPojo> ruleAnnotations = annotationsAfter.stream().filter(ann -> ann.getType() == AnnotationType.RULE).collect(Collectors.toList());
		assertAnnotations(ruleAnnotations, locationMap, metadataMap, linkedVariablesMap);
	}
	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have MULTI_EXPRESSION_IF_ELSE_CONDITION metadata
	 *
	 */
	@Test
	void identifyMultiExpIfElseBRCandidates() {
		final String fileName = "BRE2_MULEXIFELSE.cbl";
		final String path = RESOURCE_PATH + fileName;
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, fileName, "BRE2_MULEXIFELSE.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, moduleId);

		final List<AnnotationMetaDataReasonEnum> reasons = Arrays.asList(AnnotationMetaDataReasonEnum.MULTI_EXPRESSION_IF_ELSE_CONDITION,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED, AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION);
		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(787, 146), reasons);
		metadataMap.put(new ModuleLocation(1004, 197), reasons);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(864, 28)));
		locationMap.put(RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(692, 25)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(787, 146), Map.of(false, Set.of("A"), true, Set.of("TESTFIELD")));
		linkedVariablesMap.put(new ModuleLocation(692, 25), Map.of(false, Set.of("B", "A"), true, Set.of("TESTFIELD")));
		linkedVariablesMap.put(new ModuleLocation(864, 28), Map.of(true, Set.of("TESTFIELD")));
		linkedVariablesMap.put(new ModuleLocation(1004, 197), Map.of(true, Set.of("TESTFIELD")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		final List<AnnotationPojo> ruleAnnotations = annotationsAfter.stream().filter(ann -> ann.getType() == AnnotationType.RULE)
				.collect(Collectors.toList());

		assertAnnotations(ruleAnnotations, locationMap, metadataMap, linkedVariablesMap);
	}

	/**
	 * Check If the Annotation candidates identified for a Cobol program have NESTED_IF_ELSE_CONDITION metadata
	 *
	 */
	@Test
	void identifyNestedIfElseBRCandidates() {
		final String fileName = "BRE2_NESTEDIFELSE.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_NESTEDIFELSE", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(545, 133), Arrays.asList(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION,
				AnnotationMetaDataReasonEnum.NESTED_IF_ELSE_CONDITION, AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));
		metadataMap.put(new ModuleLocation(822, 209), Arrays.asList(AnnotationMetaDataReasonEnum.COBOL_EVALUATE_CONDITION,
				AnnotationMetaDataReasonEnum.NESTED_IF_ELSE_CONDITION, AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));
		metadataMap.put(new ModuleLocation(1081, 144), Arrays.asList(AnnotationMetaDataReasonEnum.LOOP_CONDITION,
				AnnotationMetaDataReasonEnum.NESTED_IF_ELSE_CONDITION, AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(411, 25), new ModuleLocation(449, 25)));

		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(545, 133), Map.of(true, Set.of("D", "E")));
		linkedVariablesMap.put(new ModuleLocation(822, 209), Map.of(true, Set.of("D", "E")));
		linkedVariablesMap.put(new ModuleLocation(1081, 144), Map.of(true, Set.of("D", "E")));
		linkedVariablesMap.put(new ModuleLocation(411, 25), Map.of(false, Set.of("A", "B"), true, Set.of("D")));
		linkedVariablesMap.put(new ModuleLocation(449, 25), Map.of(false, Set.of("A", "C"), true, Set.of("E")));
		
		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		
		assertAnnotations(annotationsAfter, locationMap, metadataMap, linkedVariablesMap);
	}

	/**
	 * Check If the Annotation candidates identified for a Cobol program have INPUT_FROM_EXTERNAL_DATA_SOURCE metadata
	 *
	 */
	@Test
	void identifyDBAndFileInputBRCandidates() {
		final String fileName = "BRE2_EXTINPUT.cbl";
		final String path = RESOURCE_PATH + fileName;
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "BRE2_EXTINPUT", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, moduleId);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(954, 25), new ModuleLocation(997, 28)));
		locationMap.put(RuleAnnotationCategory.VALIDATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(758, 63), new ModuleLocation(1395, 212),
				new ModuleLocation(1687, 97), new ModuleLocation(897, 169)));

		final List<AnnotationMetaDataReasonEnum> reasons = Arrays.asList(AnnotationMetaDataReasonEnum.INPUT_FROM_EXTERNAL_DATA_SOURCE,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED);
		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(1148, 165), reasons);
		metadataMap.put(new ModuleLocation(1864, 119), reasons);
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(954, 25), Map.of(false, Set.of("WS-EOF")));
		linkedVariablesMap.put(new ModuleLocation(997, 28), Map.of(false, Set.of("MY-RECORD")));
		linkedVariablesMap.put(new ModuleLocation(758, 63), Map.of(false, Set.of("A")));
		linkedVariablesMap.put(new ModuleLocation(1395, 212), Map.of(false, Set.of("A")));
		linkedVariablesMap.put(new ModuleLocation(897, 169), Map.of(false, Set.of("B", "WS-EOF", "MY-RECORD")));
		linkedVariablesMap.put(new ModuleLocation(1687, 97), Map.of(false, Set.of("B")));
		linkedVariablesMap.put(new ModuleLocation(1148, 165), Map.of(false, Set.of("A"), true, Set.of("TESTFIELD")));
		linkedVariablesMap.put(new ModuleLocation(1864, 119), Map.of(false, Set.of("C"), true, Set.of("TESTFIELD")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		final List<AnnotationPojo> ruleAnnotations = annotationsAfter.stream().filter(ann -> ann.getType() == AnnotationType.RULE)
				.collect(Collectors.toList());

		assertAnnotations(ruleAnnotations, locationMap, metadataMap, linkedVariablesMap);
	}
	
	/**
	 * Check If the Annotation candidates identified for a Cobol program have COMPUTATION metadata
	 *
	 */
	@Test
	void identifyComputationCandidate() {
		final String fileName = "BRE2_COMPUTATION.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_COMPUTATION", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<AnnotationMetaDataReasonEnum> reasons = Arrays.asList(AnnotationMetaDataReasonEnum.COMPUTATION,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED);
		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(455, 93), reasons);
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(455, 93), Map.of(true, Set.of("C"), false, Set.of("TESTFIELD", "A", "B")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(annotationsAfter, Map.of(), metadataMap, linkedVariablesMap);
	}

	/**
	 * Check If the Annotation candidates identified for a Cobol program have COBOL_EVALUATE_CONDITION metadata
	 *
	 */
	@Test
	void identifyCobolEvalBRCandidates() {
		final String fileName = "BRE2_CBLEVALUATE.cbl";
		final String path = RESOURCE_PATH + fileName;
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "BRE2_CBLEVALUATE", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, moduleId);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(385, 25)));
		locationMap.put(RuleAnnotationCategory.VALIDATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(481, 292)));

		final List<AnnotationMetaDataReasonEnum> reasons = Arrays.asList(AnnotationMetaDataReasonEnum.COBOL_EVALUATE_CONDITION,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED);
		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(581, 168), reasons);
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(385, 25), Map.of(false, Set.of("A", "B"), true, Set.of("D")));
		linkedVariablesMap.put(new ModuleLocation(481, 292), Map.of(false, Set.of("B"), true, Set.of("D")));
		linkedVariablesMap.put(new ModuleLocation(581, 168), Map.of(true, Set.of("D")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		for (final AnnotationPojo annotationPojo : annotationsAfter) {
			core.annotationService.update(new AnnotationPojoPrototype().withId(annotationPojo.identity()).setEnglishTranslation("changed translation"));
		}
		submitIdentifyCandidatesJob(PROJECT_ID_1, moduleId);
		final List<AnnotationPojo> annotationsAfterSecondTimeIdentify = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		final Set<String> englishTranslations = annotationsAfter.stream().map((object) -> object.getEnglishTranslation().get()).collect(Collectors.toSet());
		annotationsAfterSecondTimeIdentify.forEach(annotation -> {
			assertTrue(englishTranslations.contains(annotation.getEnglishTranslation().get()));
		});
		assertAnnotations(annotationsAfter, locationMap, metadataMap, linkedVariablesMap);
	}

	/**
	 * Check If the Annotation candidates identified for a Cobol program have NESTED_EVALUATE_CONDITION metadata
	 *
	 */
	@Test
	void identifyNestedCobolEvalBRCandidates() {
		final String fileName = "BRE2_NESTEDCBLEVALUATE.cbl";
		final String path = RESOURCE_PATH + fileName;
		final EntityId moduleId = createCobolProgram(PROJECT_ID_1, "BRE2_NESTEDCBLEVALUATE", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, moduleId);

		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(550, 268), Arrays.asList(AnnotationMetaDataReasonEnum.COBOL_EVALUATE_CONDITION,
				AnnotationMetaDataReasonEnum.NESTED_EVALUATE_CONDITION, AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));
		metadataMap.put(new ModuleLocation(926, 193), Arrays.asList(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION,
				AnnotationMetaDataReasonEnum.NESTED_EVALUATE_CONDITION, AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));
		metadataMap.put(new ModuleLocation(1205, 203), Arrays.asList(AnnotationMetaDataReasonEnum.LOOP_CONDITION,
				AnnotationMetaDataReasonEnum.NESTED_EVALUATE_CONDITION, AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(416, 25), new ModuleLocation(454, 25)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(550, 268), Map.of(true, Set.of("D", "E")));
		linkedVariablesMap.put(new ModuleLocation(926, 193), Map.of(true, Set.of("D", "E")));
		linkedVariablesMap.put(new ModuleLocation(1205, 203), Map.of(true, Set.of("D", "E")));
		linkedVariablesMap.put(new ModuleLocation(416, 25), Map.of(true, Set.of("D"), false, Set.of("A", "B")));
		linkedVariablesMap.put(new ModuleLocation(454, 25), Map.of(true, Set.of("E"), false, Set.of("A", "C")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(annotationsAfter, locationMap, metadataMap, linkedVariablesMap);
	}

	/**
	 * Check If the Annotation candidates identified for a Cobol program have LOOP_CONDITION metadata
	 *
	 */
	@Test
	void identifyLoopCondBRCandidates() {
		final String fileName = "BRE2_LOOP.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_LOOP", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(226, 33)));

		final List<AnnotationMetaDataReasonEnum> reasons = Arrays.asList(AnnotationMetaDataReasonEnum.LOOP_CONDITION,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED);
		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(323, 157), reasons);
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(226, 33), Map.of(true, Set.of("TESTFIELD"), false, Set.of("A", "B")));
		linkedVariablesMap.put(new ModuleLocation(323, 157), Map.of(true, Set.of("TESTFIELD"), false, Set.of("A", "B")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(annotationsAfter, locationMap, metadataMap, linkedVariablesMap);
	}

	/**
	 * Check If the Annotation candidates identified for a Cobol program have NESTED_LOOP_CONDITION metadata
	 *
	 */
	@Test
	void identifyNestedLoopCondBRCandidates() {
		final String fileName = "BRE2_NESTEDLOOP.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_NESTEDLOOP", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(387, 102), Arrays.asList(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION,
				AnnotationMetaDataReasonEnum.NESTED_LOOP_CONDITION, AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));
		metadataMap.put(new ModuleLocation(532, 175), Arrays.asList(AnnotationMetaDataReasonEnum.COBOL_EVALUATE_CONDITION,
				AnnotationMetaDataReasonEnum.NESTED_LOOP_CONDITION, AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));
		metadataMap.put(new ModuleLocation(757, 109), Arrays.asList(AnnotationMetaDataReasonEnum.LOOP_CONDITION,
				AnnotationMetaDataReasonEnum.NESTED_LOOP_CONDITION, AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));


		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(266, 25), new ModuleLocation(299, 25)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(387, 102), Map.of(true, Set.of("C", "D")));
		linkedVariablesMap.put(new ModuleLocation(532, 175), Map.of(true, Set.of("C", "D")));
		linkedVariablesMap.put(new ModuleLocation(757, 109), Map.of(true, Set.of("C", "D")));
		linkedVariablesMap.put(new ModuleLocation(266, 25), Map.of(true, Set.of("C"), false, Set.of("A", "B")));
		linkedVariablesMap.put(new ModuleLocation(299, 25), Map.of(true, Set.of("D"), false, Set.of("A", "B")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(annotationsAfter, locationMap, metadataMap, linkedVariablesMap);
	}

	/**
	 * Check If the Annotation candidates identified for Exceptions or Abend statements in a file 
	 *
	 */
	@Test
	void identifyErrorProcessingRuleCandidates() {
		final String fileName = "ERROR_PROCESSING.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "ERROR_PROCESSING", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(776, 68)));
		locationMap.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(2152, 28)));
		locationMap.put(RuleAnnotationCategory.VALIDATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(2218, 79)));
		locationMap.put(RuleAnnotationCategory.ERROR_PROCESSING_RULE.getName(), Sets.newHashSet(new ModuleLocation(815, 29), new ModuleLocation(1103, 31),
				new ModuleLocation(1335, 70), new ModuleLocation(1420, 78), new ModuleLocation(1551, 81), new ModuleLocation(2399, 43)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(776, 68), Map.of());
		linkedVariablesMap.put(new ModuleLocation(2152, 28), Map.of());
		linkedVariablesMap.put(new ModuleLocation(2218, 79), Map.of());
		linkedVariablesMap.put(new ModuleLocation(815, 29), Map.of());
		linkedVariablesMap.put(new ModuleLocation(1103, 31), Map.of());
		linkedVariablesMap.put(new ModuleLocation(1335, 70), Map.of());
		linkedVariablesMap.put(new ModuleLocation(1420, 78), Map.of());
		linkedVariablesMap.put(new ModuleLocation(1551, 81), Map.of());
		linkedVariablesMap.put(new ModuleLocation(2399, 43), Map.of());

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(annotationsAfter, locationMap, Collections.emptyMap(), linkedVariablesMap);
	}

	/**
	 * Check If Technical Rule Candidate is identified for a Cobol Program to check if a program successfully wrote a file
	 *
	 */
	@Test
	void identifyTechnicalRuleCandidates1() {
		final String fileName = "BRE2_TECHNICAL1.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_TECHNICAL1", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(1080, 215)));
		locationMap.put(RuleAnnotationCategory.VALIDATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(1171, 106)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(1080, 215), Map.of(false, Set.of("TESTFILE-STATUS", "TESTFILE-ID")));
		linkedVariablesMap.put(new ModuleLocation(1171, 106), Map.of(false, Set.of("TESTFILE-ID")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(annotationsAfter, locationMap, Collections.emptyMap(), linkedVariablesMap);
	}

	/**
	 * Check If Technical Rule Candidate is identified for a Cobol Program that checks if a file exists
	 *
	 */
	@Test
	void identifyTechnicalRuleCandidates2() {
		final String fileName = "BRE2_TECHNICAL2.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_TECHNICAL2", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(844, 112)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(844, 112), Map.of(false, Set.of("FILE-STAT", "WS-FNAME")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(annotationsAfter, locationMap, Collections.emptyMap(), linkedVariablesMap);
	}

	/**
	 * Check If Technical Rule Candidate is identified for a Cobol Program to check if hit the end of the file
	 *
	 */
	@Test
	void identifyTechnicalRuleCandidates3() {
		final String fileName = "BRE2_TECHNICAL3.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_TECHNICAL3", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final List<AnnotationMetaDataReasonEnum> reasons = Arrays.asList(AnnotationMetaDataReasonEnum.INPUT_FROM_EXTERNAL_DATA_SOURCE,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED);
		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(711, 215), reasons);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(802, 25), new ModuleLocation(847, 29)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(711, 215), Map.of(false, Set.of("WS-EOF"), true, Set.of("WS-STUDENT")));
		linkedVariablesMap.put(new ModuleLocation(802, 25), Map.of(false, Set.of("WS-EOF")));
		linkedVariablesMap.put(new ModuleLocation(847, 29), Map.of(true, Set.of("WS-STUDENT")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(annotationsAfter, locationMap, metadataMap, linkedVariablesMap);
	}
	
	/**
	 * Check If Technical Rule Candidate is identified for a Cobol Program that consists of statements handling SQLCODE
	 *
	 */
	@Test
	void identifyTechicalRulesForSQLCODE() {
		final String fileName = "MMRS7112.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "MMRS7112", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(3431, 78), new ModuleLocation(4440, 295)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap = new HashMap<>();
		linkedVariablesMap.put(new ModuleLocation(3431, 78), Map.of(false, Set.of("MYSQLCA-SQLCODE")));
		linkedVariablesMap.put(new ModuleLocation(4440, 295), Map.of(false, Set.of("MYSQLIN-COUNTER", "MYSQLCA-SQLCODE"), true, Set.of("KSDS-PRIMARY-INDEX")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		final List<AnnotationPojo> ruleAnnotations = annotationsAfter.stream().filter(ann -> ann.getType() == AnnotationType.RULE)
				.collect(Collectors.toList());

		assertAnnotations(ruleAnnotations, locationMap, Collections.emptyMap(), linkedVariablesMap);
	}


	@Test
	void identifyFileAccessBusinessRules() {
		final String fileName = "MMRS7111.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "MMRS7111", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final Map<String, Set<ModuleLocation>> expectedAnnotations = new HashMap<>();
		expectedAnnotations.put(RuleAnnotationCategory.BUSINESS_RULE.getName(), Sets.newHashSet(new ModuleLocation(13367, 152), new ModuleLocation(13531, 152),
				new ModuleLocation(13695, 198), new ModuleLocation(13905, 199), new ModuleLocation(14116, 202), new ModuleLocation(14330, 249),
				new ModuleLocation(14591, 202)));
		expectedAnnotations.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(12957, 257), new ModuleLocation(12491, 200),
				new ModuleLocation(10643, 326)));
		expectedAnnotations.put(RuleAnnotationCategory.VALIDATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(14959, 465)));

		final Set<AnnotationMetaDataReasonEnum> reasons = Sets.newHashSet(AnnotationMetaDataReasonEnum.OUTPUT_TO_EXTERNAL_DATA_SOURCE,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED);
		final Map<ModuleLocation, Set<AnnotationMetaDataReasonEnum>> locationReasonMap = new HashMap<>();
		expectedAnnotations.get(RuleAnnotationCategory.BUSINESS_RULE.getName()).forEach(loc -> locationReasonMap.put(loc, reasons));

		final List<AnnotationPojo> createdAnnotations = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(expectedAnnotations, createdAnnotations, locationReasonMap);
	}

	/**
	 * Check If the Annotation candidates identified for a Cobol program have Business Rule Inside Validation Rule
	 *
	 */
	@Test
	void identifyBusinessRuleInsideValidationRule() {
		final String fileName = "BRE2_NESTEDIFELSE2.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_NESTEDIFELSE2", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(532, 108), Arrays.asList(AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));
		metadataMap.put(new ModuleLocation(654, 144), Arrays.asList(AnnotationMetaDataReasonEnum.LOOP_CONDITION,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED));

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.FIELD_COMPUTATION_RULE.getName(), Sets.newHashSet(new ModuleLocation(411, 25)));
		locationMap.put(RuleAnnotationCategory.VALIDATION_RULE.getName(),
				Sets.newHashSet(new ModuleLocation(507, 133), new ModuleLocation(688, 110)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> variablesLinked = new HashMap<>();
		variablesLinked.put(new ModuleLocation(688, 110), Map.of(false, Set.of("D")));
		variablesLinked.put(new ModuleLocation(507, 133), Map.of(true, Set.of("E"), false, Set.of("D")));
		variablesLinked.put(new ModuleLocation(411, 25), Map.of(true, Set.of("E"), false, Set.of("A", "C")));
		variablesLinked.put(new ModuleLocation(532, 108), Map.of(true, Set.of("E")));
		variablesLinked.put(new ModuleLocation(654, 144), Map.of(true, Set.of("E"), false, Set.of("D")));
		
		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(annotationsAfter, locationMap, metadataMap, variablesLinked);
	}

	private void assertAnnotations(final Map<String, Set<ModuleLocation>> expectedAnnotations, final List<AnnotationPojo> createdAnnotations,
			final Map<ModuleLocation, Set<AnnotationMetaDataReasonEnum>> locationReasonMap) {
		final Map<String, Set<ModuleLocation>> actualAnnotations = new HashMap<>();
		final Map<ModuleLocation, Set<AnnotationMetaDataReasonEnum>> actualLocationReasonMap = new HashMap<>();
		createdAnnotations.forEach(a -> {
			final String categoryName = a.getCategoryName().get();
			Set<ModuleLocation> locations = actualAnnotations.get(categoryName);
			if (locations == null) {
				locations = new HashSet<>();
				actualAnnotations.put(categoryName, locations);
			}
			final ModuleLocation location = a.getLocation().get();
			locations.add(location);

			if (RuleAnnotationCategory.BUSINESS_RULE.getName().equals(categoryName)) {
				actualLocationReasonMap.put(location, a.getReasons().stream().map(AnnotationMetaDataReasonEnum::valueOfIgnoreCase).collect(Collectors.toSet()));
			}
		});
		assertEquals(expectedAnnotations, actualAnnotations);

		if ( ! locationReasonMap.isEmpty()) {
			assertEquals(locationReasonMap, actualLocationReasonMap);
		}
	}

	/**
	 * Check If Technical Rule Candidate is identified for a Cobol Program to check if random access dataset record read/deleted or not (KSDS Dataset)
	 *
	 */
	@Test
	void identifyTechnicalRuleCandidates4() {
		final String fileName = "BRE2_TECHNICAL4.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "BRE2_TECHNICAL4", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(811, 60), new ModuleLocation(885, 77),
				new ModuleLocation(1110, 60), new ModuleLocation(1184, 65)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> variablesLinked = new HashMap<>();
		variablesLinked.put(new ModuleLocation(811, 60), Map.of());
		variablesLinked.put(new ModuleLocation(1110, 60), Map.of());
		variablesLinked.put(new ModuleLocation(1184, 65), Map.of());
		variablesLinked.put(new ModuleLocation(885, 77), Map.of(true, Set.of("WS-EMPL")));

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertAnnotations(annotationsAfter, locationMap, Collections.emptyMap(), variablesLinked);
	}

	/**
	 * Tests the BRE stage 2 business rule candidate identification with exclude annotations for excluding BR candidates.
	 */
	@Test
	void identifyBusinessRuleCandidatesWithExcludes() {
		final String fileName = "BRE2_EXTINPUT.cbl";
		final String path = RESOURCE_PATH + fileName;
		final EntityId moduleId = createCobolProgram(PROJECT_ID_2, "BRE2_EXTINPUT", fileName, RESOURCE_PATH);

		final Set<ModuleLocation> excludeLocations = new HashSet<>();
		excludeLocations.add(new ModuleLocation(1615, 235));
		excludeLocations.add(new ModuleLocation(1792, 192));

		createExcludeAnnotations(moduleId, excludeLocations);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_2).withModulePath(path));
		assertEquals(excludeLocations.size(), annotationsBefore.size(), "Only EXCLUDE annotations must be present");
		annotationsBefore.forEach(annotation -> assertEquals(AnnotationType.EXCLUDE, annotation.getType(), "EXCLUDE annotation type must match"));

		submitIdentifyCandidatesJob(PROJECT_ID_2, moduleId);

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_2).withModulePath(path));
		final Map<AnnotationType, List<AnnotationPojo>> annotationTypeMap = annotationsAfter.stream().collect(Collectors.groupingBy(AnnotationPojo::getType));
		final List<AnnotationPojo> excludeAnnotations = annotationTypeMap.get(AnnotationType.EXCLUDE);
		assertEquals(excludeLocations.size(), excludeAnnotations.size(),
						"Number of exclude candidate annotations must match");
		excludeAnnotations.forEach(annotation -> {
			final ModuleLocation location = annotation.getLocation().orElse(null);
			assertNotNull("Annotation FromModuleLocation must not be null", location);
			assertTrue("Exclude annotation FromModuleLocation must be present: " + location.toString(), excludeLocations.remove(location));
		});

		assertTrue("All expected ModuleLocation of exclude annotations must have been tested", excludeLocations.isEmpty());

		/* Excluded by EXCLUDE annotations:
		 * excludeLocations.add(new ModuleLocation(1611, 236));
		 * excludeLocations.add(new ModuleLocation(1788, 193));
		 */
		final List<AnnotationMetaDataReasonEnum> reasons = Arrays.asList(AnnotationMetaDataReasonEnum.INPUT_FROM_EXTERNAL_DATA_SOURCE,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED);
		final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap = new HashMap<>();
		metadataMap.put(new ModuleLocation(1148, 165), reasons);

		final Map<String, Set<ModuleLocation>> locationMap = new HashMap<>();
		locationMap.put(RuleAnnotationCategory.VALIDATION_RULE.getName(),
				Sets.newHashSet(new ModuleLocation(758, 63), new ModuleLocation(1395, 212), new ModuleLocation(897, 169)));
		locationMap.put(RuleAnnotationCategory.TECHNICAL_RULE.getName(), Sets.newHashSet(new ModuleLocation(997, 28), new ModuleLocation(954, 25)));
		
		final Map<ModuleLocation, Map<Boolean, Set<String>>> variablesLinked = new HashMap<>();
		variablesLinked.put(new ModuleLocation(1148, 165), Map.of(false, Set.of("A"), true, Set.of("TESTFIELD")));
		variablesLinked.put(new ModuleLocation(758, 63), Map.of(false, Set.of("A")));
		variablesLinked.put(new ModuleLocation(1395, 212), Map.of(false, Set.of("A")));
		variablesLinked.put(new ModuleLocation(897, 169), Map.of(false, Set.of("B", "WS-EOF", "MY-RECORD")));
		variablesLinked.put(new ModuleLocation(954, 25), Map.of(false, Set.of("WS-EOF")));
		variablesLinked.put(new ModuleLocation(997, 28), Map.of(false, Set.of("MY-RECORD")));

		final List<AnnotationPojo> ruleAnnotations = annotationTypeMap.get(AnnotationType.RULE);
		assertAnnotations(ruleAnnotations, locationMap, metadataMap, variablesLinked);
	}
	
	/**
	 * Check the English translation for the Annotation which contains "END-" in variable names or at the end
	 *
	 */
	@Test
	void testEnglishTranslationWithMultipleEnds() {
		final String fileName = "P441402.cbl";
		final String path = RESOURCE_PATH + fileName;
		createCobolProgram(PROJECT_ID_1, "P441402", fileName, RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(PROJECT_ID_1, path);
		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(PROJECT_ID_1).withModulePath(path));
		assertEquals(12, annotationsAfter.size());
	
		final List<AnnotationPojo> englishTranslations = annotationsAfter.stream()
				.filter(ann -> {
					final String englishTranslation = ann.getEnglishTranslation().orElse(null);
					final boolean containsTranslation;
					if (englishTranslation != null) {
						final String trimmedTranslation = englishTranslation.trim().replaceAll("\\s+", " ").replace("\r\n", "\n");
						final String multipleEndStatements = MULTIPLE_END_STATEMENTS.trim().replaceAll("\\s+", " ").replace("\r\n", "\n");
						containsTranslation = trimmedTranslation.contains(multipleEndStatements);
					} else {
						containsTranslation = false;
					}
					return englishTranslation != null && containsTranslation;
				})
				.collect(Collectors.toList());

		assertEquals(1, englishTranslations.size());
	}
	
	@Test
	void identifyIfElseBranchCandidates() {
		final String path = RESOURCE_PATH + "MMRS71B1.cbl";
		final EntityId projectId = createProject().identity();
		final EntityId moduleId = createCobolProgram(projectId, "MMRS71B1", "MMRS71B1.cbl", RESOURCE_PATH);

		final List<AnnotationPojo> annotationsBefore = annotationService.find(q -> q.ofProject(projectId).withModulePath(path));
		assertTrue(NO_ANNOTATION_MSG, annotationsBefore.isEmpty());

		submitIdentifyCandidatesJob(projectId, moduleId);
		
		final List<AnnotationMetaDataReasonEnum> expectedReasons = Arrays.asList(AnnotationMetaDataReasonEnum.BV_TRANSFORMATION,
				AnnotationMetaDataReasonEnum.BUSINESS_VARIABLE_IDENTIFIED, AnnotationMetaDataReasonEnum.IF_ELSE_CONDITION);
		
		final Set<String> expectedVars = Set.of("CICS-RESP", "DO-LOGIN-FIRST", "MYVSAMK-ALL", "MYVSAMK-KEY", "MYVSAMK-RESP");

		final List<AnnotationPojo> annotationsAfter = annotationService.find(q -> q.ofProject(projectId).withModulePath(path));
		assertEquals(11, annotationsAfter.size());
		
		final List<AnnotationPojo> annotationList = annotationsAfter.stream()
				.filter(ann -> ann.getLocation().map(ModuleLocation::getOffset)
						.map(offset -> offset.intValue() == 7240).orElse(false)).collect(Collectors.toList());
		
		assertEquals(1, annotationList.size());
		final AnnotationPojo annotation = annotationList.get(0);

		final Set<String> hasBusinessRuleCountList = dataDictionaryService.find(q -> q.ofAnnotation(annotation.identity())).stream()
																			.map(d -> d.getName())
																			.collect(Collectors.toSet());
		final List<AnnotationMetaDataReasonEnum> actualReasons = annotation.getReasons().stream()
				.map(AnnotationMetaDataReasonEnum::valueOfIgnoreCase).collect(Collectors.toList());
		assertEquals(expectedVars.size(), hasBusinessRuleCountList.size());
		assertEquals(expectedReasons.size(), actualReasons.size());
		assertTrue(expectedVars.containsAll(hasBusinessRuleCountList));
		assertTrue(actualReasons.containsAll(expectedReasons));
	}

	private void createExcludeAnnotations(final EntityId moduleId, final Set<ModuleLocation> excludeLocations) {
		final var annotationCandidates = excludeLocations.stream()
				.map(loc -> createAnnotation(moduleId, loc.getOffset(), loc.getLength()))
				.collect(Collectors.toList());

		final AnnotationCandidates candidates = new AnnotationCandidates();
		final Long cnt = candidates.store(moduleId, annotationCandidates, core);
		assertEquals(excludeLocations.size(), cnt.intValue(), "Number of created exclude annotations must match");
	}

	private static AnnotationPojoTemplate createAnnotation(final EntityId moduleId, final int offset, final int length) {
		final var annotation = new AnnotationPojoTemplate();
		annotation
			.setModule(moduleId)
			.setLocation(new ModuleLocation(offset, length))
			.setState(WorkingState.CANDIDATE)
			.setType(AnnotationType.EXCLUDE)
			.setName("Exclude at: " + offset + ", length: " + length);
		return annotation;
	}

	private void submitIdentifyCandidatesJob(final EntityId projectId, final String path) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Collections.emptyList(), Arrays.asList(path))));
	}

	private void submitIdentifyCandidatesJob(final EntityId projectId, final EntityId moduleId) {
		submitJob(jobManager, new IdentifyCandidatesJob(projectId, new ModuleMatcher(Arrays.asList(moduleId), Collections.emptyList())));
	}

	private void assertAnnotations(final List<AnnotationPojo> annotations, final Map<String, Set<ModuleLocation>> locationMap,
			final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap, final Map<ModuleLocation, Map<Boolean, Set<String>>> variablesLinked) {
		final Map<String, List<AnnotationPojo>> mapByCategory = annotations.stream().collect(
				Collectors.groupingBy(a -> a.getCategoryName().orElse(null)));
		for (final Map.Entry<String, List<AnnotationPojo>> entry : mapByCategory.entrySet()) {
			assertEnglishTranslation(entry.getValue());
			if (RuleAnnotationCategory.BUSINESS_RULE.getName().equals(entry.getKey())) {
				assertAnnotationData(entry.getValue(), metadataMap);
			} else {
				assertEquals(locationMap.get(entry.getKey()).size(), entry.getValue().size(),
						"The number of Expected and Actual annotations should be equal for Category: " + entry.getKey());
				assertRuleTypeAnnotation(entry.getValue(), locationMap.remove(entry.getKey()));
			}
		}
		assertTrue("All expected ModuleLocation must have been tested. Untested Locations : " + locationMap, locationMap.isEmpty());
		assertLinkedVariables(annotations, variablesLinked);
	}

	private void assertLinkedVariables(final List<AnnotationPojo> annotations, final Map<ModuleLocation, Map<Boolean, Set<String>>> linkedVariablesMap) {
		final Map<ModuleLocation, AnnotationPojo> locationMappedAnnotations = annotations.stream()
				.filter(ann -> ann.getType() == AnnotationType.RULE)
				.collect(Collectors.toMap(a -> a.getLocation().orElse(null), a -> a));
		locationMappedAnnotations.entrySet().forEach(entry -> {
			final ModuleLocation location = entry.getKey();
			final Map<Boolean, Set<String>> expectedLinks = linkedVariablesMap.remove(location);
			final Map<Boolean, Set<String>> actualLinks = dataDictionaryService.find(q -> q.ofAnnotation(entry.getValue().identity())).stream()
					.collect(Collectors.groupingBy(d -> d.getIsBusiness().orElse(Boolean.FALSE), Collectors.mapping(d -> d.getName(), Collectors.toSet())));
			assertEquals(expectedLinks, actualLinks, "Linked Variabled did not match for location " + location);
		});
		assertTrue("All expected variables should have been linked. Unlinked locations : " + linkedVariablesMap, linkedVariablesMap.isEmpty());
	}

	private void assertAnnotationData(final List<AnnotationPojo> annotations, final Map<ModuleLocation, List<AnnotationMetaDataReasonEnum>> metadataMap) {
		assertEquals(metadataMap.size(), annotations.size(), "Number of created rule candidate annotations must match");
		for (int i = 0; i < annotations.size(); i++) {
			final AnnotationPojo annotation = annotations.get(i);
			assertEquals(ANNOTATION_NAME, annotation.getName(), annotation.getCategoryName() + " candidate annotation name must match");
			assertEquals(AnnotationType.RULE, annotation.getType(), annotation.getCategoryName() + " candidate annotation type must match");

			final ModuleLocation location = annotation.getLocation().orElse(null);
			assertNotNull(annotation.getCategoryName() + " candidate annotation FromModuleLocation must not be null", location);

			final List<AnnotationMetaDataReasonEnum> expectedReasons = metadataMap.remove(location);

			/* Test Annotation Metadata */
			assertFalse("Annotation Metadata should not be empty", annotation.getReasons().isEmpty());
			final List<AnnotationMetaDataReasonEnum> reasons = annotation.getReasons().stream()
					.map(AnnotationMetaDataReasonEnum::valueOfIgnoreCase).collect(Collectors.toList());
			assertTrue("reasons are not same", reasons.containsAll(expectedReasons));
		}
		assertTrue("All expected ModuleLocation must have been tested. Untested Location : " + metadataMap, metadataMap.isEmpty());
	}

	private void assertRuleTypeAnnotation(final List<AnnotationPojo> annotations, final Set<ModuleLocation> brCandidateLocations) {
		for (final AnnotationPojo annotation : annotations) {
			assertEquals(AnnotationType.RULE, annotation.getType(), annotation.getCategoryName() + " candidate annotation type must match");
			
			final ModuleLocation location = annotation.getLocation().orElse(null);
			assertNotNull(annotation.getCategoryName() + " candidate annotation FromModuleLocation must not be null", location);
			assertTrue(annotation.getCategoryName() + " candidate annotation FromModuleLocation must not be unknown: " + location.toString(),
					brCandidateLocations.remove(location));
		}
		assertTrue("All expected ModuleLocation must have been tested. Untested Locations : " + brCandidateLocations, brCandidateLocations.isEmpty());
	}

	private void assertEnglishTranslation(final List<AnnotationPojo> annotations) {
		for (final AnnotationPojo annotation : annotations) {
			final String fileName = annotation.getModuleName();
			final int offset = annotation.getLocation().get().getOffset();
			if ("BRE2_MULEXIFELSE.cbl".equals(fileName)) {
				if (offset == 783) {
					/*       Candidate: Multiple expression If else condition */
					assertEquals(MULTIPLE_IF_TRANSLATION, annotation.getEnglishTranslation().orElse(null), "annotation translation can not be null");
				} else if (offset == 1000) {
					/*       Candidate: Multiple expression If else condition with AT End  */
					assertEquals(MULTIPLE_IF_WITH_AT_END, annotation.getEnglishTranslation().orElse(null), "annotation translation can not be null");
				}
			}
			
			if ("BRE2_CBLEVALUATE".equals(fileName)) {
				/*       Candidate: Evaluate expression */
				if (offset == 486) {
					assertEquals(EVALUATE_TRANSLATION, annotation.getEnglishTranslation().orElse(null), "annotation translation can not be null");
				}
			}

			if ("BRE2_NESTEDLOOP".equals(fileName)) {
				/*       Candidate: loop expression */
				if (offset == 620) {
					assertEquals(LOOP_TRANSLATION, annotation.getEnglishTranslation().orElse(null), "annotation translation can not be null");
				}
			}
			
			if ("MMRS7111".equals(fileName)) {
				/*.       Candidate: loop in branch statement */
				if (offset == 10759) {
					assertEquals(LOOP_IN_BRANCH_TRANSLATE, annotation.getEnglishTranslation().orElse(null), "annotation translation can not be null");
				}
			}
			
			if ("ERROR_PROCESSING".equals(fileName)) {
				/*...Error processing tests */
				if (offset == 1335) {
					assertEquals(ON_EXCEPTION_TRANSLATION, annotation.getEnglishTranslation().orElse(null), "annotation translation can not be null");
				}
				if (offset == 1103) {
					assertEquals(ON_OVERFLOW_TRANSLATION, annotation.getEnglishTranslation().orElse(null), "annotation translation can not be null");
				}
			}
		}
	}
}
