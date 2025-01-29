/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.junit.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.annotation.api.AnnotationIdentifier;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationCategory.RuleAnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;

/**
 * Tests regarding the business rule identification core logic.
 */
public class RuleIdentificationTest extends AnnotationIdentificationTest {

	@Test
	public void testRuleIdentificationShouldFindNothingWhenNoRulesArePresent() {
		final AstNodePojo rootNode = createAst("identifier/rule", "NORULEMODULE.cbl").get(null);
		final AnnotationIdentifier ruleIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.RULE);
		final var identifiedAnnotations = ruleIdentifier.identify(rootNode, Collections.emptyMap(), Technology.COBOL, Collections.emptyMap());
		assertTrue(identifiedAnnotations.isEmpty());
	}
	
	@Test
	public void testIfCategoryGetsSet() {
		final AstNodePojo rootNode = createAst("identifier/rule", "IFELSE.cbl").get(null);
		final AnnotationIdentifier ruleIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.RULE);
		final Map<String, Long> annotationCategoryMap = new HashMap<>();
		annotationCategoryMap.put(RuleAnnotationCategory.BUSINESS_RULE.getName(), 1L);
		annotationCategoryMap.put(RuleAnnotationCategory.VALIDATION_RULE.getName(), 2L);
		final var identifiedAnnotations = ruleIdentifier.identify(rootNode, annotationCategoryMap, Technology.COBOL, Collections.emptyMap());
		for(final var a : identifiedAnnotations) {
			assertEquals(Long.valueOf(2), a.categoryId.get());
			assertEquals(a.name.get(), expectedCategory(RuleAnnotationCategory.VALIDATION_RULE));
		}
	}
	
	@Test
	public void testIfElseIdentification() {
		/* The length of the annotation should be 335 */
		test("IFELSE.cbl", Arrays.asList(L(503)), expectedCategory(RuleAnnotationCategory.VALIDATION_RULE));
	}
	
	@Test
	public void testIfEndIfIdentification() {
		test("IFENDIF.cbl", Arrays.asList(L(539)), expectedCategory(RuleAnnotationCategory.VALIDATION_RULE));
	}
	
	@Test
	public void testIfElseEndIfIdentification() {
		test("IFELSEENDIF.cbl", Arrays.asList(L(503)), expectedCategory(RuleAnnotationCategory.VALIDATION_RULE));
	}
	
	@Test
	public void testNestedIfInIfIdentification() {
		test("NESTEDIF.cbl", Arrays.asList(L(501)), expectedCategory(RuleAnnotationCategory.VALIDATION_RULE));
	}
	
	@Test
	public void testNestedIfInElseIdentification() {
		/* OFF BY ONE, the actual length is 608 */
		/* OFF BY ONE, the actual length is 289 */
		test("NESTEDELSE.cbl", Arrays.asList(L(501)), expectedCategory(RuleAnnotationCategory.VALIDATION_RULE));
	}

	@Test
	public void testSimpleEvaluateIdentification() {
		/* OFF BY ONE, actual length 366 */
		test("SIMPLEEVALUATE.cbl", Arrays.asList(L(503)), expectedCategory(RuleAnnotationCategory.VALIDATION_RULE));
	}

	@Test
	public void testNestedEvaluateInIfIdentification() {
		test("EVALUATEINIF.cbl", Arrays.asList(L(442)), expectedCategory(RuleAnnotationCategory.VALIDATION_RULE));
	}

	@Test
	public void testComplexProgramRuleIdentification() {
		test("REAL.cbl", Arrays.asList(L(4026, RuleAnnotationCategory.FIELD_COMPUTATION_RULE), L(5267, RuleAnnotationCategory.FIELD_COMPUTATION_RULE),
				L(3812, RuleAnnotationCategory.TECHNICAL_RULE), L(5053, RuleAnnotationCategory.TECHNICAL_RULE),
				L(2055, RuleAnnotationCategory.VALIDATION_RULE), L(4172, RuleAnnotationCategory.VALIDATION_RULE),
				L(5340, RuleAnnotationCategory.VALIDATION_RULE), L(8993, RuleAnnotationCategory.VALIDATION_RULE), L(9942, RuleAnnotationCategory.VALIDATION_RULE)));
	}
	
	@Test
	public void testComputationRuleIdentification() {
		test("COMPUTATION.cbl", Arrays.asList(L(403), L(567), L(724), L(899), L(1069)), expectedCategory(RuleAnnotationCategory.VALIDATION_RULE));
	}

	
	private void test(final String fileName, final List<ImmutablePair<Integer, String>> expectedLocations) {
		test(fileName, expectedLocations, null);
		
	}

	/**
	 * Checks that the number of identified annotations matches the size of the given list and verifies the location information.
	 *
	 * @param fileName the name of the file the annotations should be identified from
	 * @param expectedLocations the expected location information, first the offset and second the length, if no length is given only the offset is checked
	 */
	private void test(final String fileName, final List<ImmutablePair<Integer, String>> expectedLocations, @Nullable final String ruleAnnotationCategoryName) {
		final AstNodePojo rootNode = createAst("identifier/rule", fileName).get(null);
		
		final AnnotationIdentifier ruleIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.RULE);
		final var identifiedAnnotations = ruleIdentifier.identify(rootNode, Collections.emptyMap(), Technology.COBOL, Collections.emptyMap());
		assertEquals(expectedLocations.size(), identifiedAnnotations.size());

		for (int index = 0; index < expectedLocations.size(); index++) {
			final var annotation = identifiedAnnotations.get(index);
			assertEquals(AnnotationType.RULE, annotation.type.get());
			final String ruleName;
			/* This means that each rule has its own unique type */
			if (ruleAnnotationCategoryName == null) {
				ruleName = expectedLocations.get(index).right;
			} else {
				ruleName = ruleAnnotationCategoryName;
			}
			assertEquals(ruleName, annotation.name.get());
			final ImmutablePair<Integer, String> expectedLocation = expectedLocations.get(index);
			final ModuleLocation actualLocation = annotation.location.orElse(null);
			assertNotNull(actualLocation);
			final Integer expectedOffset = expectedLocation.left;
			final Integer actualOffset = actualLocation.getOffset();
			assertEquals(expectedOffset, actualOffset);
			
		}
	}

	private ImmutablePair<Integer, String> L(final int value, final AnnotationCategory.RuleAnnotationCategory annotationCategory) {
		return ImmutablePair.of(Integer.valueOf(value), expectedCategory(annotationCategory));
	}
	
	private ImmutablePair<Integer, String> L(final int value) {
		return ImmutablePair.of(Integer.valueOf(value), null);
	}
	/*
	 *  BUSINESS_RULE("Business Rule"),
		TECHNICAL_RULE("Technical Rule"),
		VALIDATION_RULE("Validation Rule"),
		FIELD_COMPUTATION_RULE("Field Computation Rule"),
		ERROR_PROCESSING_RULE("Error Processing Rule"),
		UNKNOWN;
	 */
	
	@Nullable
	private String expectedCategory(final RuleAnnotationCategory validationRule) {
		switch (validationRule) {
			case VALIDATION_RULE:
				return "Data Validation Rule Candidate [System identified]";
			case TECHNICAL_RULE:
				return "Technical Rule Candidate [System identified]";
			case FIELD_COMPUTATION_RULE:
				return "Field Computation Rule Candidate [System identified]";
			case ERROR_PROCESSING_RULE:
				return "Error Processing Rule Candidate [System identified]";
			case BUSINESS_RULE:
				return "Business Rule Candidate [System identified]";
			case UNKNOWN:
				return "Unknown AnnotationCategory";
			
		}
		return null;
	}
}
