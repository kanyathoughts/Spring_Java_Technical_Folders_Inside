/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.annotation.api.AnnotationIdentifier;
import innowake.mining.data.core.storeast.impl.AbstractPl1Test;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.lang.FailLater;
import innowake.mining.shared.lang.Functions;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;

/**
 * Tests regarding the business rule identification core logic for PL1.
 */
@TestInstance(Lifecycle.PER_CLASS)
class Pl1BusinessRuleIdentifierTest extends AbstractPl1Test {

	Stream<Arguments> testCases() {
		return Stream.of(
				arguments(new String[] { "NORULEMODULE.pl1" }, Collections.emptyList()),
				arguments(new String[] { "BrIf.pl1" }, Arrays.asList(L(200, 117), L(317, 95), L(412, 34))),
				arguments(new String[] { "BrBranch.pl1" }, Arrays.asList(L(548, 107), L(655, 115), L(770, 120), L(890, 60))),
				arguments(new String[] { "BrBranch2.pl1" }, Arrays.asList(L(213, 60))),
				arguments(new String[] { "BrAssignArithExpr.pl1" }, Arrays.asList(L(507, 42) ,L(549, 42), L(591, 41), L(632, 90))),
				arguments(new String[] { "BrDbAccess.pl1" }, Arrays.asList(L(318, 140), L(458, 106))),
				arguments(new String[] { "BrFileAccess.pl1" }, Arrays.asList(L(363, 59)))
		);
	}

	/**
	 * Tests that the number of identified annotations matches the size of the given list and verifies the location information.
	 *
	 * @param fileNames the names of the files the annotations should be identified from
	 * @param expectedLocations the expected location information, first the offset and second the length, if no length is given only the offset is checked
	 */
	@ParameterizedTest(name = "{0}")
	@MethodSource("testCases")
	void test(final String[] fileNames, final List<Tuple2<Integer, Integer>> expectedLocations) {
		final AstNodePojo rootNode = createAst("identifier/rule", fileNames).get(null);
		final AnnotationIdentifier ruleIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.RULE);
		final var identifiedAnnotations = ruleIdentifier.identify(rootNode, Collections.emptyMap(), Technology.PL1, Collections.emptyMap());
		final FailLater failAll = new FailLater(Arrays.toString(fileNames));
		failAll.later(() -> assertEquals("Number of annotation module locations must match", expectedLocations.size(), identifiedAnnotations.size()));
		
		Functions.loop(0, index -> index < identifiedAnnotations.size(), index -> index + 1, index -> {
			final FailLater failRule = new FailLater("Rule #" + index);
			
			final var annotation = identifiedAnnotations.get(index);
			failRule.later(() -> assertEquals("Type of annotation must be rule", AnnotationType.RULE, annotation.type.get()));
			failRule.later(() -> assertEquals("Business Rule Candidate [System identified]", annotation.name.get()));
			
			final ModuleLocation actualLocation = annotation.location.orElse(null);
			failRule.later(() -> {
				assertNotNull("Actual module location must not be null", actualLocation);
				final Tuple2<Integer, Integer> expectedLocation = expectedLocations.size() > index ? expectedLocations.get(index) : L(-1, -1);
				failRule.later(() -> assertEquals("Offset must match", expectedLocation.a, actualLocation.getOffset()));
				failRule.later(() -> assertEquals("Length must match", expectedLocation.b, actualLocation.getLength()));
			});
			
			failAll.later(failRule::now);
		});
		failAll.now();
	}

}
