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
import innowake.mining.data.core.storeast.impl.AbstractNaturalTest;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;

/**
 * Tests regarding the (new) business rule identification core logic.
 */
@TestInstance(Lifecycle.PER_CLASS)
class NaturalBusinessRuleIdentifierTest extends AbstractNaturalTest {

	Stream<Arguments> testCases() {
		/* Acceptance Criteria for business rule candidates:
		 * For all BRANCH_STATEMENT check if any of the following applies:
		 * 	- The condition of the statement contains at least two FIELD_REFERENCE or CONSTANT_REFERENCE
		 * 	- The condition of the statement contains at least one FIELD_REFERENCE from a Copybook
		 * 	- Also check if any of the following applies to the bodies of the statement:
		 * 	- The body contains an ASSIGNMENT_STATEMENT with an ARITHMETIC_EXPRESSION on its right hand side
		 * 	- The body contains an ARITHMETIC_STATEMENT
		 * 	- The body contains another BRANCH_STATEMENT
		 * 	- The body contains a DATABASE_ACCESS_STATEMENT
		 * 	- The body contains a FILE_ACCESS_STATEMENT
		 *
		 * Do not create an Annotation if the new Annotation would be inside of an EXIT paragraph.
		 * 
		 * All modules contain comments why a BRANCH_STATEMENT got identified as a BR candidate or not */
		return Stream.of(
				arguments(new String[] { "BrIfCpy.nsp", "BRGDA.nsiwg", "BRPDA.NSIWA", "BRLDA.NSIWA"}, Arrays.asList(L(129, 53), L(226, 53), L(323, 53),
						L(420, 52), L(521, 58), L(665, 66))),
				arguments(new String[] { "BrIf.nsp"}, Arrays.asList(L(365, 45), L(579, 50), L(705, 40))),
				arguments(new String[] { "BrAssignArithExpr.nsp" }, Arrays.asList(L(583, 60), L(724, 38), L(843, 45), L(1019, 38), L(1138, 45),
																				  L(1314, 38), L(1433, 45), L(1559, 43), L(1683, 45), L(1859, 38),
																				  L(1978, 43), L(2152, 38), L(2271, 45), L(2397, 46), L(2574, 39))),
				arguments(new String[] { "BrBranch.nsp" }, Arrays.asList(L(389, 112), L(558, 229), L(1022, 386), L(1503, 170), L(2067, 373), L(2293, 73))),
				arguments(new String[] { "BrDbAccess.nsp" }, Arrays.asList(L(311, 93), L(464, 109), L(633, 120), L(813, 122), L(1028, 42), L(1301, 69))),
				arguments(new String[] { "BrFileAccess.nsp" }, Arrays.asList(L(524, 69), L(698, 80), L(839, 115)))
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
		final var identifiedAnnotations = ruleIdentifier.identify(rootNode, Collections.emptyMap(), Technology.NATURAL, Collections.emptyMap());
		assertEquals("Number of annotation module locations must match", expectedLocations.size(), identifiedAnnotations.size());

		for (int index = 0; index < expectedLocations.size(); index++) {
			final var annotation = identifiedAnnotations.get(index);
			assertEquals("Type of annotation must be rule", AnnotationType.RULE, annotation.type.get());
			assertEquals("Business Rule Candidate [System identified]", annotation.name.get());

			final ModuleLocation actualLocation = annotation.location.orElse(null);
			assertNotNull("Actual module location must not be null", actualLocation);

			final Tuple2<Integer, Integer> expectedLocation = expectedLocations.get(index);
			assertEquals("Offset must match", expectedLocation.a, actualLocation.getOffset());
			assertEquals("Length must match", expectedLocation.b, actualLocation.getLength());
		}
	}
}
