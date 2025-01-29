/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation;

import static java.util.Collections.emptyList;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.params.provider.Arguments.arguments;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import org.javatuples.Triplet;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.annotation.api.AnnotationIdentifier;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;

/**
 * Tests regarding the (new) business rule identification core logic.
 */
@TestInstance(Lifecycle.PER_CLASS)
class CobolBusinessRuleIdentifierTest extends AnnotationIdentificationTest {

	private static final String TECHNICAL_RULE_NAME = "Technical Rule Candidate [System identified]";
	private static final String VALIDATION_RULE_NAME = "Data Validation Rule Candidate [System identified]";
	private static final String FIELD_COMPUTATION_RULE_NAME = "Field Computation Rule Candidate [System identified]";

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
				arguments("NORULEMODULE.cbl", emptyList()),
				arguments("BrIfCpy.cbl", Arrays.asList(T(381, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(525, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(690, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(855, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(998, VALIDATION_RULE_NAME, AnnotationType.RULE))),
				arguments("BrIf.cbl", Arrays.asList(T(564, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(704, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(888, VALIDATION_RULE_NAME, AnnotationType.RULE))),
				arguments("BrAssignArithExpr.cbl", Arrays.asList(T(1702, FIELD_COMPUTATION_RULE_NAME, AnnotationType.RULE),
						T(1732, FIELD_COMPUTATION_RULE_NAME, AnnotationType.RULE),
						T(1755, FIELD_COMPUTATION_RULE_NAME, AnnotationType.RULE),
						T(1787, FIELD_COMPUTATION_RULE_NAME, AnnotationType.RULE),
						T(403, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(567, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(724, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(892, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1099, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1278, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1446, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1611, VALIDATION_RULE_NAME, AnnotationType.RULE))),
				arguments("BrBranch.cbl", Arrays.asList(T(387, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(545, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(929, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1276, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1541, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1876, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(2061, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(2357, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(2538, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(2710, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(2936, VALIDATION_RULE_NAME, AnnotationType.RULE))),
				arguments("BrDbAccess.cbl", Arrays.asList(T(240, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(438, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(699, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(954, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1254, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1439, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1641, VALIDATION_RULE_NAME, AnnotationType.RULE))),
				arguments("BrFileAccess.cbl", Arrays.asList(T(957, TECHNICAL_RULE_NAME, AnnotationType.RULE),
						T(1000, TECHNICAL_RULE_NAME, AnnotationType.RULE),
						T(1658, TECHNICAL_RULE_NAME, AnnotationType.RULE),
						T(1698, TECHNICAL_RULE_NAME, AnnotationType.RULE),
						T(745, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(892, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1147, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1332, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(1516, VALIDATION_RULE_NAME, AnnotationType.RULE))),
				arguments("BrExitParagraph.cbl", Arrays.asList(T(563, VALIDATION_RULE_NAME, AnnotationType.RULE))),

				/* Candidates include copybooks. The ModuleLocations are set with offsets and lengths for the assembled content.
				 * However the Annotation ModuleLocations should have the values for the unassembled content */
				arguments("BrBranch2.cbl", Arrays.asList(T(403, VALIDATION_RULE_NAME, AnnotationType.RULE),
						T(617, VALIDATION_RULE_NAME, AnnotationType.RULE))),
				/* Ignore all candidates that are located in copybooks */
				arguments("BrBranch3.cbl", emptyList())
		);
	}

	/**
	 * Tests that the number of identified business rule candidate annotations matches the size of the given list and verifies the location information.
	 *
	 * @param fileName the name of the file the annotations should be identified from
	 * @param expectedLocations the expected location information, first the offset and second the length, if no length is given only the offset is checked
	 */
	@ParameterizedTest(name = "{0}")
	@MethodSource("testCases")
	void test(final String fileName, final List<Triplet<Integer, String, AnnotationType>> expectedLocations) {
		final AstNodePojo rootNode = createAst("identifier/rule", fileName).get(null);
		
		final AnnotationIdentifier ruleIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.RULE);
		final List<AnnotationPojoTemplate> identifiedAnnotations = ruleIdentifier.identify(rootNode, Collections.emptyMap(), Technology.COBOL, Collections.emptyMap());
		assertEquals("Number of annotation module locations must match", expectedLocations.size(), identifiedAnnotations.size());

		for (int index = 0; index < identifiedAnnotations.size(); index++) {
			final var annotation = identifiedAnnotations.get(index);
			final Triplet<Integer, String, AnnotationType> triplet = expectedLocations.get(index);
			final ModuleLocation actualLocation = annotation.location.orElse(null);
			assertNotNull("Actual module location must not be null", actualLocation);
			assertEquals("Offset must match", triplet.getValue0(), actualLocation.getOffset());
			assertEquals(triplet.getValue1(), annotation.name.get());
			assertEquals("Type of annotation must be rule", triplet.getValue2(), annotation.type.get());
			assertNotNull("Actual module location must not be null", actualLocation);
		}
	}
	
	/**
	 * @param offset The offset value
	 * @param name The name of the validation rule
	 * @param type the {@link AnnotationType}
	 * @return A {@link Tuple2} with the {@link Integer} values of the given {@code offset} and {@code length}
	 */
	protected static final Triplet<Integer, String, AnnotationType> T(final int offset, final String name, final AnnotationType type) {
		return Triplet.with(Integer.valueOf(offset), name, type);
	}
}
