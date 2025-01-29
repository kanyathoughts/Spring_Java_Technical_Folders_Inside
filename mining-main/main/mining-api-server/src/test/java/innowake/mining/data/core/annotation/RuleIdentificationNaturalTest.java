/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Test;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.annotation.api.AnnotationIdentifier;
import innowake.mining.data.core.storeast.impl.AbstractNaturalTest;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;

/**
 * Tests regarding the business rule identification core logic.
 */
public class RuleIdentificationNaturalTest extends AbstractNaturalTest {

	private static final Integer NULL_LENGTH = Integer.valueOf(Integer.MIN_VALUE);

	@Test
	public void testDecideForNaturalRuleIdentification() {
		test("DecideFor.nsp", Arrays.asList(L(150, 112)));
	}
	
	@Test
	public void testIfNaturalRuleIdentification() {
		test("IF.NSP", Arrays.asList(L(138, 131), L(273, 77), L(354, 162)));
	}
	
	@Test
	public void testAddNaturalRuleIdentification() {
		test("ADD.nsp", Arrays.asList(L(140, 60)));
	}
	
	@Test
	public void testComputeNaturalRuleIdentification() {
		test("COMPUTE.nsp", Arrays.asList(L(165, 45)));
	}
	
	@Test
	public void testMultiplyNaturalRuleIdentification() {
		test("MULTIPLY.nsp", Arrays.asList(L(192, 73)));
	}
	
	@Test
	public void testSubtractNaturalRuleIdentification() {
		test("SUBTRACT.nsp", Arrays.asList(L(132, 45)));
	}
	
	/**
	 * Checks that the number of identified annotations matches the size of the given list and verifies the location information.
	 *
	 * @param fileName the name of the file the annotations should be identified from
	 * @param expectedLocations the expected location information, first the offset and second the length, if no length is given only the offset is checked
	 */
	private void test(final String fileName, final List<Tuple2<Integer, Integer>> expectedLocations) {
		final AstNodePojo rootNode = createAst("identifier/rule", fileName).get(null);
		
		final AnnotationIdentifier ruleIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.RULE);
		final var identifiedAnnotations = ruleIdentifier.identify(rootNode, Collections.emptyMap(), Technology.NATURAL, Collections.emptyMap());
		assertEquals(expectedLocations.size(), identifiedAnnotations.size());

		for (int index = 0; index < expectedLocations.size(); index++) {
			final var annotation = identifiedAnnotations.get(index);
			assertEquals(AnnotationType.RULE, annotation.type.get());
			assertEquals("Business Rule Candidate [System identified]", annotation.name.get());
			final Tuple2<Integer, Integer> expectedLocation = expectedLocations.get(index);
			final ModuleLocation actualLocation = annotation.location.orElse(null);
			assertNotNull(actualLocation);
			final Integer expectedOffset = expectedLocation.a;
			final Integer actualOffset = actualLocation.getOffset();
			assertEquals(expectedOffset, actualOffset);
			
			if (expectedLocation.b != NULL_LENGTH) {
				final Integer expectedLength = expectedLocation.b;
				final Integer actualLength = actualLocation.getLength();
				assertEquals(expectedLength, actualLength);
			}
		}
	}
}
