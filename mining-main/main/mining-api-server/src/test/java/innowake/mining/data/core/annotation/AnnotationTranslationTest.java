/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.annotation;

import static org.junit.Assert.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Collections;

import org.junit.Test;

import innowake.mining.data.core.annotation.api.AnnotationIdentifier;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Technology;

/**
 * Test for English translation of the rules.
 */
public class AnnotationTranslationTest extends AnnotationIdentificationTest {
	final static String TRANSLATE_MULTIPLE_WHEN ="If\n"
			+ "	<i>A</i> Equal to 5 OR 6\n"
			+ "OR 	<i>A</i> Equal to 7\n"
			+ "Then\n"
			+ "	Display '5'";

	@Test
	public void testRuleIdentificationShouldFindNothingWhenNoRulesArePresent() {
		final AstNodePojo rootNode = createAst("identifier/rule", "MultipleWhen.cbl").get(null);
		final AnnotationIdentifier ruleIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.RULE);
		final var identifiedAnnotations = ruleIdentifier.identify(rootNode, Collections.emptyMap(), Technology.COBOL, Collections.emptyMap());
		assertFalse(identifiedAnnotations.isEmpty());
		assertEquals(TRANSLATE_MULTIPLE_WHEN, identifiedAnnotations.get(0).englishTranslation.get());
	}

}