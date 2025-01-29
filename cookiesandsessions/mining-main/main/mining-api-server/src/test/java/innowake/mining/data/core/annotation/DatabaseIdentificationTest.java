/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.annotation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Collections;

import org.junit.Test;

import innowake.mining.data.core.annotation.api.AnnotationIdentifier;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;

/**
 * Tests regarding identifying annotations based on database statements.
 */
public class DatabaseIdentificationTest extends AnnotationIdentificationTest {
	
	@Test
	public void testDatabaseIdentificationShouldNotFindAnythingWhenProgramHasNoExecSqlStatements() {
		final AstNodePojo rootNode = createAst("identifier/database", "NODBMODULE.cbl").get(null);
		final AnnotationIdentifier databaseIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.DATABASE);
		final var identifiedAnnotations = databaseIdentifier.identify(rootNode, Collections.emptyMap(), Technology.COBOL, Collections.emptyMap());
		assertTrue(identifiedAnnotations.isEmpty());
	}
	
	@Test
	public void testIdentifyDatabaseAnnotationsBasedOnExecSqlStatements() {
		LOG.debug(() -> "testIdentifyDatabaseAnnotationsBasedOnExecSqlStatements");
		final AstNodePojo rootNode = createAst("identifier/database", "MMRS7112.cbl").get(null);
		final AnnotationIdentifier databaseIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.DATABASE);
		final var identifiedAnnotations = databaseIdentifier.identify(rootNode, Collections.emptyMap(), Technology.COBOL, Collections.emptyMap());
		
		/* Actually there should be 8 annotations but the Cobol parser ignores EXEC SQL BEGIN/END DECLARE SECTION statements
		 * and EXEC SQL INCLUDE is changed to a COPY */
		final int numberOfExpectedAnnotations = 5;
		assertEquals(numberOfExpectedAnnotations, identifiedAnnotations.size());
		for (final var annotation : identifiedAnnotations) {
			printAnnotationInformation(annotation);
		}
	}
	
	@Test
	public void testIdentifyDatabaseAnnotationsOriginatingInCopybooks() {
		LOG.debug(() -> "testIdentifyDatabaseAnnotationsOriginatingInCopybooks");
		final AstNodePojo rootNode = createAst("identifier/database", "MMRS7113.cbl").get(null);
		final AnnotationIdentifier databaseIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.DATABASE);
		final var identifiedAnnotations = databaseIdentifier.identify(rootNode, Collections.emptyMap(), Technology.COBOL, Collections.emptyMap());
		
		/* Actually there should be 8 annotations but the Cobol parser ignores EXEC SQL BEGIN/END DECLARE SECTION statements
		 * and EXEC SQL INCLUDE is changed to a COPY */
		final int numberOfExpectedAnnotations = 5;
		assertEquals(numberOfExpectedAnnotations, identifiedAnnotations.size());
		for (final var annotation : identifiedAnnotations) {
			printAnnotationInformation(annotation);
		}
	}
	
	@Test
	public void testIdentifyDatabaseAnnotationsOriginatingInNestedCopybooks() {
		LOG.debug(() -> "testIdentifyDatabaseAnnotationsOriginatingInCopybooks");
		final AstNodePojo rootNode = createAst("identifier/database", "MMRS7114.cbl").get(null);
		final AnnotationIdentifier databaseIdentifier = AnnotationIdentifier.getAnnotationIdentifier(AnnotationType.DATABASE);
		final var identifiedAnnotations = databaseIdentifier.identify(rootNode, Collections.emptyMap(), Technology.COBOL, Collections.emptyMap());
		
		final int numberOfExpectedAnnotations = 2;
		assertEquals(numberOfExpectedAnnotations, identifiedAnnotations.size());
		for (final var annotation : identifiedAnnotations) {
			printAnnotationInformation(annotation);
		}
	}
	
	private void printAnnotationInformation(final AnnotationPojoPrototype annotation) {
		final ModuleLocation location = annotation.location.getNonNull();
		LOG.debug(() -> String.format("Module ID: %s | Offset: %d | Length: %d", annotation.module.get(), location.getOffset(), location.getLength()));
	}

}
