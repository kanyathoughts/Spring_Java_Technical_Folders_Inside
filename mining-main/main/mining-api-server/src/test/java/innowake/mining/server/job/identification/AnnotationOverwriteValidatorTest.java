/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import innowake.mining.data.core.SchemaConstants;
import innowake.mining.server.job.genai.GenerateAnnotationDescriptionsJob;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;

/**
 * Tests behavior of {@linkplain AnnotationOverwriteValidator} for various different {@linkplain Annotation Annotations}.
 */
class AnnotationOverwriteValidatorTest {
	
	final AnnotationOverwriteValidator validator = new AnnotationOverwriteValidator();
	
	/**
	 * Tests behavior for system identified annotation that wasn't modified. We want to overwrite these descriptions by default so we expect {@linkplain AnnotationOverwriteValidator#shouldOverwriteAnnotationName shouldOverwriteAnnotationName} to return {@code true} here.
	 */
	@Test
	void overWriteAnnotationSystemIdentified() {
		final AnnotationPojo annotation = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setName("Business Rule Candidate [System identified]"));
		assertTrue(validator.shouldOverwriteAnnotationName(annotation));
	}
	
	
	/**
	 * Tests behavior for a system identified annotation that was modified by a user. We don't want to overwrite user-written descriptions by default so we expect {@linkplain AnnotationOverwriteValidator#shouldOverwriteAnnotationName shouldOverwriteAnnotationName} to return {@code false} here.
	 */
	@Test
	void overWriteAnnotationSystemIdentifiedUserModified() {
		final AnnotationPojo annotation = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setUpdatedByUserId("admin")
				.setName("This is an awesome user modified Annotation"));
		assertFalse(validator.shouldOverwriteAnnotationName(annotation));
	}
	
	/**
	 * Tests behavior for an Annotation that was created by and modified by the system user. This would be the case for an Annotation that was created by {@linkplain IdentifyCandidatesJob} and later changed by {@linkplain GenerateAnnotationDescriptionsJob}.
	 * We don't want to overwrite existing AI-generated descriptions by default so we expect {@linkplain AnnotationOverwriteValidator#shouldOverwriteAnnotationName shouldOverwriteAnnotationName} to return {@code false} here.
	 */
	@Test
	void overWriteAnnotationSystemIdentifiedSystemModified() {
		final AnnotationPojo annotation = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setCreatedByUserId(SchemaConstants.SYSTEM_USER)
				.setUpdatedByUserId(SchemaConstants.SYSTEM_USER)
				.setName("This is an awesome system modified Annotation"));
		assertFalse(validator.shouldOverwriteAnnotationName(annotation));
	}
	
	/**
	 * Tests behavior for a user created annotation that was not modified. We don't want to overwrite user-written descriptions by default so we expect {@linkplain AnnotationOverwriteValidator#shouldOverwriteAnnotationName shouldOverwriteAnnotationName} to return {@code false} here.
	 */
	@Test
	void overWriteAnnotationUserCreated() {
		final AnnotationPojo annotation = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setCreatedByUserId("some_user")
				.setName("This is an awesome user created Annotation"));
		assertFalse(validator.shouldOverwriteAnnotationName(annotation));
	}
	
	/**
	 * Tests behavior for a user created annotation that was not modified but has an empty description. We want to overwrite empty descriptions by default so we expect {@linkplain AnnotationOverwriteValidator#shouldOverwriteAnnotationName shouldOverwriteAnnotationName} to return {@code true} here.
	 */
	@Test
	void overWriteAnnotationEmpty() {
		final AnnotationPojo annotation = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setCreatedByUserId("some_user")
				.setName(""));
		assertTrue(validator.shouldOverwriteAnnotationName(annotation));
	}

}
