/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import innowake.mining.data.core.SchemaConstants;
import innowake.mining.shared.entities.AnnotationPojo;

/**
 * Helper class for checking whether we can overwrite existing descriptions with GenAI generated descriptions.
 */
public class AnnotationOverwriteValidator {
	
	/**
	 * Checks whether an {@linkplain AnnotationPojo Annotations} name can be overwritten by a GenAI generated description.
	 *
	 * @param annotation the {@linkplain AnnotationPojo} we want to generate a description for
	 * @return {@code true} if:
	 * <li>the existing annotation name is empty or</li>
	 * <li>the annotation was created by the {@linkplain SchemaConstants#SYSTEM_USER SYSTEM_USER} and never updated</li>
	 * {@code false} otherwise
	 */
	public boolean shouldOverwriteAnnotationName(final AnnotationPojo annotation) {
		final String name = annotation.getName();
		final String createdByUserId = annotation.getCreatedByUserId();
		final var updatedByUserId = annotation.getUpdatedByUserId();
		return name.isBlank() || (createdByUserId.equals(SchemaConstants.SYSTEM_USER) && updatedByUserId.orElse("").isEmpty());
	}

}
