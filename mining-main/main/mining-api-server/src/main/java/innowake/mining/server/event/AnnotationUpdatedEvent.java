/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.event;

import innowake.mining.shared.access.EntityId;

import java.util.Optional;

/**
 * Event signaling that an Annotation was updated for a project.
 */
public class AnnotationUpdatedEvent extends AnnotationEvent {

	/**
	 * Constructor for passing along {@link AnnotationUpdatedEvent}.
	 * @param projectId
	 * 		The id of the project.
	 */
	public AnnotationUpdatedEvent(final EntityId projectId) {
		super(projectId);
	}
	
	
	/**
	 * Constructor for passing along {@link AnnotationUpdatedEvent}.
	 * @param projectId
	 * 		The id of the project.
	 * @param annotationId
	 * 		The id of the annotation.
	 */
	public AnnotationUpdatedEvent(final EntityId projectId, final EntityId annotationId) {
		super(projectId, annotationId);
	}
}
