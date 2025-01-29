/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.event;

import innowake.mining.shared.access.EntityId;

/**
 * Event signaling that an Annotation was created for a project.
 */
public class AnnotationCreatedEvent extends AnnotationEvent {
	
	/**
	 * Constructor for passing along {@link AnnotationCreatedEvent}.
	 *
	 * @param projectId
	 * 		The id of the project.
	 * @param annotationId
	 * 		The id of the created annotation.
	 */
	public AnnotationCreatedEvent(final EntityId projectId, final EntityId annotationId) {
		super(projectId, annotationId);
	}
}
