/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.event;

import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.event.ProjectSpecificEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event signaling that the Annotations for a project were modified (created/updated/deleted).
 * <p>
 * This event is fired when Annotations and AnnotationCategories are modified, 
 * but is <strong>not</strong> fired when Annotation state is changed.
 */
public class AnnotationEvent extends ProjectSpecificEvent {
	
	@Nullable
	private final EntityId annotationId;
	
	public AnnotationEvent(final EntityId projectId) {
		super(Optional.of(projectId));
		this.annotationId = null;
	}
	
	/**
	 * Constructor for passing along {@link AnnotationEvent}.
	 * @param projectId The id of the project
	 * @param annotationId Is present if only one annotation was changed, null otherwise.
	 */
	public AnnotationEvent(final EntityId projectId, EntityId annotationId) {
		super(Optional.of(projectId));
		this.annotationId = annotationId;
	}

	/**
	 * Get the Id of the Annotation this event is specific to.
	 * @return An Annotation Id.
	 */
	public Optional<EntityId> getAnnotationId() {
		return Optional.ofNullable(annotationId);
	}
}
