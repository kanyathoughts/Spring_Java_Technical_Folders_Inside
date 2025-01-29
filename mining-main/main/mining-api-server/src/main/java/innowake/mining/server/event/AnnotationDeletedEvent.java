/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.event;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;

import java.util.Optional;

/**
 * Event signaling that an Annotation was deleted for a project.
 */
public class AnnotationDeletedEvent extends AnnotationEvent {

	@Nullable
	private final EntityId moduleId;
	
	public AnnotationDeletedEvent(final EntityId projectId) {
		super(projectId);
		moduleId = null;
	}
	
	/**
	 * Constructor for notifying observers that an annotation has been deleted.
	 *
	 * @param projectId
	 * 		The id of the project.
	 * @param annotationId
	 * 		The id of the annotation. 
	 */
	public AnnotationDeletedEvent(final EntityId projectId, final EntityId annotationId) {
		super(projectId, annotationId);
		moduleId = null;
	}
	
	/**
	 * Constructor for notifying observers that an annotation has been deleted.
	 *
	 * @param projectId
	 * 		The id of the project.
	 * @param moduleId 
	 * 		The id of the module.
	 * @param annotationId
	 * 		The id of the annotation. 
	 */
	public AnnotationDeletedEvent(final EntityId projectId, final EntityId moduleId, final EntityId annotationId) {
		super(projectId, annotationId);
		this.moduleId = moduleId;
	}
	
	/**
	 * Returns the optional Id of the module that contained the deleted annotation.
	 *
	 * @return The optional id of the deleted annotation..
	 */
	public Optional<EntityId> getModuleId() {
		return Optional.ofNullable(moduleId);
	}
}
