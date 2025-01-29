/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.event;

import java.util.Optional;

import innowake.mining.data.event.ProjectSpecificEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event signaling that the SavedSearch for a project were modified.
 * <p>
 * This event is fired when SavedSearches are modified, 
 */
public class SavedSearchModifiedEvent extends ProjectSpecificEvent {
	
	public SavedSearchModifiedEvent(final EntityId projectId) {
		super(Optional.of(projectId));
	}
}
