/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.event;

import java.util.Optional;

import innowake.mining.data.event.ProjectSpecificEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event signaling that custom properties defined for a Project have been changed.
 * <p>
 * This event is fired when either the the definition of custom property classes
 * for the Project is updated, or when one of the classes is modified.
 */
public class CustomPropertiesModifiedEvent extends ProjectSpecificEvent {

	public CustomPropertiesModifiedEvent(final Optional<EntityId> projectId) {
		super(projectId);
	}

}
