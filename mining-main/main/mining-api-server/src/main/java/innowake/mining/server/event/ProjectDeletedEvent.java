/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.event;

import java.util.Optional;

import innowake.mining.data.event.ProjectSpecificEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event signaling that a Project was deleted.
 */
public class ProjectDeletedEvent extends ProjectSpecificEvent {

	public ProjectDeletedEvent(final EntityId projectId) {
		super(Optional.of(projectId));
	}

}
