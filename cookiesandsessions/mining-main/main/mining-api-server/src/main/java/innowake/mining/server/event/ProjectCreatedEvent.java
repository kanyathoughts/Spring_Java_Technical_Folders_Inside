/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.event;

import java.util.Optional;

import innowake.mining.data.event.ProjectSpecificEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event signaling that a new Project was created.
 */
public class ProjectCreatedEvent extends ProjectSpecificEvent {

	public ProjectCreatedEvent(final EntityId projectId) {
		super(Optional.of(projectId));
	}

}
