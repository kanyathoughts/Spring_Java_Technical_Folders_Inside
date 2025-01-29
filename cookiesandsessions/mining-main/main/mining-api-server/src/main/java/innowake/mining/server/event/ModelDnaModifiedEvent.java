/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.event;

import java.util.Optional;

import innowake.mining.data.event.ProjectSpecificEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event signalling that the Model DNA for a project were modified (created/updated/deleted).
 * <p>
 * This event is fired when Model DNA are modified, 
 */
public class ModelDnaModifiedEvent extends ProjectSpecificEvent {
	
	public ModelDnaModifiedEvent(final EntityId projectId) {
		super(Optional.of(projectId));
	}

}
