/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.event;

import innowake.lib.core.lang.Nullable;
import innowake.mining.shared.access.EntityId;

import java.util.Optional;

/**
 * Base for project specific events.
 */
public abstract class ProjectSpecificEvent extends ClusteredEvent {

	@Nullable
	private final EntityId projectId;

	/**
	 * Create a project specific event.
	 * @param projectId Id of the Project or <i>null</i>.
	 */
	protected ProjectSpecificEvent(final Optional<EntityId> projectId) {
		this.projectId = projectId.orElse(null);
	}

	/**
	 * Get the Id of the Project this event is specific to.
	 * @return A Project Id.
	 */
	public Optional<EntityId> getProjectId() {
		return Optional.ofNullable(projectId);
	}

}
