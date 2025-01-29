/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.event;

import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.event.ProjectSpecificEvent;
import innowake.mining.shared.access.EntityId;

/**
 * Event signaling that the Modules for a project were modified (created/updated/deleted).
 * <p>
 * This event is fired when Modules are modified, 
 */
public class ModulesModifiedEvent extends ProjectSpecificEvent {
	
	@Nullable
	private final EntityId moduleId;
	
	/**
	 * Constructor for passing along ModulesModifiedEvent
	 * @param projectId The id of the project
	 * @param moduleId Is present if only one module was changed, Optional.empty() otherwise
	 */
	public ModulesModifiedEvent(final EntityId projectId, final Optional<EntityId> moduleId) {
		super(Optional.of(projectId));
		this.moduleId = moduleId.orElse(null);
	}

	/**
	 * Get the Id of the Module this event is specific to.
	 * @return Module Id.
	 */
	public Optional<EntityId> getModuleId() {
		return Optional.ofNullable(moduleId);
	}
}
