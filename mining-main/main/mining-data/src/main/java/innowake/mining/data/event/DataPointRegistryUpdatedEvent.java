/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.event;

import java.util.Optional;

import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.shared.access.EntityId;

/**
 * Event indicating that the DataPointRegistry must be queried for changes.
 */
public class DataPointRegistryUpdatedEvent extends ProjectSpecificEvent {

	private final transient DataPointRegistry registry;

	/**
	 * @param registry The registry the event originated from.
	 * @param projectId The affected project, if the event is project specific.
	 */
	public DataPointRegistryUpdatedEvent(final DataPointRegistry registry, final Optional<EntityId> projectId) {
		super(projectId);
		this.registry = registry;
	}

	/**
	 * @return The registry the event originated from.
	 */
	public DataPointRegistry getRegistry() {
		return registry;
	}

	@Override
	public boolean isToBePublishedToCluster() {
		return false;
	}

}
