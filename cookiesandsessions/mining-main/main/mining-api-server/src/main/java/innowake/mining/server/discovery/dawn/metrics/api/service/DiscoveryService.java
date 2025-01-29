/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.api.service;

import innowake.mining.server.discovery.metrics.TaskHandler;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.ModuleParameters;

import java.util.List;

/**
 * Main discovery entry point, providing high-level functions for executing Discovery.
 */
public interface DiscoveryService {

	/**
	 * Prepare for running Discover Metrics tasks. No actual tasks are run until calling {@link OngoingDiscovery#executeNextCycle()}
	 * on the returned object.
	 *
	 * @param taskHandler the task handler that will be used to execute Discovery tasks
	 * @param projectId the id of the project on which Discovery is executed
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by Discovery
	 * @param sourceObjectIds ids of the source objects that Discovery should be executed on
	 * @return an object representing the ongoing Discovery operation and allows interacting with it
	 */
	OngoingDiscovery discoverMetrics(TaskHandler taskHandler, EntityId projectId, ModuleParameters moduleParameters, List<EntityId> sourceObjectIds);
}
