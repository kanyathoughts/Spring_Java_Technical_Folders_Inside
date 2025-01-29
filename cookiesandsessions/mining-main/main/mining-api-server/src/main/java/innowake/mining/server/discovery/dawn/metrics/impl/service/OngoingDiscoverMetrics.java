/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.service;

import java.util.List;

import org.apache.commons.lang.NotImplementedException;

import innowake.mining.server.discovery.dawn.metrics.api.service.OngoingDiscovery;
import innowake.mining.server.discovery.dawn.metrics.api.temporarystorage.DiscoveryTemporaryStorage;
import innowake.mining.server.discovery.metrics.TaskHandler;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.ModuleParameters;

/**
 * Represents an ongoing Discover Metrics operation.
 * <p>
 * The "cycles" are as follows:
 * <ol>
 *     <li>In the first cycle contributors are executed and Modules are created, but no dependency resolution is done.</li>
 *     <li>In the second cycle "anchoring" tasks and dependency resolution are processed. This establishes links between Modules.</li>
 *     <li>The remaining cycles execute deferred actions and may create additional Modules and dependencies. There can be any number of additional cycles.</li>
 * </ol>
 */
class OngoingDiscoverMetrics implements OngoingDiscovery {

	private enum Cycle {
		/**
		 * Initial phase before anything has been run. Next step is to run the contributors.
		 */
		CONTRIBUTORS,
		/**
		 * Contributors have been run, but no anchoring, dependency resolution or deferred actions. Next step is anchoring and dependency resolution.
		 */
		DEPENDENCIES,
		/**
		 * Anchoring and dependency resolution has been run. Next step is running the deferred actions.
		 */
		DEFERRED_ACTIONS,
		/**
		 * Creates modules if no matching targets found in dependency resolution as defined in anchoring with 
		 * {@link ResolutionFlag#CREATE_IF_MISSING} 
		 */
		CREATE_DEFAULT_MODULES_IF_MISSING,
		/**
		 * All known dependencies are resolved. Next step is to handle all the unresolved dependencies and create missing/utility dependencies.
		 */
		UNRESOLVED_DEPENDENCIES,
		/**
		 * All the cycles are completed, discovery is completed.
		 */
		FINISHED
	}

	private final DiscoveryServiceImpl discoveryService;
	private final TaskHandler taskHandler;
	private final EntityId projectId;
	private final ModuleParameters moduleParameters;
	private final List<EntityId> sourceObjectIds;
	private final DiscoveryTemporaryStorage discoveryTemporaryStorage;

	private Cycle cycle = Cycle.CONTRIBUTORS;
	private int iteration = 0;

	/**
	 * Creates a new ongoing Discover Metrics process.
	 *
	 * @param discoveryService the {@link DiscoveryServiceImpl} that is used to execute the actual tasks
	 * @param taskHandler the task handler that will be used to execute Discovery tasks
	 * @param projectId the id of the project on which Discovery is executed
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by Discovery
	 * @param sourceObjectIds ids of the source objects that Discovery should be executed on
	 * @param discoveryTemporaryStorage the discoveryTemporaryStorage
	 */
	public OngoingDiscoverMetrics(final DiscoveryServiceImpl discoveryService, final TaskHandler taskHandler, final EntityId projectId,
			final ModuleParameters moduleParameters, final List<EntityId> sourceObjectIds, final DiscoveryTemporaryStorage discoveryTemporaryStorage) {
		this.discoveryService = discoveryService;
		this.taskHandler = taskHandler;
		this.projectId = projectId;
		this.moduleParameters = moduleParameters;
		this.sourceObjectIds = sourceObjectIds;
		this.discoveryTemporaryStorage = discoveryTemporaryStorage;
	}

	@Override
	public boolean hasNextCycle() {
		switch (cycle) {
			case CONTRIBUTORS:
			case DEPENDENCIES:
			case CREATE_DEFAULT_MODULES_IF_MISSING:
			case UNRESOLVED_DEPENDENCIES:
				return true;
			case DEFERRED_ACTIONS:
				return hasMoreDeferredActions();
			default:
				return false;
		}
	}

	@Override
	public void executeNextCycle() {
		switch (cycle) {
			case CONTRIBUTORS:
				discoveryService.executeContributorTasks(taskHandler, projectId, iteration, moduleParameters, sourceObjectIds);
				cycle = Cycle.DEPENDENCIES;
				break;
			case DEPENDENCIES:
				/* ensures ContainsModule references are fully persisted before anchoring and dependency resolution (OrientDB-only) */
				iteration++;
				discoveryService.executeAnchorTasks(taskHandler, projectId, iteration, moduleParameters);
				if (hasMoreDeferredActions()) {
					discoveryService.executeDependencyMergeTasks(taskHandler, projectId, iteration, moduleParameters);
					discoveryService.executeDependencyResolutionTasks(taskHandler, projectId, iteration, moduleParameters);
					/* this is only required for OrientDiscoveryPersistence - i.e. OrientDB-specific workaround */
					cycle = Cycle.DEFERRED_ACTIONS;
				} else {
					/* If we don't have deferred actions then we can directly create missing external modules and resolve dependencies after creating them */
					cycle = Cycle.CREATE_DEFAULT_MODULES_IF_MISSING;
					/* When there is no deferred action in the current contributor we never execute discoveryService.executeReferenceImportTasks()
					 * and when legacy is on transitive phase the required dependencies is not available for it, Ex: PlContributor tests WMIN3793A 
					 * Hence invoked executeNextCycle() below to execute discoveryService.executeReferenceImportTasks() before the transitive phase of any
					 * legacy contributor */
					executeNextCycle();
				}
				break;
			case DEFERRED_ACTIONS:
				discoveryService.executeDeferredActionTasks(taskHandler, projectId, iteration, moduleParameters);
				cycle = Cycle.DEPENDENCIES;
				break;
			case CREATE_DEFAULT_MODULES_IF_MISSING:
				discoveryService.executeCreateIfMissingDefaultModules(taskHandler, projectId, iteration, moduleParameters);
				iteration++;
				discoveryService.executeDependencyMergeTasks(taskHandler, projectId, iteration, moduleParameters);
				discoveryService.executeDependencyResolutionTasks(taskHandler, projectId, iteration, moduleParameters);
				cycle = Cycle.UNRESOLVED_DEPENDENCIES;
				break;
			case UNRESOLVED_DEPENDENCIES:
				discoveryService.executeUnresolvedDependenciesTask(taskHandler, projectId, iteration, moduleParameters);
				cycle = Cycle.FINISHED;
				break;
			default:
				throw new NotImplementedException("not yet implemented");
		}
	}

	private boolean hasMoreDeferredActions() {
		return discoveryTemporaryStorage.hasModulesWithDeferredActions(taskHandler.getJobId());
	}
}
