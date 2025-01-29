/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.impl.service;

import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContributorProvider;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.discovery.dawn.metrics.api.service.DiscoveryService;
import innowake.mining.server.discovery.dawn.metrics.api.service.OngoingDiscovery;
import innowake.mining.server.discovery.dawn.metrics.api.temporarystorage.DiscoveryTemporaryStorage;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.discovery.metrics.TaskHandler;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.job.Message;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class DiscoveryServiceImpl implements DiscoveryService {

	private static final String STATUS_MESSAGE_CONTRIBUTORS = "[Dawn] Executing contributor tasks";
	private static final String STATUS_MESSAGE_DEPENDENCIES = "[Dawn] Executing dependencies tasks";
	private static final String STATUS_MESSAGE_MERGE_DEPENDENCIES = "[Dawn] Executing dependency merge tasks";
	private static final String STATUS_MESSAGE_ANCHOR_CONTRIBUTOR_RESULT = "[Dawn] Executing anchor tasks";
	private static final String STATUS_MESSAGE_DEFERRED_ACTION = "[Dawn] Executing Deferred action tasks";
	private static final String STATUS_MESSAGE_CREATE_IF_MISSING_RESULT = "[Dawn] Executing create if missing default modules tasks";
	private static final String STATUS_MESSAGE_UNRESOLVED_DEPENDENCIES = "[Dawn] Executing unresolved dependencies tasks";
	private static final String FAILED_TASK_MESSAGE_FORMAT = "%s: %d tasks failed";

	private final DiscoveryContributorProvider discoveryContributorProvider;
	private final DiscoveryTemporaryStorage discoveryTemporaryStorage;
	private final DiscoveryPersistenceImpl orientDiscoveryPersistence;

	@Autowired
	public DiscoveryServiceImpl(final DiscoveryContributorProvider discoveryContributorProvider,
			final DiscoveryTemporaryStorage discoveryTemporaryStorage,
			final DiscoveryPersistenceImpl orientDiscoveryPersistence) {
		this.discoveryContributorProvider = discoveryContributorProvider;
		this.discoveryTemporaryStorage = discoveryTemporaryStorage;
		this.orientDiscoveryPersistence = orientDiscoveryPersistence;
	}

	@Override
	public OngoingDiscovery discoverMetrics(final TaskHandler taskHandler, final EntityId projectId, final ModuleParameters moduleParameters,
			final List<EntityId> sourceObjectIds) {
		return new OngoingDiscoverMetrics(this, taskHandler, projectId, moduleParameters, sourceObjectIds, discoveryTemporaryStorage);
	}

	/**
	 * Execute all contributor tasks.
	 *
	 * @param taskHandler the task handler that will be used to execute Discovery tasks
	 * @param projectId the id of the project on which Discovery is executed
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by Discovery
	 * @param sourceObjectIds ids of the source objects that Discovery should be executed on
	 */
	void executeContributorTasks(final TaskHandler taskHandler, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters,
			final List<EntityId> sourceObjectIds) {
		final List<DiscoveryContributor> noSourceContributors = discoveryContributorProvider.getNoSourceContributors();
		final int totalTaskCount = noSourceContributors.size() + sourceObjectIds.size();
		final DiscoveryResultConsumer<Serializable> resultConsumer = new DiscoveryResultConsumer<>(taskHandler.getJobMonitor(), STATUS_MESSAGE_CONTRIBUTORS);
		resultConsumer.setTotalTaskCount(totalTaskCount);

		final Iterator<DiscoveryContributor> noSourceContributorIterator = noSourceContributors.iterator();
		final TaskSource<Serializable> taskSource = new TaskSource<Serializable>() {

			@Override
			public boolean hasNextTask() {
				return noSourceContributorIterator.hasNext();
			}

			@Override
			public Task<Serializable> nextTask() {
				taskHandler.getJobMonitor().checkCanceled();
				return new ContributorTask(taskHandler.getJobMonitor(), projectId, iteration, moduleParameters, noSourceContributorIterator.next().getClass().getName());
			}
		};

		taskHandler.forkJobTasks(taskSource, resultConsumer);

		final Iterator<EntityId> sourceObjectIterator = sourceObjectIds.iterator();
		final TaskSource<Serializable> fromSourceTaskSource = new TaskSource<Serializable>() {

			@Override
			public boolean hasNextTask() {
				return sourceObjectIterator.hasNext();
			}

			@Override
			public Task<Serializable> nextTask() {
				taskHandler.getJobMonitor().checkCanceled();
				return new ContributorFromSourceTask(taskHandler.getJobMonitor(), projectId, iteration, moduleParameters, sourceObjectIterator.next());
			}
		};

		taskHandler.forkJobTasks(fromSourceTaskSource, resultConsumer);
		reportFailedTasks(taskHandler.getJobMonitor(), STATUS_MESSAGE_CONTRIBUTORS, resultConsumer.getFailedTaskCount());
	}

	/**
	 * Execute all dependencyMerge tasks, the method will fetch all the modules which has more than 1 dependency definitions with merge duplicate definition
	 * and merges the duplicate definition into a single one and also merges the reachedFromModules list.
	 *
	 * @param taskHandler the task handler that will be used to execute Discovery tasks
	 * @param projectId the id of the project on which Discovery is executed
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by Discovery
	 */
	void executeDependencyMergeTasks(final TaskHandler taskHandler, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters) {
		final List<EntityId> moduleIdsWithMergeDuplicates = orientDiscoveryPersistence.fetchModuleIdsWithMergeDuplicates(projectId);

		final DiscoveryResultConsumer<Serializable> resultConsumer = new DiscoveryResultConsumer<>(taskHandler.getJobMonitor(),
				STATUS_MESSAGE_MERGE_DEPENDENCIES);
		resultConsumer.setTotalTaskCount(moduleIdsWithMergeDuplicates.size());

		final Iterator<EntityId> iterator = moduleIdsWithMergeDuplicates.iterator();
		final TaskSource<Serializable> taskSource = new TaskSource<Serializable>() {

			@Override
			public boolean hasNextTask() {
				return iterator.hasNext();
			}

			@Override
			public Task<Serializable> nextTask() {
				taskHandler.getJobMonitor().checkCanceled();
				return new DependencyMergeTask(taskHandler.getJobMonitor(), projectId, iteration, moduleParameters, iterator.next());
			}
		};
		taskHandler.forkJobTasks(taskSource, resultConsumer);
		reportFailedTasks(taskHandler.getJobMonitor(), STATUS_MESSAGE_MERGE_DEPENDENCIES, resultConsumer.getFailedTaskCount());
	}

	/**
	 * Execute all dependencyResolution tasks.
	 *
	 * @param taskHandler the task handler that will be used to execute Discovery tasks
	 * @param projectId the id of the project on which Discovery is executed
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by Discovery
	 */
	void executeDependencyResolutionTasks(final TaskHandler taskHandler, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters) {
		final List<EntityId> modulesWithUnresolvedDependencies = orientDiscoveryPersistence.getModulesWithUnresolvedDependencies(projectId);

		final DiscoveryResultConsumer<Serializable> resultConsumer = new DiscoveryResultConsumer<>(taskHandler.getJobMonitor(), STATUS_MESSAGE_DEPENDENCIES);
		resultConsumer.setTotalTaskCount(modulesWithUnresolvedDependencies.size());

		final Iterator<EntityId> modulesWithUnresolvedDependenciesIterator = modulesWithUnresolvedDependencies.iterator();
		final TaskSource<Serializable> taskSource = new TaskSource<Serializable>() {

			@Override
			public boolean hasNextTask() {
				return modulesWithUnresolvedDependenciesIterator.hasNext();
			}

			@Override
			public Task<Serializable> nextTask() {
				taskHandler.getJobMonitor().checkCanceled();
				return new DependencyResolutionTask(taskHandler.getJobMonitor(), projectId, iteration, moduleParameters, modulesWithUnresolvedDependenciesIterator.next());
			}
		};
		taskHandler.forkJobTasks(taskSource, resultConsumer);
		reportFailedTasks(taskHandler.getJobMonitor(), STATUS_MESSAGE_DEPENDENCIES, resultConsumer.getFailedTaskCount());
	}
	
	/**
	 * Execute anchor tasks.
	 *
	 * @param taskHandler the task handler that will be used to execute Discovery tasks
	 * @param projectId the id of the project on which Discovery is executed
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by Discovery
	 */
	void executeAnchorTasks(final TaskHandler taskHandler, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters) {
		final Collection<ContributorResult> unanchoredContributorResults =
				discoveryTemporaryStorage.getAndRemoveUnanchoredContributorResults(taskHandler.getJobId());
		
		final DiscoveryResultConsumer<Serializable> resultConsumer = new DiscoveryResultConsumer<>(taskHandler.getJobMonitor(), STATUS_MESSAGE_ANCHOR_CONTRIBUTOR_RESULT);
		resultConsumer.setTotalTaskCount(unanchoredContributorResults.size());

		final Iterator<ContributorResult> unanchoredContributorResultIterator = unanchoredContributorResults.iterator();
		final TaskSource<Serializable> taskSource = new TaskSource<Serializable>() {

			@Override
			public boolean hasNextTask() {
				return unanchoredContributorResultIterator.hasNext();
			}

			@Override
			public Task<Serializable> nextTask() {
				taskHandler.getJobMonitor().checkCanceled();
				return new AnchorTask(taskHandler.getJobMonitor(), projectId, iteration, moduleParameters, unanchoredContributorResultIterator.next());
			}
		};
		taskHandler.forkJobTasks(taskSource, resultConsumer);
		reportFailedTasks(taskHandler.getJobMonitor(), STATUS_MESSAGE_ANCHOR_CONTRIBUTOR_RESULT, resultConsumer.getFailedTaskCount());
	}
	
	/**
	 * Execute deferredAction tasks.
	 *
	 * @param taskHandler the task handler that will be used to execute Discovery tasks
	 * @param projectId the id of the project on which Discovery is executed
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by Discovery
	 */
	void executeDeferredActionTasks(final TaskHandler taskHandler, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters) {
		final Collection<EntityId> moduleIdsWithDeferredActions = discoveryTemporaryStorage.getAndRemoveModulesWithDeferredActions(taskHandler.getJobId());
		
		final DiscoveryResultConsumer<Serializable> resultConsumer = new DiscoveryResultConsumer<>(taskHandler.getJobMonitor(), STATUS_MESSAGE_DEFERRED_ACTION);
		resultConsumer.setTotalTaskCount(moduleIdsWithDeferredActions.size());

		final Iterator<EntityId> moduleIdsWithDeferredActionsIterator = moduleIdsWithDeferredActions.iterator();
		final TaskSource<Serializable> taskSource = new TaskSource<>() {

			@Override
			public boolean hasNextTask() {
				return moduleIdsWithDeferredActionsIterator.hasNext();
			}

			@Override
			public Task<Serializable> nextTask() {
				taskHandler.getJobMonitor().checkCanceled();
				return new DeferredActionTask(taskHandler.getJobMonitor(), projectId, iteration, moduleParameters, moduleIdsWithDeferredActionsIterator.next());
			}
		};
		taskHandler.forkJobTasks(taskSource, resultConsumer);
		reportFailedTasks(taskHandler.getJobMonitor(), STATUS_MESSAGE_DEFERRED_ACTION, resultConsumer.getFailedTaskCount());
	}
	
	/**
	 * Execute create If missing default modules task
	 *
	 * @param taskHandler the task handler that will be used to execute Discovery tasks
	 * @param projectId the id of the project on which Discovery is executed
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by Discovery
	 */
	void executeCreateIfMissingDefaultModules(final TaskHandler taskHandler, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters) {
		final Collection<ContributorResult> allUnanchoredContributorResults = 
				discoveryTemporaryStorage.getAndRemoveUnanchoredContributorResults(taskHandler.getJobId()); 

		final List<ContributorResult> unanchoredMissedDefaultModuleCR = allUnanchoredContributorResults.stream()
				.filter(contributorResult -> contributorResult.getResolutionFlags().contains(ResolutionFlag.CREATE_IF_MISSING))
				.collect(Collectors.toList());

		final DiscoveryResultConsumer<Serializable> resultConsumer = new DiscoveryResultConsumer<>(taskHandler.getJobMonitor(), STATUS_MESSAGE_CREATE_IF_MISSING_RESULT);
		resultConsumer.setTotalTaskCount(unanchoredMissedDefaultModuleCR.size());

		final Iterator<ContributorResult> unanchoredContributorResultIterator = unanchoredMissedDefaultModuleCR.iterator();
		final TaskSource<Serializable> taskSource = new TaskSource<Serializable>() {

			@Override
			public boolean hasNextTask() {
				return unanchoredContributorResultIterator.hasNext();
			}

			@Override
			public Task<Serializable> nextTask() {
				taskHandler.getJobMonitor().checkCanceled();
				return new CreateIfMissingDefaultModulesTask(taskHandler.getJobMonitor(), projectId, iteration, moduleParameters,
						unanchoredContributorResultIterator.next());
			}
		};
		taskHandler.forkJobTasks(taskSource, resultConsumer);
		reportFailedTasks(taskHandler.getJobMonitor(), STATUS_MESSAGE_CREATE_IF_MISSING_RESULT, resultConsumer.getFailedTaskCount());
	}

	/**
	 * Execute unresolvedDependencies tasks which will create missing and utility dependencies for remaining unresolved dependencies.
	 *
	 * @param taskHandler the task handler that will be used to execute Discovery tasks
	 * @param projectId the id of the project on which Discovery is executed
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by Discovery
	 */
	public void executeUnresolvedDependenciesTask(final TaskHandler taskHandler, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters) {
		final List<EntityId> modulesWithUnresolvedDependencies = orientDiscoveryPersistence.getModulesWithUnresolvedDependencies(projectId);

		final DiscoveryResultConsumer<Serializable> resultConsumer = new DiscoveryResultConsumer<>(taskHandler.getJobMonitor(), STATUS_MESSAGE_UNRESOLVED_DEPENDENCIES);
		resultConsumer.setTotalTaskCount(modulesWithUnresolvedDependencies.size());

		final Iterator<EntityId> modulesWithUnresolvedDependenciesIterator = modulesWithUnresolvedDependencies.iterator();
		final TaskSource<Serializable> taskSource = new TaskSource<Serializable>() {

			@Override
			public boolean hasNextTask() {
				return modulesWithUnresolvedDependenciesIterator.hasNext();
			}

			@Override
			public Task<Serializable> nextTask() {
				taskHandler.getJobMonitor().checkCanceled();
				return new UnresolvedDependencyTask(taskHandler.getJobMonitor(), projectId, iteration, moduleParameters,
						modulesWithUnresolvedDependenciesIterator.next());
			}
		};
		taskHandler.forkJobTasks(taskSource, resultConsumer);
		reportFailedTasks(taskHandler.getJobMonitor(), STATUS_MESSAGE_UNRESOLVED_DEPENDENCIES, resultConsumer.getFailedTaskCount());
	}

	private void reportFailedTasks(final JobMonitor jobMonitor, final String message, final int errorCount) {
		if (errorCount > 0) {
			jobMonitor.addMessage(new Message(Message.Severity.ERROR, String.format(FAILED_TASK_MESSAGE_FORMAT, message, errorCount)));
		}
	}
}
