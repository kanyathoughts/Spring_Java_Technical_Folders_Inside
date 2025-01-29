/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.impl.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContextProvider;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryCore;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * This task will handle creation of missing and utility dependencies for unresolved dependencies.
 */
class UnresolvedDependencyTask extends Task<Serializable> {
	
	private final EntityId moduleId;
	private final EntityId projectId;
	private final int iteration;
	private final ModuleParameters moduleParameters;

	@Autowired
	private transient DiscoveryCore discoveryCore;

	@Autowired
	private transient DiscoveryContextProvider discoveryContextProvider;
	
	@Autowired
	private transient DiscoveryPersistenceImpl orientDiscoveryPersistence;

	@Autowired
	private transient ModuleService moduleService;

	/**
	 * Creates a new UnresolvedDependencyTask.
	 * 
	 * @param jobMonitor the progress monitor for the task
	 * @param projectId id of the project the task executes on
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by the Discovery
	 * @param moduleId id of the module on which the dependency was declared 
	 */
	public UnresolvedDependencyTask(final JobMonitor jobMonitor, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters,
			final EntityId moduleId) {
		super(jobMonitor, jobMonitor.getJobId());
		this.moduleId = moduleId;
		this.projectId = projectId;
		this.iteration = iteration;
		this.moduleParameters = moduleParameters;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		try (final DiscoveryContext context = discoveryContextProvider.createContext(projectId, getJobId(), iteration, moduleParameters, progressMonitor,
				assertNotNull(taskSpan))) {
			final var unresolvedDependencyDefinitions = orientDiscoveryPersistence.fetchUnresolvedDependencyDefinitions(moduleId);
			unresolvedDependencyDefinitions
					.forEach(unresolvedDependencyDefinition -> {
						try {
							discoveryCore.handleUnresolvedDependencies(context, moduleId, unresolvedDependencyDefinition);
						} catch (final Exception e) {
							final var errorMessage = "UnresolvedDependencyTask: Error while creating Dependency for moduleId : " + moduleId +
									" Failed " + "Dependency definition: " + unresolvedDependencyDefinition;
							final var errorMarker = new ErrorMarkerPojoPrototype()
									.setProject(projectId)
									.setModule(moduleId)
									.setKey(ErrorKey.DEPENDENCY_RESOLUTION_ERROR)
									.setSeverity(Severity.ERROR)
									.setCause(errorMessage + e);
							moduleService.createErrorMarker(errorMarker);
							throw new IllegalStateException(errorMessage, e);
						}
					});
			return new Result<>(Boolean.TRUE);
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

}
