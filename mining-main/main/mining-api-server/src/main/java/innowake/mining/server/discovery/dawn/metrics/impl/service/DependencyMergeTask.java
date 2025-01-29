/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.impl.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;

import innowake.mining.shared.entities.ResolutionFlag;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContextProvider;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryCore;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Task that executes merging of duplicate dependency definition with resolution flag {@link innowake.mining.shared.entities.ResolutionFlag#MERGE_DUPLICATES}.
 */
class DependencyMergeTask extends Task<Serializable> {

	private final EntityId moduleId;
	private final EntityId projectId;
	private final int iteration;
	private final ModuleParameters moduleParameters;

	@Autowired
	private transient DiscoveryCore discoveryCore;

	@Autowired
	private transient DiscoveryContextProvider discoveryContextProvider;

	@Autowired
	private transient ModuleService moduleService;

	/**
	 * Creates a new DependencyMergeTask.
	 *
	 * @param jobMonitor the progress monitor for the task
	 * @param projectId id of the project the task executes on
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by the Discovery
	 * @param moduleId id of the module on which the dependency was declared
	 */
	public DependencyMergeTask(final JobMonitor jobMonitor, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters,
			final EntityId moduleId) {
		super(jobMonitor, jobMonitor.getJobId());
		this.moduleId = moduleId;
		this.projectId = projectId;
		this.iteration = iteration;
		this.moduleParameters = moduleParameters;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		try (final var context = discoveryContextProvider.createContext(projectId, getJobId(), iteration, moduleParameters, progressMonitor,
				assertNotNull(taskSpan))) {
			final var definitions =
					moduleService.findDependencyDefinitions(q -> q.ofModule(moduleId).withResolved(false).withFlag(ResolutionFlag.MERGE_DUPLICATES));
			discoveryCore.mergeDependencies(context, moduleId, definitions);
			return new Result<>(Boolean.TRUE);
		} catch (final Exception e) {
			final var errorMessage = "DependencyResolutionTask: Error while merging Dependencies for moduleId : '" + moduleId;
			final var errorMarker = new ErrorMarkerPojoPrototype()
					.setProject(projectId)
					.setModule(moduleId)
					.setKey(ErrorKey.DEPENDENCY_RESOLUTION_ERROR)
					.setSeverity(Severity.ERROR)
					.setCause(errorMessage + e);
			moduleService.createErrorMarker(errorMarker);
			throw new IllegalStateException(e);
		}
	}
}
