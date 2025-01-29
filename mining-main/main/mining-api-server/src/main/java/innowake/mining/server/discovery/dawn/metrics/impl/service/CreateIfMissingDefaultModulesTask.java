/* Copyright (c) 2023 Deloitte. All rights reserved. */
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
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Task that executes creation of default modules if missing and updates it if found. It is executed on the collected result from the anchoring 
 * with {@link ResolutionFlag#CREATE_IF_MISSING} 
 */
class CreateIfMissingDefaultModulesTask extends Task<Serializable> {

	private final ContributorResult contributorResult;
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
	 * Creates a new Create If missing default modules task
	 * 
	 * @param jobMonitor the progress monitor for the task
	 * @param projectId id of the project the task executes on
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by the Discovery
	 * @param contributorResult the unanchored contributor result
	 */
	public CreateIfMissingDefaultModulesTask(final JobMonitor jobMonitor, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters, final ContributorResult contributorResult) {
		super(jobMonitor, jobMonitor.getJobId());
		this.projectId = projectId;
		this.iteration = iteration;
		this.moduleParameters = moduleParameters;
		this.contributorResult = contributorResult;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		try (final DiscoveryContext context = discoveryContextProvider.createContext(projectId, getJobId(), iteration, moduleParameters, progressMonitor,
				assertNotNull(taskSpan))) {
			discoveryCore.createIfMissingDefaultModules(context, contributorResult);
			return new Result<>(Boolean.TRUE);
		} catch (final Exception e) {
			final var errorMessage = "Error occurred while executing createIfMissing task with module filter: " + contributorResult.getModuleFilter();
			final var errorMarker = new ErrorMarkerPojoPrototype()
					.setProject(projectId)
					.setKey(ErrorKey.MODULE_ABORT)
					.setSeverity(Severity.ERROR)
					.setCause(errorMessage + e);
			moduleService.createErrorMarker(errorMarker);
			throw new IllegalStateException(errorMessage, e);
		}
	}

}
