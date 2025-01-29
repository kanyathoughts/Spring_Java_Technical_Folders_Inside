/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContextProvider;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryCore;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Task that executes a single "no source" contributor.
 */
class ContributorTask extends Task<Serializable> {

	private final EntityId projectId;
	private final int iteration;
	private final ModuleParameters moduleParameters;
	private final String contributorClassName;

	@Autowired
	private transient DiscoveryCore discoveryCore;

	@Autowired
	private transient DiscoveryContextProvider discoveryContextProvider;

	@Autowired
	private transient ApplicationContext applicationContext;

	@Autowired
	private transient ModuleService moduleService;

	/**
	 * Creates a new ContributorTask.
	 * 
	 * @param jobMonitor the progress monitor for the task
	 * @param projectId id of the project the task executes on
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by the Discovery
	 * @param contributorClassName the fully-qualified class name of the contributor to execute. The class must implement the {@link DiscoveryContributor} interface.
	 */
	public ContributorTask(final JobMonitor jobMonitor, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters, final String contributorClassName) {
		super(jobMonitor, jobMonitor.getJobId());
		this.projectId = projectId;
		this.iteration = iteration;
		this.moduleParameters = moduleParameters;
		this.contributorClassName = contributorClassName;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		final DiscoveryContributor contributor;
		try {
			contributor = applicationContext.getBean(Class.forName(contributorClassName).asSubclass(DiscoveryContributor.class));
		} catch (final ClassNotFoundException e) {
			final var errorMessage = "ContributorTask: Unable to execute contributor: Contributor class " + contributorClassName + " not found.";
			addErrorMarker(e, errorMessage);
			throw new IllegalStateException(errorMessage, e);
		}

		try (final DiscoveryContext context = discoveryContextProvider.createContext(projectId, getJobId(), iteration, moduleParameters, progressMonitor,
				assertNotNull(taskSpan))) {
			final List<ContributorResult> results = discoveryCore.executeContributor(contributor, context);
			discoveryCore.importContributorResults(context, results);

			return new Result<>(Boolean.TRUE);
		} catch (final Exception e) {
			final var errorMessage = "ContributorTask: Error while executing contributor '" + contributorClassName + "'";
			addErrorMarker(e, errorMessage);
			throw new IllegalStateException(errorMessage, e);
		}
	}

	private void addErrorMarker(final Exception e, final String errorMessage) {
		final var errorMarker = new ErrorMarkerPojoPrototype()
				.setProject(projectId)
				.setKey(ErrorKey.MODULE_ABORT)
				.setSeverity(Severity.ERROR)
				.setCause(errorMessage + e);
		moduleService.createErrorMarker(errorMarker);
	}
}
