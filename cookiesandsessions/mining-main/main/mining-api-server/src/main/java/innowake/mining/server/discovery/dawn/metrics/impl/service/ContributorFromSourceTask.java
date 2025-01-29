/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.service;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContextProvider;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContributorProvider;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryCore;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.server.discovery.metrics.SourceExportService;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Task that executes all applicable contributors to a certain source file.
 */
class ContributorFromSourceTask extends Task<Serializable> {

	private final EntityId projectId;
	private final int iteration;
	private final ModuleParameters moduleParameters;
	private final EntityId sourceObjectId;

	@Autowired
	private transient DiscoveryCore discoveryCore;

	@Autowired
	private transient DiscoveryContextProvider discoveryContextProvider;

	@Autowired
	private transient DiscoveryContributorProvider discoveryContributorProvider;

	@Autowired
	private transient SourceService sourceService;

	@Autowired
	private transient ModuleService moduleService;

	@Autowired
	private transient SourceExportService sourceExportService;

	/**
	 * Creates a new ContributorFromSourceTask.
	 * 
	 * @param jobMonitor the progress monitor for the task
	 * @param projectId id of the project the task executes on
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by the Discovery
	 * @param sourceObjectId id of the source object the task executes on
	 */
	public ContributorFromSourceTask(final JobMonitor jobMonitor, final EntityId projectId, final int iteration,
			final ModuleParameters moduleParameters, final EntityId sourceObjectId) {
		super(jobMonitor, jobMonitor.getJobId());
		this.projectId = projectId;
		this.iteration = iteration;
		this.moduleParameters = moduleParameters;
		this.sourceObjectId = sourceObjectId;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		try (final DiscoveryContext context = discoveryContextProvider.createContext(projectId, getJobId(), iteration, moduleParameters, progressMonitor,
				assertNotNull(taskSpan))) {
			final SourcePojo sourceObject = assertNotNull(sourceService.get(q -> q.byId(sourceObjectId)));

			/* Check if the sourceObject or all sources of the same technology must be exported */
			sourceExportService.exportSourceIfRequired(getJobId(), sourceObject);

			final List<DiscoveryContributorFromSource> contributors = discoveryContributorProvider.getContributorsForSourceObject(context, sourceObject);

			for (final DiscoveryContributorFromSource contributor : contributors) {
				try {
					final List<ContributorResult> results = discoveryCore.executeContributorOnSourceObject(contributor, context, sourceObject);
					discoveryCore.importContributorResults(context, results);
				} catch (final Exception e) {
					final var errorMessage = "ContributorFromSourceTask: Error while executing contributor " + contributor.getClass()
							.getName() + "' on source file '" + sourceObject.getPath() + "'";
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

		return new Result<>(Boolean.TRUE);
	}
}
