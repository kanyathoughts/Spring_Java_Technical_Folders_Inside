/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.impl.service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContextProvider;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryCore;
import innowake.mining.server.discovery.dawn.metrics.api.model.DeferredActionDefinition;
import innowake.mining.server.discovery.dawn.metrics.api.temporarystorage.DiscoveryTemporaryStorage;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.model.ModuleParameters;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.Serializable;
import java.util.Collection;

import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Task that executes deferred actions for the module
 */
class DeferredActionTask extends Task<Serializable> {

	private static final Logger LOG = LoggerFactory.getLogger(DeferredActionTask.class);

	private final EntityId projectId;
	private final EntityId moduleId;
	private final int iteration;
	private final ModuleParameters moduleParameters;

	@Autowired
	private transient DiscoveryCore discoveryCore;

	@Autowired
	private transient DiscoveryContextProvider discoveryContextProvider;
	
	@Autowired
	private transient DiscoveryTemporaryStorage discoveryTemporaryStorage;

	@Autowired
	private transient ModuleService moduleService;

	/**
	 * Creates a new DeferredActionTask.
	 * 
	 * @param jobMonitor the progress monitor for the task
	 * @param projectId id of the project the task executes on
	 * @param iteration The incremented iteration before executing the DEPENDENCIES phase, used to reset the cache
	 * @param moduleParameters additional common attributes that need to be set on Modules created or modified by the Discovery
	 * @param moduleId id of the module
	 */
	public DeferredActionTask(final JobMonitor jobMonitor, final EntityId projectId, final int iteration, final ModuleParameters moduleParameters,
			final EntityId moduleId) {
		super(jobMonitor, jobMonitor.getJobId());
		this.projectId = projectId;
		this.iteration = iteration;
		this.moduleId = moduleId;
		this.moduleParameters = moduleParameters;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		try (final DiscoveryContext context = discoveryContextProvider.createContext(projectId, getJobId(), iteration, moduleParameters, progressMonitor,
				assertNotNull(taskSpan))) {
			final Collection<DeferredActionDefinition> deferredActionDefinitions = discoveryTemporaryStorage.getAndRemoveDeferredActions(context.getJobId(), moduleId);

			deferredActionDefinitions.forEach(deferredActionDefinition  -> {
				try {
					discoveryCore.executeDeferredAction(context, moduleId, deferredActionDefinition);
				} catch (final Exception e) {
					final var errorMessage = "DeferredActionTask: Error while executing deferredAction for moduleId : " + moduleId + " Failed Deferred Action:"
							+ deferredActionDefinition;
					moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
							.setProject(projectId)
							.setModule(moduleId)
							.setKey(ErrorKey.MODULE_ABORT)
							.setSeverity(Severity.ERROR)
							.setCause(errorMessage + e));
					LOG.debug(() -> ExceptionUtils.getFullStackTrace(e));
					throw new IllegalStateException(errorMessage, e);
				}
			});
			return new Result<>(Boolean.TRUE);
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

}
