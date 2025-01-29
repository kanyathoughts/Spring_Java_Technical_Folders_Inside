/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job.identification;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.data.Logging;
import innowake.mining.data.core.moduledescription.ModuleDescriptionIdentifier;
import innowake.mining.server.job.base.ModuleTask;
import innowake.mining.shared.access.EntityId;

/**
 * {@link Task} implementation that will identify the Module description for one single module.
 */
class IdentifyModuleDescriptionTask extends ModuleTask {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.DATA);

	IdentifyModuleDescriptionTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		super(progressMonitor, jobId, projectId, moduleId);
	}


	@Override
	protected void run(final EntityId moduleId) {
		if (isSourceAvailable(moduleId)) {
			try {
				ModuleDescriptionIdentifier.identify(moduleId, moduleService);
			} catch (final Exception e) {
				LOG.error(() -> "Error during module description identification for the module with Id " + moduleId, e);
				throw e;
			}
		}
	}

}
