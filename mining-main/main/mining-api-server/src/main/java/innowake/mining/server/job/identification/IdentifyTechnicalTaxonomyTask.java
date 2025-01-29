/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job.identification;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.job.base.ModuleTask;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.shared.access.EntityId;

/**
 * {@link Task} implementation that will identify the Technical Taxonomies for one single module.
 */
class IdentifyTechnicalTaxonomyTask extends ModuleTask {
	
	@Autowired
	private transient ExecutorService executorService;

	IdentifyTechnicalTaxonomyTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		super(progressMonitor, jobId, projectId, moduleId);
	}

	@Override
	protected void run(final EntityId moduleId) {
		if (isSourceAvailable(moduleId)) {
			executorService.executeTechnicalTaxonomyIdentification(moduleId);
		}
	}

}
