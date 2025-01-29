/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.job.identification;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.job.base.GenericProgramModulesTask;
import innowake.mining.server.service.ExecutorService;
import innowake.mining.shared.access.EntityId;


/**
 * {@link Task} implementation that will identify the candidates for one single module.
 */
class IdentifyCandidatesTask extends GenericProgramModulesTask {
	
	@Autowired
	private transient ExecutorService executorService;

	private final boolean identifyOnlyDDE;
	
	/**
	 * Constructor.
	 *
	 * @param progressMonitor the progress monitor to use
	 * @param jobId the Id of the job this task belongs to
	 * @param projectId the Id of the project
	 * @param moduleId the Id of the module
	 * @param identifyOnlyDDE boolean to run identification of DDE only and skip annotation identification
	 */
	IdentifyCandidatesTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId,
			final boolean identifyOnlyDDE) {
		super(progressMonitor, jobId, projectId, moduleId);
		this.identifyOnlyDDE = identifyOnlyDDE;
	}

	@Override
	protected void run(final EntityId moduleId) {
		if (isSourceAvailable(moduleId)) {
			executorService.setParsingSummarizer(getTaskSummary());
			executorService.executeCandidateIdentification(moduleId, getJobId(), identifyOnlyDDE);
		}
	}
}
