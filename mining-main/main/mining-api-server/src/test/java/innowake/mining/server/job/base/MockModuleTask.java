/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.base;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;

public class MockModuleTask extends ModuleTask {
	
	/**
	 * A mock task for the mock job to complete.
	 * @param progressMonitor the progress monitor.
	 * @param jobId the id of the parent job.
	 * @param projectId the id of the project being processed.
	 * @param moduleId the id of the module being processed.
	 */
	public MockModuleTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		super(progressMonitor, jobId, projectId, moduleId);
	}

	@Override
	protected void run(final EntityId moduleId) {
		ModulePojo module = moduleService.getModule(moduleId);
		final int value = Integer.parseInt(module.getName());
		
		if (value % 3 == 0) {
			super.getTaskSummary().unsupported(moduleId);
			return;
		}
		
		if (value % 5 == 0) {
			super.getTaskSummary().error(moduleId);
			super.getTaskSummary().setHadUnhandledException(true);
			return;
		}
		
		super.getTaskSummary().success(moduleId);
	}
}