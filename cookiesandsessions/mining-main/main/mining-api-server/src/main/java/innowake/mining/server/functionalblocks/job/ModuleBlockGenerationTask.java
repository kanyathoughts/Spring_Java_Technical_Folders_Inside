/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.ModuleBlockGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.server.job.base.ModuleTask;
import innowake.mining.shared.access.EntityId;

/**
 * Task that executes {@linkplain innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService#generate method
 * for generating a functional block}.
 */
public class ModuleBlockGenerationTask extends ModuleTask {
	
	@Autowired
	private transient FunctionalBlockGenerationService functionalBlockGenrativeService;

	/**
	 * Constructor.
	 *
	 * @param progressMonitor the progress monitor to use
	 * @param jobId the Id of the job this task belongs to
	 * @param projectId the Id of the project
	 * @param moduleId the Id of the module
	 */
	public ModuleBlockGenerationTask(final ProgressMonitor progressMonitor, final String jobId, final EntityId projectId, final EntityId moduleId) {
		super(progressMonitor, jobId, projectId, moduleId);
	}

	@Override
	protected void run(final EntityId moduleId) {
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(projectId);
		functionalBlockGenrativeService.generate(ModuleBlockGeneration.class, context, moduleId);
	}
}
