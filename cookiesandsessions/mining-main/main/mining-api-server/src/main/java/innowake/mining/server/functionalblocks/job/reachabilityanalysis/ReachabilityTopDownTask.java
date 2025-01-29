/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job.reachabilityanalysis;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.reachabilityanalysis.ReachabilityTopDownGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.server.job.base.TaskSummary;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.functionalblocks.ReachabilityAnalysisConfig;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.UUID;

/**
 * Task that executes {@linkplain FunctionalBlockGenerationService#generate method
 * for generating a functional block}.
 */
public class ReachabilityTopDownTask extends Task<ReachabilityTaskResult> {

	private static final Logger LOG = LoggerFactory.getLogger(ReachabilityTopDownTask.class);
	@Autowired
	private transient FunctionalBlockGenerationService functionalBlockGenerationService;
	private final EntityId projectId;
	private final EntityId moduleId;
	private final TaskSummary taskSummary;
	private final ReachabilityAnalysisConfig config;

	/**
	 * Constructor.
	 *
	 * @param progressMonitor the progress monitor to use
	 * @param jobId           the Id of the job this task belongs to
	 * @param projectId       the Id of the project
	 * @param moduleId        the Id of the module
	 */
	public ReachabilityTopDownTask(final ProgressMonitor progressMonitor, final String jobId,
			final EntityId projectId, final EntityId moduleId, final ReachabilityAnalysisConfig config) {
		super(progressMonitor, jobId);
		this.taskSummary = new TaskSummary(moduleId);
		this.projectId = projectId;
		this.moduleId = moduleId;
		this.config = config;
	}

	@Override
	protected Result<ReachabilityTaskResult> run(final ProgressMonitor progressMonitor) {
		final ArrayList<UUID> functionalBlockUids = new ArrayList<>();
		progressMonitor.checkCanceled();
		try {
			final var context = new FunctionalBlockGenerationContext(projectId, getJobId(), getProgressMonitor(), config);
			functionalBlockUids.addAll(functionalBlockGenerationService.generate(ReachabilityTopDownGeneration.class, context, moduleId).stream()
					.filter(op -> ! op.getKey().equals(FunctionalBlockGenerationResult.Operation.DELETE)).map(Pair::getValue).toList());
		} catch (final Exception ex) {
			LOG.error("Reachability Top-Down Task for module " + moduleId + " has failed", ex);
			taskSummary.error(moduleId);
			taskSummary.setHadUnhandledException(true);
		}
		return new Result<>(new ReachabilityTaskResult(functionalBlockUids, taskSummary));
	}
}
