/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.Task;
import innowake.mining.server.functionalblocks.service.FunctionalBlockComputationService;

import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

/**
 * Task that executes {@linkplain innowake.mining.server.functionalblocks.service.FunctionalBlockComputationService#compute(Collection)
 * functional block computation} on a single functional block.
 */
public class FunctionalBlockComputationTask extends Task<Boolean> {

	@Autowired
	private transient FunctionalBlockComputationService functionalBlockComputationService;

	/* must use ArrayList here so that the value is serializable */
	private final ArrayList<UUID> functionalBlockIds;

	public FunctionalBlockComputationTask(final ProgressMonitor progressMonitor, final String jobId, final List<UUID> functionalBlockIds) {
		super(progressMonitor, jobId);
		this.functionalBlockIds = new ArrayList<>(functionalBlockIds);
	}

	@Override
	protected Result<Boolean> run(final ProgressMonitor progressMonitor) {
		functionalBlockComputationService.compute(functionalBlockIds, progressMonitor);
		return new Result<>(Boolean.TRUE);
	}
}
