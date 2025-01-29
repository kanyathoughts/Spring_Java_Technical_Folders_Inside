/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job.reachabilityanalysis;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.ReachabilityNetworkGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockComputationService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

public class ReachabilityResourceNetworkJob extends Job<Serializable> {

	@Autowired
	private transient FunctionalBlockService functionalBlockService;
	@Autowired
	private transient FunctionalBlockComputationService functionalBlockComputationService;
	@Autowired
	private transient FunctionalBlockGenerationService functionalBlockGenerationService;

	private final EntityId projectId;

	public ReachabilityResourceNetworkJob(final EntityId projectId) {
		this.projectId = projectId;
	}


	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(projectId, getJobId(), progressMonitor);
		functionalBlockGenerationService.generate(ReachabilityNetworkGeneration.class, context, null);

		final Optional<FunctionalBlockPojo> networkBlock = functionalBlockService.find(q -> {
			q.ofProject(projectId);
			q.withType(FunctionalBlockType.REACHABILITY_NETWORK);
		}).stream().findAny();

		if (networkBlock.isEmpty()) {
			throw new IllegalStateException("Generation of REACHABILITY_NETWORK block failed");
		}

		functionalBlockComputationService.compute(List.of(networkBlock.get().getUid()), progressMonitor);

		return new Result<>(new Status(Severity.OK));
	}
}
