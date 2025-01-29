/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.job.datalineagefunctionalblock;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.Status;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.datalineagefunctionalblock.DataLineageFunctionalBlockGeneration;
import innowake.mining.server.functionalblocks.service.FunctionalBlockComputationService;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.job.Message;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Job for generating a functional block based on data lineage.
 */
public class DataLineageFunctionalBlockJob extends Job<Serializable> {

	@Autowired
	private transient FunctionalBlockGenerationService functionalBlockGenerationService;
	@Autowired
	private transient FunctionalBlockComputationService functionalBlockComputationService;

	private final EntityId projectId;
	private final List<EntityId> ddeUids;

	public DataLineageFunctionalBlockJob(final EntityId projectId, final List<EntityId> ddeUids) {
		this.projectId = projectId;
		this.ddeUids = ddeUids;
	}

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		final FunctionalBlockGenerationContext context = new FunctionalBlockGenerationContext(projectId, getJobId(), progressMonitor);
		final Collection<Pair<FunctionalBlockGenerationResult.Operation, UUID>> generated = functionalBlockGenerationService.generate(
				DataLineageFunctionalBlockGeneration.class, context, ddeUids);
		functionalBlockComputationService.compute(generated.stream().map(Pair::getValue).collect(Collectors.toList()), progressMonitor);
		final List<String> additionalData = context.getAdditionalData();
		if ( ! additionalData.isEmpty() && additionalData.size() > 1) {
			for (final String data : additionalData) {
				writeMessage(Message.Severity.INFO, data);
			}
			return new Result<>(new Status(Severity.WARNING), "Some functional blocks could not be identified");
		}
		return new Result<>(new Status(Severity.OK));
	}
	
}