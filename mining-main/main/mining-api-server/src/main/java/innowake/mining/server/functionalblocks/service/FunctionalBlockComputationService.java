/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.functionalblocks.computation.FunctionalBlockComputation;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Service for executing {@link FunctionalBlockComputation}.
 */
@Service
public class FunctionalBlockComputationService {

	private final FunctionalBlockService functionalBlockService;
	private final List<FunctionalBlockComputation<?>> functionalBlockComputations;

	public FunctionalBlockComputationService(final FunctionalBlockService functionalBlockService,
											 final List<FunctionalBlockComputation<?>> functionalBlockComputations) {
		this.functionalBlockService = functionalBlockService;
		this.functionalBlockComputations = functionalBlockComputations;
	}

	/**
	 * Executes all applicable {@link FunctionalBlockComputation} on the given functional blocks.
	 * @param functionalBlockIds the ids of the functional blocks on which to execute computations
	 */
	public void compute(final Collection<UUID> functionalBlockIds, final ProgressMonitor progressMonitor) {
		final List<FunctionalBlockPojo> functionalBlocks = functionalBlockService.find(q -> q.byUids(functionalBlockIds));

		for (final FunctionalBlockComputation<?> computation: functionalBlockComputations) {
			final List<FunctionalBlockPojo> filteredBlocks = functionalBlocks.stream()
					.filter(computation::accept)
					.toList();
			if ( ! filteredBlocks.isEmpty()) {
				executeComputation(filteredBlocks, computation, progressMonitor);
			}
		}
	}

	private <T> void executeComputation(final List<FunctionalBlockPojo> functionalBlocks, final FunctionalBlockComputation<T> computation,
			final ProgressMonitor progressMonitor) {
		final Map<FunctionalBlockPojo, T> results = computation.computeBatched(functionalBlocks, progressMonitor);
		computation.persistBatched(results);
		functionalBlockService.setComputedAt(functionalBlocks.stream().map(FunctionalBlockPojo::getUid).toList(), Instant.now());
	}
}
