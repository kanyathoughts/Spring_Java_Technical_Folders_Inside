/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGeneration;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import java.util.*;


/**
 * Interface for functional block computations. The computations compute additional information based on a functional block definition.
 * Computations should be written in a way where they can be re-run on the same block any number of times and refresh the computed data.
 * <p>
 * Implementations must implement either {@link #compute(FunctionalBlockPojo)} or {@link #computeBatched(List)}
 * and either {@link #persist(UUID, Object)} or {@link #persistBatched(Map)}. You can mix batched and non-batched, e.g. you can implement
 * {@link #persist(UUID, Object)} even though you are implementing {@link #computeBatched(List)}.
 * <p>
 * The difference between this and {@link FunctionalBlockGeneration} is that generations create
 * new blocks or update the definitions of existing blocks using additional data (e.g. derived from other mining entities). The computations work
 * in the opposite direction and take the definition of an existing block and then derive additional data from it, that gets attached to the block.
 *
 * @param <T> the data type produced by the computation
 */
public interface FunctionalBlockComputation<T> {

	/**
	 * Returns whether the computation is applicable to the given functional block.
	 * @param functionalBlock the functional block
	 * @return {@code true} if this computation should be executed on the block or {@code false} otherwise
	 */
	boolean accept(FunctionalBlockPojo functionalBlock);

	/**
	 * Run the computation on the given functional block and return the result.
	 * @param functionalBlock the functional block
	 * @return the result of the computation
	 */
	@Nullable
	default T compute(final FunctionalBlockPojo functionalBlock, final ProgressMonitor progressMonitor) {
		/* default implementation does nothing */
		return null;
	}

	default Map<FunctionalBlockPojo, T> computeBatched(final List<FunctionalBlockPojo> functionalBlocks, final ProgressMonitor progressMonitor) {
		/* default implementation calls compute() on each element */
		final Map<FunctionalBlockPojo, T> result = new HashMap<>();
		for (final FunctionalBlockPojo block : functionalBlocks) {
			result.put(block, this.compute(block, progressMonitor));
		}
		return result;

	}

	/**
	 * Persists the result of the computation to the database.
	 * @param functionalBlockId the functional block
	 * @param data the data to persist
	 */
	default void persist(final UUID functionalBlockId, @Nullable final T data) {
		/* default implementation does nothing */
	}

	default void persistBatched(final Map<FunctionalBlockPojo, T> map) {
		/* default implementation calls persist() on (id, data) pair */
		for (final var entry : map.entrySet()) {
			persist(entry.getKey().getUid(), entry.getValue());
		}
	}
}
