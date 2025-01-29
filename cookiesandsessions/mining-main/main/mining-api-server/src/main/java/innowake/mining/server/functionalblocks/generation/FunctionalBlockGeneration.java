/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.generation;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.functionalblocks.computation.FunctionalBlockComputation;
import innowake.mining.server.functionalblocks.service.FunctionalBlockGenerationService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * Interface for functional block generations. The generations can create new functional blocks or update or delete existing ones.
 * <p>
 * The difference between this and {@link FunctionalBlockComputation} is that computations execute on existing blocks and augment them with additional
 * data that can be derived from the block definition. The generations work in the opposite direction and derive a functional block definition from
 * additional data that is either passed to the generation function or obtained from the database.
 * <p>
 * Note that while the generations <i>can</i> delete existing blocks, it is usually not a good idea, because users may lose information that was attached
 * to these blocks. It is better to flag a block with {@link FunctionalBlockFlag#MISSING} or {@link FunctionalBlockFlag#REQUIRES_REVIEW} instead.
 *
 * @param <I> type of input data required for the computation
 * @param <O> type of additional data produced by the computation
 */
public interface FunctionalBlockGeneration<I, O> {

	/**
	 * Executes the functional block generation.
	 *
	 * @param context the generation context
	 * @param inputData input data required by the computation
	 * @return list of results (blocks that are to be created/updated/deleted)
	 */
	Collection<FunctionalBlockGenerationResult<O>> generate(FunctionalBlockGenerationContext context, @Nullable I inputData);

	/**
	 * Persists additional data that was returned from {@link #generate(FunctionalBlockGenerationContext, Object)}, attaching it to the given
	 * functional block.
	 *
	 * @param context the generation context
	 * @param functionalBlockId the id of the functional block to which the additional data shall be attached
	 * @param data the additional data to attach to the block.
	 */
	default void persistAdditionalData(final FunctionalBlockGenerationContext context, final UUID functionalBlockId, final O data) {
		/* do nothing by default */
	}

	/**
	 * Sets the {@link FunctionalBlockGenerationService} for the class.
	 * This is used to work around the circular dependency reference that would occur otherwise.
	 *
	 * @param service the service to set.
	 */
	default void setFunctionalBlockGenerationService(final FunctionalBlockGenerationService service) {}

	/**
	 * Optionally, return a "category" (any string) for a lock that should be acquired preventing parallel execution of this generation.
	 * Only generations that use the same "category" are prevented from being executed in parallel.
	 * <p>
	 * When returning anything else besides {@link Optional#empty()}, then you should also implement
	 * {@link #getModuleIdsToLock(FunctionalBlockGenerationContext, Object)} to indicate which modules should be locked from parallel execution.
	 *
	 * @param context the generation context
	 * @param inputData input data required by the computation
	 * @return a lock category, or {@link Optional#empty()} when no locking is required
	 */
	default Optional<String> getLockCategory(final FunctionalBlockGenerationContext context, @Nullable final I inputData) {
		return Optional.empty();
	}

	/**
	 * Optionally, return a list of module ids that are "locked" while this generation is in progress.
	 * Generations that lock the same module ids (and are using the same "lock category") are prevented from being executed in parallel.
	 * <p>
	 * When returning something here, then you need to also implement
	 * {@link #getLockCategory(FunctionalBlockGenerationContext, Object)} to return a "lock category".
	 * @param context the generation context
	 * @param inputData input data required by the computation
	 * @return a list of module ids, or empty list if no modules need to be locked
	 */
	default List<Long> getModuleIdsToLock(final FunctionalBlockGenerationContext context, @Nullable final I inputData) {
		return Collections.emptyList();
	}
}
