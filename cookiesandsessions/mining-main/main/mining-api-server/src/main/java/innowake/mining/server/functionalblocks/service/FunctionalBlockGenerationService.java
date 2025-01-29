/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGeneration;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult.Operation;
import innowake.mining.server.locking.ModuleLockService;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * Service for executing {@linkplain FunctionalBlockGeneration}.
 */
@Service
public class FunctionalBlockGenerationService {

	private final FunctionalBlockGenerationPersistenceService persistenceService;
	private final List<FunctionalBlockGeneration<?, ?>> functionalBlockGenerations;
	private final ModuleLockService moduleLockService;

	public FunctionalBlockGenerationService(final FunctionalBlockGenerationPersistenceService persistenceService,
											final List<FunctionalBlockGeneration<?, ?>> functionalBlockGenerations,
											final ModuleLockService moduleLockService) {
		this.persistenceService = persistenceService;
		this.functionalBlockGenerations = functionalBlockGenerations;
		this.moduleLockService = moduleLockService;
		for (final FunctionalBlockGeneration<?, ?> functionalBlockGeneration : functionalBlockGenerations) {
			functionalBlockGeneration.setFunctionalBlockGenerationService(this);
		}
	}

	/**
	 * Executes the given functional block generation using the provided context and input data. Returns a list of the affected functional blocks.
	 *
	 * @param generationClass the class implementing the generation to execute
	 * @param context the context for the generation
	 * @param inputData optional input data required by the generation class
	 * @return list of affected functional blocks and the operation performed on them
	 * @param <I> type of the input data required by the generation class
	 * @param <O> type of additional data produced by the generation class
	 */
	public <I, O> Collection<Pair<Operation, UUID>> generate(final Class<? extends FunctionalBlockGeneration<I, O>> generationClass,
			final FunctionalBlockGenerationContext context, @Nullable final I inputData) {

		final FunctionalBlockGeneration<I, O> functionalBlockGeneration = functionalBlockGenerations.stream()
				.filter(generationClass::isInstance)
				.map(generationClass::cast)
				.findAny()
				.orElseThrow(() -> new IllegalArgumentException("Functional Block generation " + generationClass.getSimpleName() + " does not exist"));

		final Optional<String> lockCategory = functionalBlockGeneration.getLockCategory(context, inputData);
		if (lockCategory.isPresent()) {
			final List<Long> moduleIdsToLock = new ArrayList<>(functionalBlockGeneration.getModuleIdsToLock(context, inputData));
			try (final ModuleLockService.Lock lock = moduleLockService.tryLock(lockCategory.get(), context.getProjectId().getNid(), moduleIdsToLock)) {
				/* execute with lock */
				return executeGeneration(functionalBlockGeneration, context, inputData);
			}
		} else {
			/* execute without lock */
			return executeGeneration(functionalBlockGeneration, context, inputData);
		}
	}

	private <I, O> Collection<Pair<Operation, UUID>> executeGeneration(final FunctionalBlockGeneration<I, O> functionalBlockGeneration,
			final FunctionalBlockGenerationContext context, @Nullable final I inputData) {

		final Collection<FunctionalBlockGenerationResult<O>> results = functionalBlockGeneration.generate(context, inputData);

		return persistenceService.persist(context, functionalBlockGeneration, results);
	}
}
