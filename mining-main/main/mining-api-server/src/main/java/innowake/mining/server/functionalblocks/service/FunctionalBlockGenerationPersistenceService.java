/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import innowake.mining.server.functionalblocks.generation.FunctionalBlockGeneration;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult.Operation;
import innowake.mining.shared.access.FunctionalBlockService;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

/**
 * Service for persisting the result of{@linkplain FunctionalBlockGeneration}.
 * <p>
 * Used by {@link FunctionalBlockGenerationService} - separate service so that we can make it transactional.
 */
@Service
public class FunctionalBlockGenerationPersistenceService {

	private final FunctionalBlockService functionalBlockService;

	public FunctionalBlockGenerationPersistenceService(final FunctionalBlockService functionalBlockService) {
		this.functionalBlockService = functionalBlockService;
	}

	@Transactional("postgres")
	public <I, O> Collection<Pair<Operation, UUID>> persist(final FunctionalBlockGenerationContext context,
			final FunctionalBlockGeneration<I, O> functionalBlockGeneration, final Collection<FunctionalBlockGenerationResult<O>> results) {

		final List<Pair<Operation, UUID>> performedOperations = new ArrayList<>();
		for (final FunctionalBlockGenerationResult<O> result : results) {
			switch (result.getOperation()) {
				case CREATE: {
					final UUID functionalBlockId = functionalBlockService.create(result.getFunctionalBlock());
					result.getAdditionalData().ifPresent(data -> functionalBlockGeneration.persistAdditionalData(context, functionalBlockId, data));
					performedOperations.add(Pair.of(Operation.CREATE, functionalBlockId));
					break;
				}
				case UPDATE: {
					final UUID functionalBlockId = result.getFunctionalBlock().uid.getNonNull();
					functionalBlockService.update(result.getFunctionalBlock());
					result.getAdditionalData().ifPresent(data -> functionalBlockGeneration.persistAdditionalData(context, functionalBlockId, data));
					performedOperations.add(Pair.of(Operation.UPDATE, functionalBlockId));
					break;
				}
				case DELETE: {
					final UUID functionalBlockId = result.getFunctionalBlock().uid.getNonNull();
					functionalBlockService.delete(functionalBlockId);
					performedOperations.add(Pair.of(Operation.DELETE, functionalBlockId));
					break;
				}
				default:
					throw new IllegalArgumentException("Functional Block Generation Operation " + result.getOperation() + " not supported");
			}
		}

		return performedOperations;
	}
}
