/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.functionalblocks.generation.ModuleBlockGeneration;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * For blocks that were generated from a Module, updates it's deleted flag to true if the Module went missing.
 * This computation is executed last to ensure that the deleted flag is set after missingSince is set in the previous computation.
 */
@Component
@Order
public class DeletedModuleBlockComputation implements FunctionalBlockComputation<FunctionalBlockPojoPrototype> {

    private final FunctionalBlockService functionalBlockService;

    public DeletedModuleBlockComputation(final FunctionalBlockService functionalBlockService) {
        this.functionalBlockService = functionalBlockService;
    }

    @Override
    public boolean accept(final FunctionalBlockPojo functionalBlock) {
        if (ModuleBlockGeneration.MODULE_BLOCK_GENERATION_ID.equals(functionalBlock.getFlags().get(FunctionalBlockFlag.GENERATED_BY.name()))) {
            final Long generatedAt = (Long) functionalBlock.getFlags().get(FunctionalBlockFlag.GENERATED_AT.name());
            final Optional<GeneratedFrom> generatedFrom = functionalBlockService.getGeneratedFrom(functionalBlock.getUid());
            return generatedFrom.isPresent() && generatedFrom.get().getMissingSince().isPresent()
                    && generatedFrom.get().getMissingSince().get().isAfter(Instant.ofEpochMilli(generatedAt));
        } else {
            return false;
        }
    }

    @Override
    public Map<FunctionalBlockPojo, FunctionalBlockPojoPrototype> computeBatched(final List<FunctionalBlockPojo> functionalBlocks, final ProgressMonitor progressMonitor) {
        Map<FunctionalBlockPojo, FunctionalBlockPojoPrototype> result = new HashMap<>();
        if ( ! functionalBlocks.isEmpty()) {
            return functionalBlocks.stream().collect(Collectors.toMap(
                    Function.identity(),
                    block -> {
                        final FunctionalBlockPojoPrototype prototype = new FunctionalBlockPojoPrototype();
                        prototype.flags.set(new HashMap<>(block.getFlags()));
                        prototype.flags.getNonNull().put(FunctionalBlockFlag.DELETED.name(), true);
                        return prototype;
                    }
            ));
        }
        return result;
    }

    @Override
    public void persist(final UUID functionalBlockId, final FunctionalBlockPojoPrototype data) {
        if (data != null && data.flags.isDefined()) {
            data.uid.set(functionalBlockId);
            functionalBlockService.update(data);
        }
    }

}
