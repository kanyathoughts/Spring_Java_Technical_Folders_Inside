/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Checks if a Merge reachability block contains any FunctionalBlock that was modified
 */
@Component
public class ReachabilityBlockOutdatedComputationForMergeBlocks implements FunctionalBlockComputation<FunctionalBlockPojoPrototype> {

	private final FunctionalBlockService functionalBlockService;

	public ReachabilityBlockOutdatedComputationForMergeBlocks(final FunctionalBlockService functionalBlockService) {
		this.functionalBlockService = functionalBlockService;
	}

	@Override
	public boolean accept(final FunctionalBlockPojo functionalBlock) {
		return FunctionalBlockUtil.hasType(functionalBlock, List.of(FunctionalBlockType.REACHABILITY,
				FunctionalBlockType.MERGE_PARENT, FunctionalBlockType.RA_TOP_DOWN));
	}

	@Nullable
	@Override
	public FunctionalBlockPojoPrototype compute(final FunctionalBlockPojo functionalBlock, final ProgressMonitor progressMonitor) {
		final Map<String, Object> flags = new HashMap<>(functionalBlock.getFlags());

		final boolean childOutdated =
				functionalBlockService.find(q -> q.byUids(functionalBlock.getChildren())
						.withFlag(FunctionalBlockFlag.OUTDATED, true).notWithFlag(FunctionalBlockFlag.DELETED, true)).isEmpty();
		flags.put(FunctionalBlockFlag.OUTDATED.name(), ! childOutdated);
		final List<FunctionalBlockPojo> mergeBlockDeletedChildren =
				functionalBlockService.find(q -> q.byUids(functionalBlock.getChildren()).withFlag(FunctionalBlockFlag.DELETED, true));
		if (mergeBlockDeletedChildren.size() == functionalBlock.getChildren().size()) {
			flags.put(FunctionalBlockFlag.DELETED.name(), true);
			flags.put(FunctionalBlockFlag.OUTDATED.name(), true);
		} else {
			flags.put(FunctionalBlockFlag.DELETED.name(), false);
		}
		return new FunctionalBlockPojoPrototype().setFlags(flags);
	}

	@Override
	public void persist(final UUID functionalBlockId, final FunctionalBlockPojoPrototype data) {
		if (data != null && data.flags.isDefined()) {
			data.uid.set(functionalBlockId);
			functionalBlockService.update(data);
		}
	}
}
