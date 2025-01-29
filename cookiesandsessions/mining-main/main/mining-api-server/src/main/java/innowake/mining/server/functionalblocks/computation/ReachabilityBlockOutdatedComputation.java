/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
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

import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Checks if a {@linkplain FunctionalBlockType#REACHABILITY reachability block} contains any module block that was modified
 * or went missing since the block was generated and marks the reachability block with the {@link  FunctionalBlockFlag#OUTDATED} flag.
 */
@Component
public class ReachabilityBlockOutdatedComputation implements FunctionalBlockComputation<FunctionalBlockPojoPrototype> {

	private final FunctionalBlockService functionalBlockService;

	public ReachabilityBlockOutdatedComputation(final FunctionalBlockService functionalBlockService) {
		this.functionalBlockService = functionalBlockService;
	}

	@Override
	public boolean accept(final FunctionalBlockPojo functionalBlock) {
		/* this computation runs only on top-down reachability blocks (could also run on bottom-up in the future, if needed) */
		return FunctionalBlockUtil.hasType(functionalBlock, FunctionalBlockType.RA_TOP_DOWN) &&
				! FunctionalBlockUtil.hasType(functionalBlock, FunctionalBlockType.MERGE_PARENT);
	}

	@Nullable
	@Override
	public FunctionalBlockPojoPrototype compute(final FunctionalBlockPojo functionalBlock, final ProgressMonitor progressMonitor) {
		//TODO: would be good to implement computeBatched() here, but then we would need to take the minimum if GENERATED_AT
		// in the call to findChildrenDeep() and then check the value for each block individually still
		final Map<String, Object> flags = new HashMap<>(functionalBlock.getFlags());
		flags.put(FunctionalBlockFlag.OUTDATED.name(), true);
		final Long generatedAt = (Long) flags.get(FunctionalBlockFlag.GENERATED_AT.name());

		if (generatedAt == null) {
			/* mark any block that doesn't have the GENERATED_AT flag yet as "OUTDATED" */
			return new FunctionalBlockPojoPrototype().setFlags(flags);
		}
		final List<FunctionalBlockPojo> upperBoundMissingModules = functionalBlockService.findChildrenDeep(functionalBlock.getUid(), 1,
				q -> q.withType(FunctionalBlockType.MODULE).withParent(p -> p.withType(FunctionalBlockType.RA_UPPER_BOUND))
						.withMissingSinceAfter(Instant.ofEpochMilli(generatedAt)));

		if ( ! upperBoundMissingModules.isEmpty()) {
			/*
			 * mark any block that contains modules that have gone missing since the block
			 * was generated as "OUTDATED" and when the missing module is the RA_UPPER_BOUND
			 * module then mark the reachability block (RA_TOP_DOWN) itself as "DELETED"
			 */
			flags.put(FunctionalBlockFlag.DELETED.name(), true);
			return new FunctionalBlockPojoPrototype().setFlags(flags);
		} else {
			final List<FunctionalBlockPojo> otherMissingModules = functionalBlockService.findChildrenDeep(functionalBlock.getUid(), 1,
					q -> q.withType(FunctionalBlockType.MODULE).notWithParent(p -> p.withType(FunctionalBlockType.RA_UPPER_BOUND))
							.withMissingSinceAfter(Instant.ofEpochMilli(generatedAt)));
			if ( ! otherMissingModules.isEmpty()) {
				/*
				 * mark any block that contains modules that have gone missing since the block
				 * was generated as "OUTDATED"
				 */
				return new FunctionalBlockPojoPrototype().setFlags(flags);
			}
		}

		final List<FunctionalBlockPojo> changedModules = functionalBlockService.findChildrenDeep(functionalBlock.getUid(), 1, q -> q
				.withType(FunctionalBlockType.MODULE)
				.withContentChangedOrDependencyChangedAfter(Instant.ofEpochMilli(generatedAt), Instant.ofEpochMilli(generatedAt)));

		if ( ! changedModules.isEmpty()) {
			/* mark any block that contains modules that have source code changes since the block was generated as "OUTDATED" */
			return new FunctionalBlockPojoPrototype().setFlags(flags);
		}

		return null;
	}

	@Override
	public void persist(final UUID functionalBlockId, final FunctionalBlockPojoPrototype data) {
		if (data != null && data.flags.isDefined()) {
			data.uid.set(functionalBlockId);
			functionalBlockService.update(data);
		}
	}
}
