/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.generation;


import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

/**
 * Generates a single block with type {@link FunctionalBlockType#REACHABILITY_NETWORK} or update the existing block if present. The block
 * has all {@link FunctionalBlockType#RA_TOP_DOWN} blocks as children as is intended to hold the links computed by {@code ReachabilityResourceNetworkComputation}.
 */
@Component
public class ReachabilityNetworkGeneration implements FunctionalBlockGeneration<Void, Void> {

	private final FunctionalBlockService functionalBlockService;

	public ReachabilityNetworkGeneration(final FunctionalBlockService functionalBlockService) {
		this.functionalBlockService = functionalBlockService;
	}

	@Override
	public Collection<FunctionalBlockGenerationResult<Void>> generate(final FunctionalBlockGenerationContext context, @Nullable final Void inputData) {
		final List<UUID> children = new ArrayList<>();
		/* add merged blocks */
		children.addAll(functionalBlockService.find(q -> {
			q.ofProject(context.getProjectId());
			q.withType(FunctionalBlockType.RA_TOP_DOWN);
			q.withType(FunctionalBlockType.MERGE_PARENT);
		}).stream().map(FunctionalBlockPojo::getUid).toList());
		/* add blocks that are NOT inside a merged block */
		children.addAll(functionalBlockService.find(q -> {
			q.ofProject(context.getProjectId());
			q.withType(FunctionalBlockType.RA_TOP_DOWN);
			q.notWithParent(p -> p.byUids(children));
		}).stream().map(FunctionalBlockPojo::getUid).toList());

		final Optional<FunctionalBlockPojo> existingNetworkBlock = functionalBlockService.find(q -> {
			q.ofProject(context.getProjectId());
			q.withType(FunctionalBlockType.REACHABILITY_NETWORK);
		}).stream().findAny();

		final FunctionalBlockPojoPrototype networkBlockPrototype = new FunctionalBlockPojoPrototype();
		if (existingNetworkBlock.isPresent()) {
			networkBlockPrototype.setUid(existingNetworkBlock.get().getUid());
			networkBlockPrototype.setChildren(children);
			/* update the GENERATED_AT timestamp - leave other flags as-is */
			networkBlockPrototype.setFlags(Map.of(FunctionalBlockFlag.GENERATED_AT.name(), Instant.now().toEpochMilli()));
			return List.of(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.UPDATE, networkBlockPrototype));
		} else {
			networkBlockPrototype.setProject(context.getProjectId());
			networkBlockPrototype.setName("Reachability Network");
			networkBlockPrototype.setDescription("");
			networkBlockPrototype.setFlags(Map.of(
					FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.REACHABILITY_NETWORK.name()),
					FunctionalBlockFlag.GENERATED_BY.name(), ReachabilityNetworkGeneration.class.getSimpleName(),
					FunctionalBlockFlag.GENERATED_AT.name(), Instant.now().toEpochMilli()
			));
			networkBlockPrototype.setChildren(children);
			return List.of(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.CREATE, networkBlockPrototype));
		}
	}
}
