/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Service for merging and un-merging operations on Functional Blocks.
 */
@Service
@Transactional("postgres")
public class FunctionalBlockMergeService {

	private final FunctionalBlockService functionalBlockService;

	@Autowired
	public FunctionalBlockMergeService(final FunctionalBlockService functionalBlockService) {
		this.functionalBlockService = functionalBlockService;
	}

	/**
	 * Merges the given merge children blocks into the provided merge parent. While at the process of doing that, it unmerges any merge children
	 * that are already merged. The unmerged children are then merged into the merge parent.
	 *
	 * @param commonParent the common parent of the blocks to be merged
	 * @param mergeParent the UUID of the merge parent block in case if already exists
	 * @param mergeParentPrototype the merge block prototype in which the children blocks shall be merged, in case a new one needs to be created
	 * @param mergeChildren the child blocks to merge
	 * @param removeEmptyBlocks if {@code true}, empty merge parent blocks will be removed after the merge
	 *
	 * @return UUID of mergeParent
	 */
	public UUID merge(final UUID commonParent, @Nullable final UUID mergeParent, @Nullable final FunctionalBlockPojoPrototype mergeParentPrototype,
			final Collection<UUID> mergeChildren, final boolean removeEmptyBlocks) {
		final UUID mergeParentUid;
		if (mergeParent == null && mergeParentPrototype != null) {
			if (mergeParentPrototype.flags.isDefined()) {
				@SuppressWarnings("unchecked")
				final var mergeParentTypes = (Collection<String>) mergeParentPrototype.flags.getNonNull().getOrDefault(FunctionalBlockFlag.TYPE.name(),
						new ArrayList<>());
				mergeParentTypes.add(FunctionalBlockType.MERGE_PARENT.name());
				mergeParentPrototype.flags.getNonNull().put(FunctionalBlockFlag.TYPE.name(), mergeParentTypes);
			} else {
				mergeParentPrototype.flags.set(Map.of(FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.MERGE_PARENT.name())));
			}
			mergeParentUid = functionalBlockService.create(mergeParentPrototype);
			final FunctionalBlockPojo commonParentPojo = functionalBlockService.find(commonParent).orElseThrow(
					() -> new IllegalArgumentException(String.format("Common parent block [%s] not found", commonParent)));
			final List<UUID> commonParentChildren = new ArrayList<>(commonParentPojo.getChildren());
			commonParentChildren.add(mergeParentUid);
			functionalBlockService.update(new FunctionalBlockPojoPrototype().setUid(commonParent).setChildren(commonParentChildren));
		} else {
			mergeParentUid = mergeParent;
		}

		final var blocks = functionalBlockService.find(q -> q.byUids(new ArrayList<>(mergeChildren)));
		final var mergeBlocks = blocks.parallelStream().filter(b -> isChildMergeBlock(mergeChildren, b)).collect(Collectors.toList());
		mergeBlocks.forEach(b -> {
			functionalBlockService.unmerge(commonParent, b.getUid(), b.getChildren());
			mergeChildren.addAll(b.getChildren());
			mergeChildren.remove(b.getUid());
		});

		functionalBlockService.merge(commonParent, Objects.requireNonNull(mergeParentUid), mergeChildren);
		if (removeEmptyBlocks) {
			mergeBlocks.forEach(b -> functionalBlockService.delete(b.getUid()));
		}
		return mergeParentUid;
	}

	/**
	 * Un-merges the {@code mergeChildren} Functional Blocks from {@code mergeParent} Functional Block.
	 *
	 * @param commonParent the common parent of MergeParent and MergeChild
	 * @param mergeParent the UUID of the merge Parent
	 * @param mergeChildren the merge Children block UUIDs
	 * @param removeEmptyBlocks if {@code true}, empty merge parent blocks will be removed after the un-merge
	 */
	public void unmerge(final UUID commonParent, final UUID mergeParent, final Collection<UUID> mergeChildren, final boolean removeEmptyBlocks) {
		functionalBlockService.unmerge(commonParent, mergeParent, mergeChildren);
		if (removeEmptyBlocks) {
			final FunctionalBlockPojo mergeParentFunctionalBlock = functionalBlockService.find(mergeParent).orElseThrow(
					() -> new IllegalArgumentException(String.format("Merge parent block [%s] not found", mergeParent))
			);
			if (mergeParentFunctionalBlock.getChildren().isEmpty()) {
				functionalBlockService.delete(mergeParent);
			}
			functionalBlockService.find(q -> q.byUids(new ArrayList<>(mergeChildren)).withType(FunctionalBlockType.MERGE_PARENT))
					.stream()
					.filter(child -> child.getChildren().isEmpty())
					.forEach(child -> functionalBlockService.delete(child.getUid()));
		}
	}

	private boolean isChildMergeBlock(final Collection<UUID> mergeChildren, final FunctionalBlockPojo block) {
		if (block.getFlags().containsKey(FunctionalBlockFlag.TYPE.name())) {
			final Object blockType = block.getFlags().get(FunctionalBlockFlag.TYPE.name());
			if (blockType instanceof Collection) {
				@SuppressWarnings("unchecked")
				final Collection<String> types = (Collection<String>) blockType;
				return types.contains(FunctionalBlockType.MERGE_PARENT.name()) && mergeChildren.contains(block.getUid());
			}
		}
		return false;
	}
}
