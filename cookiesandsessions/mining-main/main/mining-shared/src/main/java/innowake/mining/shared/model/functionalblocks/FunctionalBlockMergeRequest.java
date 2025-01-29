/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.functionalblocks;

import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;

/**
 * Pojo used for merge and un-merge operation for the functional block.
 */
public class FunctionalBlockMergeRequest {

	private final UUID commonParent;
	@Nullable
	private final UUID mergeParent;
	@Nullable
	private final FunctionalBlockPojoPrototype mergeParentPrototype;
	private final List<UUID> mergeChildren;
	private final Boolean removeEmptyBlocks;
	
	/**
	 * Constructor for MergeBlocks.
	 * 
	 * @param commonParent Common parent UUID for mergeParentPrototype and mergeChildren
	 * @param mergeParent UUID for the merge parent
	 * @param mergeParentPrototype Prototype for the merge parent
	 * @param mergeChildren List of UUIDs for merge children
	 * @param removeEmptyBlocks Flag indicating whether to remove empty blocks
	 */
	@JsonCreator
	public FunctionalBlockMergeRequest(@JsonProperty("commonParent") final UUID commonParent,
			@JsonProperty("mergeParent") @Nullable final UUID mergeParent,
			@JsonProperty("mergeParentPrototype") @Nullable final FunctionalBlockPojoPrototype mergeParentPrototype,
			@JsonProperty("mergeChildren") final List<UUID> mergeChildren,
			@JsonProperty("removeEmptyBlocks") final Boolean removeEmptyBlocks) {
		this.commonParent = commonParent;
		this.mergeParent = mergeParent;
		this.mergeParentPrototype = mergeParentPrototype;
		this.mergeChildren = mergeChildren;
		this.removeEmptyBlocks = removeEmptyBlocks;
	}
	
	/**
	 * @return UUID Common parent UUID for mergeParentPrototype and mergeChildrens
	 */
	public UUID getCommonParent() {
		return commonParent;
	}

	/**
	 * @return UUID for the merge parent
	 */
	@Nullable
	public UUID getMergeParent() {
		return mergeParent;
	}
	
	/**
	 * @return FunctionalBlockPojoPrototype for the merge parent
	 */
	@Nullable
	public FunctionalBlockPojoPrototype getMergeParentPrototype() {
		return mergeParentPrototype;
	}
	
	/**
	 * @return List of UUIDs for merge children
	 */
	public List<UUID> getMergeChildren() {
		return mergeChildren;
	}
	
	/**
	 * @return Flag indicating whether to remove empty blocks
	 */
	public Boolean getRemoveEmptyBlocks() {
		return removeEmptyBlocks;
	}
}
