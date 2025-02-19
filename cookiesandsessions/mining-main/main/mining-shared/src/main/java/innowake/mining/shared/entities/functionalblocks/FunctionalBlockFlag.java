/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

/**
 * Enum values to use for the keys of {@linkplain FunctionalBlockPojo#getFlags() functional block flags}.
 */
public enum FunctionalBlockFlag {
	/**
	 * The type of the functional block. Value will be a set of one or more of {@link FunctionalBlockType}.
	 */
	TYPE,

	/**
	 * Indicates that the functional block was generated by a FunctionalBlockGeneration and holds the id of the generation in its value.
	 */
	GENERATED_BY,
	/**
	 * Contains a timestamp when the block was generated.
	 */
	GENERATED_AT,
	/**
	 * Contains a timestamp when functional block computation was last executed on the block. The timestamp is updated only after _all_ computations are done.
	 */
	COMPUTED_AT,
	/**
	 * Indicates that the functional block can not be modified by the user. Note that this is just a hint for the UI to display the block as read-only.
	 * The block can still be updated through the REST API.
	 */
	READ_ONLY,
	/**
	 * Indicates that the data stored in the functional block is outdated and the block should be regenerated or manually updated.
	 */
	OUTDATED,

	/**
	 * The type the resource is accessed inside the "Reachability Analysis" functional block.
	 */
	RA_ACCESS_TYPE,
	/**
	 * The state of the FunctionalBlock.
	 */
	STATUS,
	/**
	 * Flag holding a list of {@link ExcludedBranch}. Defines branches to be excluded from control flow graphs computed for blocks of type
	 * {@link FunctionalBlockType#FUNCTIONAL_GROUP}.
	 */
	FB_EXCLUDED_BRANCHES,

	/**
	 * Flag indicating that this block is to be considered the "root node" of a network (meaningful only if the block is linked to other blocks).
	 */
	ENTRY_POINT,
	/**
	 * Flag indicating that this block is to be considered a terminal node of a network (meaningful only if the block is linked to other blocks).
	 */
	HALT_POINT,

	/**
	 * Set this flag to softly delete the functional block
	 */
	DELETED,

	/**
	 * Flag indicating the Peer count of the Functional block
	 */
	PEER_COUNT,

	/**
	 * Flag indicating the functional group to which the block belongs.
	 * It is specifically used for grouping when the block type is
	 * either {@link FunctionalBlockType#FUNCTIONAL_CONDITION} or {@link FunctionalBlockType#FUNCTIONAL_STATEMENT}.
	 */
	FUNCTIONAL_GROUP,

	/**
	 * Flag indicating that the Control Flow Graph of the block has been Generated.
	 * It is specifically used for the functional block of type {@link FunctionalBlockType#FUNCTIONAL_GROUP}.
	 */
	HAS_CFG
}
