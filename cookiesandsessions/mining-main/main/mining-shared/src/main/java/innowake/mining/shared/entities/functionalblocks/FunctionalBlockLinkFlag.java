/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

/**
 * Enum values to use for the keys of {@linkplain FunctionalBlockLink#getFlags() functional block link flags}.
 */
public enum FunctionalBlockLinkFlag {

	/**
	 * Indicates that the functional block is a merge block and holds the id of the previously right linked block in its value.
	 */
	MERGE_CHILD_A,
	/**
	 * Indicates that the functional block is a merge block and holds the id of the previously left linked block in its value.
	 */
	MERGE_CHILD_B,
	/**
	 * The type of the link. Value will be a set of one or more of {@link FunctionalBlockLinkType}.
	 */
	TYPE,

	/**
	 * "RA_SHARED_RESOURCE_ID" Indicates the id of the shared resource module
	 */
	RA_SHARED_RESOURCE_ID,

	/**
	 * "RA_ACCESS_TYPE" Indicates the access types of the access module
	 */
	RA_ACCESS_TYPE,

	/**
	 * "CALL_CHAIN_EDGE_RELATIONSHIP_TYPE" Indicates the relationship type of the call chain edge
	 */
	CALL_CHAIN_EDGE_RELATIONSHIP_TYPE,

	/**
	 * "CALL_CHAIN_EDGE_RELATIONSHIP_PROPERTIES" Indicates the relationship attributes of the call chain edge
	 */
	CALL_CHAIN_EDGE_RELATIONSHIP_PROPERTIES,
	GENERATED_BY
}
