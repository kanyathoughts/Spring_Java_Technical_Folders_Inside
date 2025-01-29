/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Enum for types of functional blocks.
 */
@MiningDataType(name = "FunctionalBlockType")
public enum FunctionalBlockType {
	/**
	 * Functional block corresponds to one Module.
	 */
	MODULE,
	/**
	 * Functional block represents a structural unit within the source code, e.g. a method, function or a paragraph.
	 */
	STRUCTURAL,
	/**
	 * Functional block represents a single piece of code that implements some business or technical function.
	 */
	FUNCTIONAL_UNIT,
	/**
	 * Functional block is a collection of functional units and describes a higher-level business function.
	 */
	FUNCTIONAL_GROUP,
	/**
	 * Functional block represents a condition associated with a {@link #FUNCTIONAL_UNIT} or {@link #FUNCTIONAL_GROUP}.
	 */
	FUNCTIONAL_CONDITION,
	/**
	 * Functional block represents a Statement associated with a {@link #FUNCTIONAL_UNIT}
	 */
	FUNCTIONAL_STATEMENT,

	REACHABILITY,
	RA_BOTTOM_UP,
	RA_TOP_DOWN,
	RA_LOWER_BOUND,
	RA_UPPER_BOUND,
	RA_ACCESS_MODULE,
	CALL_CHAIN,

	REACHABILITY_NETWORK,
	/**
	 * Functional block is a collection of functional blocks merged inside it.
	 */
	MERGE_PARENT
}
