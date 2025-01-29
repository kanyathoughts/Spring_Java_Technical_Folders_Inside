/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package db.migration.model.legacy.dna;

/**
 * Constants class for similarity ids.
 * 
 * DO NOT CHANGE THIS FILE !!!
 * 
 * @deprecated must be used in old java based migration scripts only. Will be removed once migration from OrientDB to Postres is done.
 */
@Deprecated
public enum SequencerId {
	/**
	 * Name for the Cobol skeleton rule.
	 */
	COBOL_SKELETON_RULE("COBOL Skeleton"),

	/**
	 * Name for the Cobol method rule.
	 */
	COBOL_METHOD_RULE("COBOL Methods"),

	/**
	 * Name for the PLI skeleton rule.
	 */
	PLI_SKELETON_RULE("PL/I Skeleton"),

	/**
	 * Name for the PLI method rule.
	 */
	PLI_METHOD_RULE("PL/I Methods"),

	/**
	 * Name for the NATURAL skeleton rule.
	 */
	NATURAL_SKELETON_RULE("NATURAL Skeleton"),

	/**
	 * Name for the NATURAL method rule.
	 */
	NATURAL_METHOD_RULE("NATURAL Methods"),

	/**
	 * Name for the C skeleton rule.
	 */
	C_SKELETON_RULE("C Skeleton"),

	/**
	 * Name for the C method rule.
	 */
	C_METHOD_RULE("C Methods");


	private final String id;

	private SequencerId(final String id) {
		this.id = id;
	}

	/**
	 * Get the rule id.
	 *
	 * @return the rule id.
	 */
	public String getId() {
		return id;
	}

	/**
	 * Parse the enum from a given string.
	 *
	 * @param id The id to parse
	 * @return The enum
	 * @throws IllegalStateException If the id does not match a enum.
	 */
	public static final SequencerId parse(final String id) {
		for (final SequencerId value : values()) {
			if (value.getId().equals(id)) {
				return value;
			}
		}
		throw new IllegalStateException("RuleId for name " + id + " is not available.");
	}

}
