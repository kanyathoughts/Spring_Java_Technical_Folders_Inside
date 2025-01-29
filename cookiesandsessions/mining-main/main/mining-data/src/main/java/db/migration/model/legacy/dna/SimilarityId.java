/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package db.migration.model.legacy.dna;

/**
 * Ids for the implementations of the similarity calculations.
 * 
 * DO NOT CHANGE THIS FILE !!!
 * 
 * @deprecated must be used in old java based migration scripts only. Will be removed once migration from OrientDB to Postres is done.
 */
@Deprecated
public enum SimilarityId {

	/**
	 * Weighted Levenshtein
	 */
	WEIGHTED_LEVENSHTEIN("Weighted Levenshtein");
	
	private final String id;
	
	private SimilarityId(final String id) {
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
	public static final SimilarityId parse(final String id) {
		if (WEIGHTED_LEVENSHTEIN.id.equals(id)) {
			return WEIGHTED_LEVENSHTEIN;
		}
		throw new IllegalStateException("SimilarityId for name " + id + " is not available.");
	}
	
}
