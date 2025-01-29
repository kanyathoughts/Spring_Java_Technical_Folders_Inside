/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

/**
 * Ids for the implementations of the similarity calculations.
 */
public enum DnaSimilarityAlgorithm {

	/**
	 * Weighted Levenshtein
	 */
	WEIGHTED_LEVENSHTEIN("Weighted Levenshtein");
	
	private final String id;
	
	private DnaSimilarityAlgorithm(final String id) {
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
	public static final DnaSimilarityAlgorithm parse(final String id) {
		if (WEIGHTED_LEVENSHTEIN.id.equals(id)) {
			return WEIGHTED_LEVENSHTEIN;
		}
		throw new IllegalStateException("SimilarityId for name " + id + " is not available.");
	}
	
}
