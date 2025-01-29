/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dna.similarity;

/**
 * Weighted Levenshtein cost for insertion, deletion and substitution.
 */
public interface WeightedLevenshtein {

	/**
	 * Specify the deletion cost for a given entry.
	 *
	 * @param s The dna element.
	 * @return A value lower 1 means less important, 1 is default, value > 1 boost this dna importance.
	 */
	double deletionCost(String s);

	/**
	 * Specify the insertion cost for a given entry.
	 *
	 * @param s The dna element.
	 * @return A value lower 1 means less important, 1 is default, value > 1 boost this dna importance.
	 */
	double insertionCost(String s);

	/**
	 * Specify the substitution costs for two dna elements.
	 *
	 * @param s1 Left dna element
	 * @param s2 Right dna element
	 * @return A value lower 1 means less important, 1 is default, value > 1 boost this dna importance.
	 */
	double substitutionCost(String s1, String s2);

}
