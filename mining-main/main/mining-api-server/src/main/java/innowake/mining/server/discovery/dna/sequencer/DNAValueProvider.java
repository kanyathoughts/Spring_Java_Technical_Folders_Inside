/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer;

/**
 * Function used to provide a dna value.
 * Requirements are
 * <li> Comma separated list. Example {@code a,b,c}
 * <li> Good balance between uniqueness and common factory for communities. If the dna is unique for all modules this will never become
 * a good community. If the dna is to general (or too short) this will result in one community containing every module.
 */
@FunctionalInterface
public interface DNAValueProvider {

	/**
	 * Get the dna string value.
	 * Format should be a comma separated list of texts representing the dna.
	 * This list is the input for the similarity comparison.
	 *
	 * @return A csv list of dna values.
	 */
	String getValue();
}
