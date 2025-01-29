/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer;

import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.dna.DnaSequencer;

/**
 * Base interface for all sequencer rules.
 * @param <T> the type of the parse result this sequencer operates on
 */
public interface SequencerRule<T> {

	/**
	 * Get the name of the rule used to identify the dna source algorithm.
	 * Should be a human readable format.
	 *
	 * @return The rule name.
	 */
	DnaSequencer getId();
	
	/**
	 * Extract the DNA for the given module id.
	 * Extracted DNA should be added via {@link DNACollector#add(DNAValueProvider, 
	 * innowake.ndt.core.parsing.ast.AstNode)} or the overloaded methods.
	 *
	 * @param collector The DNA collector. It also provides the parse result on which the rule operates.
	 * @throws DiscoveryException if the DNA extraction fails
	 */
	void apply(DNACollector<T> collector) throws DiscoveryException;
}
