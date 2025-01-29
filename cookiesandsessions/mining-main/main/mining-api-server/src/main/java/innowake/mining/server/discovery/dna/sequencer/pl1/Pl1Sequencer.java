/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer.pl1;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.function.UnaryOperator;

import innowake.mining.server.discovery.dna.sequencer.AbstractSequencer;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.mining.server.discovery.dna.sequencer.pl1.rule.Pl1MethodRule;
import innowake.mining.server.discovery.dna.sequencer.pl1.rule.Pl1SkeletonRule;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider.Pl1ParseResult;
import innowake.mining.shared.discovery.config.ConfigResources;

/**
 * Applies DNA sequencer rules to a PL/1 source object and extracts DNA strings.
 */
public class Pl1Sequencer extends AbstractSequencer<Pl1ParseResult> {

	/**
	 * Create a new instance of the PL/1 sequencer.
	 * 
	 * @param configProvider Access to the configuration
	 * @param resultProvider Access to the Pl1ParseResultProvider
	 */
	public Pl1Sequencer(final UnaryOperator<String> configProvider, final Pl1ParseResultProvider resultProvider) {
		super(() -> configProvider.apply(ConfigResources.DNA_SEQUENCER_CONFIG.getResourceName()), resultProvider);
	}
	
	@Override
	protected Collection<SequencerRule<Pl1ParseResult>> getRules() {
		return Collections.unmodifiableSet(new HashSet<>(Arrays.asList(
				new Pl1SkeletonRule(),
				new Pl1MethodRule()
		)));
	}

}
