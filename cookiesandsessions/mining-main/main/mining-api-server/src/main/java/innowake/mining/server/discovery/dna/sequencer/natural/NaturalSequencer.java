/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.dna.sequencer.natural;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.function.UnaryOperator;

import innowake.mining.server.discovery.dna.sequencer.AbstractSequencer;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.mining.server.discovery.dna.sequencer.natural.rule.NaturalMethodRule;
import innowake.mining.server.discovery.dna.sequencer.natural.rule.NaturalSkeletonRule;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider.NaturalParseResult;
import innowake.mining.shared.discovery.config.ConfigResources;

/**
 * Applies DNA sequencer rules to a Natural source object and extracts DNA strings.
 */
public class NaturalSequencer extends AbstractSequencer<NaturalParseResult> {

	/**
	 * Create a new instance of the Natural sequencer.
	 *
	 * @param configProvider Access to the configuration
	 * @param resultProvider Access to the NaturalParseResultProvider
	 */
	public NaturalSequencer(final UnaryOperator<String> configProvider, final NaturalParseResultProvider resultProvider) {
		super(() -> configProvider.apply(ConfigResources.DNA_SEQUENCER_CONFIG.getResourceName()), resultProvider);
	}

	@Override
	protected Collection<SequencerRule<NaturalParseResult>> getRules() {
		return Collections.unmodifiableSet(new HashSet<>(Arrays.asList(
				new NaturalMethodRule(),
				new NaturalSkeletonRule()
		)));
	}
}
