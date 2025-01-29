/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.dna.sequencer.c;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.function.UnaryOperator;

import innowake.mining.server.discovery.parser.c.CAntlrAstModelParseResultProvider;

import innowake.mining.server.discovery.dna.sequencer.AbstractSequencer;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.mining.server.discovery.dna.sequencer.c.rule.CMethodRule;
import innowake.mining.server.discovery.dna.sequencer.c.rule.CSkeletonRule;
import innowake.mining.shared.discovery.config.ConfigResources;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Applies DNA sequencer rules to a C source object and extracts DNA strings.
 */
public class CSequencer extends AbstractSequencer<AstModel> {

	/**
	 * Create a new instance of the C sequencer.
	 * @param configProvider Function providing configuration data by configuration name.
	 * @param parserProvider Access to the CAntlrAstModelParseResultProvider
	 */
	public CSequencer(final UnaryOperator<String> configProvider, final CAntlrAstModelParseResultProvider parserProvider) {
		super(() -> configProvider.apply(ConfigResources.DNA_SEQUENCER_CONFIG.getResourceName()), parserProvider);
	}

	@Override
	protected Collection<SequencerRule<AstModel>> getRules() {
		return Collections.unmodifiableSet(new HashSet<>(Arrays.asList(
				new CMethodRule(),
				new CSkeletonRule()
		)));
	}
}
