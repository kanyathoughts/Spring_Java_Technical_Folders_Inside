/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.dna.sequencer.cobol;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.function.UnaryOperator;

import innowake.mining.server.discovery.dna.sequencer.AbstractSequencer;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.mining.server.discovery.dna.sequencer.cobol.rule.CobolMethodRule;
import innowake.mining.server.discovery.dna.sequencer.cobol.rule.CobolSkeletonRule;
import innowake.mining.server.discovery.parser.cobol.CobolParseResultProvider;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.discovery.config.ConfigResources;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.core.assembling.IAssembling;

/**
 * Applies DNA sequencer rules to a Cobol source object and extracts DNA strings.
 */
public class CobolSequencer extends AbstractSequencer<Tuple2<CobolModel, IAssembling<SourcePojo>>> {
	
	/**
	 * Create a new instance of the Cobol sequencer.
	 * 
	 * @param configProvider Function providing configuration data by configuration name.
	 * @param resultProvider Access to the CobolParseResultProvider
	 */
	public CobolSequencer(final UnaryOperator<String> configProvider, final CobolParseResultProvider resultProvider) {
		super(() -> configProvider.apply(ConfigResources.DNA_SEQUENCER_CONFIG.getResourceName()), resultProvider);
	}
	
	@Override
	protected Collection<SequencerRule<Tuple2<CobolModel, IAssembling<SourcePojo>>>> getRules() {
		return Collections.unmodifiableSet(new HashSet<>(Arrays.asList(
				new CobolSkeletonRule(),
				new CobolMethodRule()
		)));
	}
}
