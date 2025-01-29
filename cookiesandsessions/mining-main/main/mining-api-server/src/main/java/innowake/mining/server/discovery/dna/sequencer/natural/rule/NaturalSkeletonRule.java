/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dna.sequencer.natural.rule;

import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.lib.parsing.util.visitor.TopDown;
import innowake.lib.parsing.util.visitor.Visitor;
import innowake.mining.server.discovery.dna.sequencer.DNACollector;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider.NaturalParseResult;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.model.Technology;
import innowake.ndt.naturalparser.ast.ProgramObject;

/**
 * The {@link SequencerRule} for the language {@link Technology#NATURAL} that captures the statements which makes the skeleton of the program into a DNA string.
 */
public class NaturalSkeletonRule implements SequencerRule<NaturalParseResult> {

	@Override
	public DnaSequencer getId() {
		return DnaSequencer.NATURAL_SKELETON_RULE;
	}

	@Override
	public void apply(final DNACollector<NaturalParseResult> collector) throws DiscoveryException {
		final ProgramObject programObject = collector.getParseResult().getHeavyweightModel().getProgramObject();

		final Visitor visitor = new TopDown(new NaturalSkeletonRuleVisitor(collector));
		visitor.visit(programObject);
	}

	private static class NaturalSkeletonRuleVisitor extends NaturalDNAVisitor {

		private static final Map<String, String> stringMap;

		static {
			final String[][] stringMapArray = new String[][] {

					{
							"StopStmt", "stop"
					}, {
							"TerminateStmt", "terminate"
					}, {
							"PerformStmt", "perform"
					}, {
							"DefineSubroutineStmt", "define_subroutine"
					}
			};
			stringMap = Stream.of(stringMapArray)
					.collect(Collectors.collectingAndThen(Collectors.toMap(data -> data[0], data -> data[1]), Collections::<String, String> unmodifiableMap));
		}

		private NaturalSkeletonRuleVisitor(final DNACollector<NaturalParseResult> collector) {
			super(collector);
		}

		@Override
		protected Map<String, String> getStringMap() {
			return stringMap;
		}

	}

}
