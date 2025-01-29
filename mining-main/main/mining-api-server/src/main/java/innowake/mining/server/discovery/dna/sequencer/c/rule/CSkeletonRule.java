/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dna.sequencer.c.rule;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import innowake.mining.server.discovery.dna.sequencer.DNACollector;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.model.Technology;
import innowake.ndt.core.parsing.ast.AstModel;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.core.parsing.ast.visitor.Visitor;

/**
 * The {@link SequencerRule} for the language {@link Technology#C} that captures all the statements into a DNA string.
 */
public class CSkeletonRule implements SequencerRule<AstModel> {

	@Override
	public DnaSequencer getId() {
		return DnaSequencer.C_SKELETON_RULE;
	}

	@Override
	public void apply(final DNACollector<AstModel> collector) throws DiscoveryException {
		final Optional<AstNode> node = collector.getParseResult().getRoot();
		if (node.isPresent()) {
			final Visitor<AstNode> visitor = new CSkeletonRuleVisitor(collector);
			visitor.visit(node.get().getAstNode());
		}

	}

	private static class CSkeletonRuleVisitor extends CDNAVisitor {

		private static final Map<String, String> stringMap;

		static {
			final String[][] stringMapArray = new String[][] {
					{
							"CIncludeDirective", "include"
					}, {
							"CFunctionDefinition", "function_definition"
					}, {
							"CFunctionReference", "function_reference"
					}, {
							"CGoToStatement", "goto"
					}, {
							"CLibraryFunction", "function_library"
					}
			};
			stringMap = Stream.of(stringMapArray)
					.collect(Collectors.collectingAndThen(Collectors.toMap(data -> data[0], data -> data[1]), Collections::<String, String> unmodifiableMap));
		}

		private CSkeletonRuleVisitor(final DNACollector<AstModel> collector) {
			super(collector);
		}

		@Override
		protected Map<String, String> getStringMap() {
			return stringMap;
		}

	}

}
