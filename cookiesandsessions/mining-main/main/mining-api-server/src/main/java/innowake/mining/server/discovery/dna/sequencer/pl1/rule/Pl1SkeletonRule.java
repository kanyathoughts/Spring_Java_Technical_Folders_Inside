/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer.pl1.rule;

import java.util.Optional;

import innowake.mining.server.discovery.dna.sequencer.DNACollector;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.mining.server.discovery.parser.pl1.Pl1ParseResultProvider.Pl1ParseResult;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.ndt.core.parsing.ast.AstNode;
import innowake.ndt.pl1parser.ast.model.Pl1ModelTraverser;
import innowake.ndt.pl1parser.ast.model.statement.block.BeginBlock;
import innowake.ndt.pl1parser.ast.model.statement.block.PackageBlock;
import innowake.ndt.pl1parser.ast.model.statement.block.ProcedureBlock;

public class Pl1SkeletonRule implements SequencerRule<Pl1ParseResult> {

	@Override
	public DnaSequencer getId() {
		return DnaSequencer.PLI_SKELETON_RULE;
	}

	@Override
	public void apply(final DNACollector<Pl1ParseResult> collector) throws DiscoveryException {
		final Pl1ParseResult parseResult = collector.getParseResult();
		
		final Optional<AstNode> rootNode = parseResult.getProgramModel().getRoot();
		
		if ( ! rootNode.isPresent()) {
			/* this program does not appear to be parseable */
			return;
		}
		
		new Pl1ModelTraverser() {
			
			@Override
			public boolean handle(final PackageBlock node) {
				collector.add(() -> "package", node);
				return true;
			}
			
			@Override
			public boolean handle(final ProcedureBlock node) {
				collector.add(() -> "procedure", node);
				return true;
			}
			
			@Override
			public boolean handle(final BeginBlock node) {
				collector.add(() -> "begin", node);
				return true;
			}
			
		}.traverse(rootNode.get());
	}
}
