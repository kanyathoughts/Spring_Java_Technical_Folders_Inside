/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer.cobol.rule;

import innowake.mining.server.discovery.dna.sequencer.DNACollector;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.server.discovery.dna.sequencer.SequencerRule;
import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.cobol.parser.ast.model.CobolSection;
import innowake.ndt.cobol.parser.ast.statement.CobolGoToStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolLabelStmt;
import innowake.ndt.cobol.parser.ast.statement.CobolPerformStmt;
import innowake.ndt.core.assembling.IAssembling;

public class CobolSkeletonRule implements SequencerRule<Tuple2<CobolModel, IAssembling<SourcePojo>>> {

	@Override
	public DnaSequencer getId() {
		return DnaSequencer.COBOL_SKELETON_RULE;
	}
	
	@Override
	public void apply(final DNACollector<Tuple2<CobolModel, IAssembling<SourcePojo>>> collector) {
		new CobolSkeletonTraverser(collector).visit(collector.getParseResult().e1); 
	}
	
	class CobolSkeletonTraverser extends CustomCobolModelTraverser {
		
		public CobolSkeletonTraverser(final DNACollector<Tuple2<CobolModel, IAssembling<SourcePojo>>> model) {
			super(model);
		}

		@Override
		protected void handleCobolLabelStmt(final @Nullable CobolLabelStmt label) {
			collector.add(() -> "l", label);
			super.handleCobolLabelStmt(label);
		}
		
		@Override
		protected void handleCobolSection(final @Nullable CobolSection section) {
			collector.add(() -> "s", section);
			super.handleCobolSection(section);
		}
		
		@Override
		protected void handleCobolPerformStmt(final @Nullable CobolPerformStmt perform) {
			collector.add(() -> "p", perform);
			super.handleCobolPerformStmt(perform);
		}
		
		@Override
		protected void handleCobolGoToStmt(final @Nullable CobolGoToStmt gotoStmt) {
			collector.add(() -> "g", gotoStmt);
			super.handleCobolGoToStmt(gotoStmt);
		}
		
	}

}
