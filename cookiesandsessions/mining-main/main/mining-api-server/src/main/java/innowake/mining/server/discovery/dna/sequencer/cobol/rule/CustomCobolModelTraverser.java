/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna.sequencer.cobol.rule;

import innowake.mining.server.discovery.dna.sequencer.DNACollector;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.cobol.parser.ast.CobolModelTraverser;
import innowake.ndt.cobol.parser.ast.model.CobolModel;
import innowake.ndt.core.assembling.IAssembling;

abstract class CustomCobolModelTraverser extends CobolModelTraverser {
	
	protected DNACollector<Tuple2<CobolModel, IAssembling<SourcePojo>>> collector;

	public CustomCobolModelTraverser(final DNACollector<Tuple2<CobolModel, IAssembling<SourcePojo>>> collector) {
		this.collector = collector;
	}

	public void visit(final DNACollector<Tuple2<CobolModel, IAssembling<SourcePojo>>> collector) {
		this.collector = collector;
		super.visit(collector.getParseResult().e1);
	}
}
