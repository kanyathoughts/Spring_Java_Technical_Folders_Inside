/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dna.sequencer.natural.rule;

import java.util.Map;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.parsing.AstNode;
import innowake.lib.parsing.util.visitor.Visitor;
import innowake.mining.server.discovery.dna.sequencer.DNACollector;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider.NaturalParseResult;

/**
 * {@link Visitor} to capture the DNA strings
 */
abstract class NaturalDNAVisitor implements Visitor {
	
	final DNACollector<NaturalParseResult> collector;

	NaturalDNAVisitor(final DNACollector<NaturalParseResult> collector) {
		this.collector = collector;
	}

	protected abstract Map<String, String> getStringMap();

	@Override
	public boolean visit(@Nullable final Object o) {
		if ( ! (o instanceof AstNode)) {
			return true;
		}
		final AstNode node = (AstNode) o;
		String classSimpleName = node.getClass().getSimpleName();
		if (classSimpleName.startsWith("$")) {
			classSimpleName = classSimpleName.substring(1);
		}
		final String dnaStringElement = getStringMap().get(classSimpleName);
		if (dnaStringElement != null) {
			collector.add(() -> dnaStringElement, node);
		}
		return true;
	}

}
