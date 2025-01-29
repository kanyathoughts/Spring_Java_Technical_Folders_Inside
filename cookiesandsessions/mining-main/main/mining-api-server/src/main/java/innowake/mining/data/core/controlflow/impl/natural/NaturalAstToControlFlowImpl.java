/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractAstToControlFlowImpl;
import innowake.mining.data.core.controlflow.impl.ControlFlowResult;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Traverses AST nodes and calculates control flow for Natural programs
 */
public final class NaturalAstToControlFlowImpl extends AbstractAstToControlFlowImpl {

	/**
	 * Constructor.
	 * 
	 * @param rootNode the root node of the AST
	 */
	public NaturalAstToControlFlowImpl(final AstNodePojo rootNode) {
		super(rootNode);
	}
	
	@Override
	protected Tuple2<ControlFlowResult, List<ControlFlowPrototype>> resolveControlFlow(final Set<AstNodePojo> entries, final AstNodePojo rootNode) {
		return NaturalControlFlowResolver.getControlFlow(entries, rootNode);
	}

	@Override
	protected void addEntryPoints(final AstNodePojo entryNode) {
		final Optional<AstNodePojo> firstEntry = new AstNodeCollector(n -> !entryNode.equals(n) && AstNodeUtils.hasAnySuperType(n, AstNodeUtils.STATEMENT)
				&& NaturalSpecificHandler.isNaturalControlflowRelevantStatement(n)).firstDeep(entryNode);
		if (firstEntry.isPresent()) {
			final AstNodePojo firstEntryNode = firstEntry.get();
			addEntryPoint(firstEntryNode);
		}
		super.addEntryPoints(entryNode);
	}
}
