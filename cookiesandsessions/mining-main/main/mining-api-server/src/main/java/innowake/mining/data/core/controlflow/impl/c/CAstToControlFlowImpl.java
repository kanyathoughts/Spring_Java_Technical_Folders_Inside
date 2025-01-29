/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import java.util.List;
import java.util.Set;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.controlflow.impl.AbstractAstToControlFlowImpl;
import innowake.mining.data.core.controlflow.impl.ControlFlowResult;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Traverses AST nodes and calculates control flow for C program.
 */
public final class CAstToControlFlowImpl extends AbstractAstToControlFlowImpl {
	
	/**
	 * Constructor to instantiate {@link CAstToControlFlowImpl}.
	 * 
	 * @param rootNode the root node of the AST
	 */
	public CAstToControlFlowImpl(final AstNodePojo rootNode) {
		super(rootNode);
	}

	@Override
	protected Tuple2<ControlFlowResult, List<ControlFlowPrototype>> resolveControlFlow(Set<AstNodePojo> entries, AstNodePojo rootNode) {
		return CControlFlowResolver.getControlFlow(entries, rootNode);
	}
}
