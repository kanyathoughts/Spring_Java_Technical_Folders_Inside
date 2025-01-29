/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import java.util.List;
import java.util.Set;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.controlflow.impl.AbstractAstToControlFlowImpl;
import innowake.mining.data.core.controlflow.impl.ControlFlowResult;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Traverses AST nodes and calculates control flow for JCL
 */
public class JclAstToControlFlowImpl extends AbstractAstToControlFlowImpl {

	/**
	 * Constructor to instantiate {@link JclAstToControlFlowImpl}.
	 * 
	 * @param rootNode the root node of the AST
	 */
	public JclAstToControlFlowImpl(final AstNodePojo rootNode) {
		super(rootNode);
	}

	@Override
	protected Tuple2<ControlFlowResult, List<ControlFlowPrototype>> resolveControlFlow(Set<AstNodePojo> entries, AstNodePojo rootNode) {
		return JclControlFlowResolver.getControlFlow(entries, rootNode);
	}

}
