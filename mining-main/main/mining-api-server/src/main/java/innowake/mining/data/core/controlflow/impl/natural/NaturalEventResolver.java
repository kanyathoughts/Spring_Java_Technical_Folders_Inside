/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import java.util.Optional;

import innowake.mining.data.core.controlflow.impl.AbstractControlFlowResolver;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Control flow resolver for natural loop event statements.
 */
public class NaturalEventResolver extends AbstractControlFlowResolver<NaturalStatement, NaturalControlFlowContext>{

	protected NaturalEventResolver(final NaturalStatement loopStmt, final AstNodePojo eventNode, final NaturalControlFlowContext context) {
		super(context);
		parentForNextNode = eventNode;
		final Optional<NaturalStatement> firstStatement = getFirstChildStatement(eventNode, loopStmt);
		if (firstStatement.isPresent()) {
			next.add(firstStatement.get());
			addFlowsControlEdgeWithLabel(loopStmt, firstStatement.get().getAstNode(), eventNode);
		}
	}

	@Override
	public ControlFlowSubResult<NaturalStatement> createControlFlow() {
		return super.createControlFlow();
	}
}
