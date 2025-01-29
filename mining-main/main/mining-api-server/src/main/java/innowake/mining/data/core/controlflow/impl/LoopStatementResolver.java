/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.Optional;

/**
 * Base class for loop statement resolver
 */
class LoopStatementResolver<S extends Statement, C extends ControlFlowContext<S, C>> extends AbstractControlFlowResolver<S, C> {

	final S loopStatement;
	
	/**
	 * Constructor.
	 * 
	 * @param loopStatement the loop statement
	 * @param context the context of the control flow resolver
	 */
	public LoopStatementResolver(final S loopStatement, final C context) {
		super(context);
		this.parentForNextNode = loopStatement.getAstNode();
		this.loopStatement = loopStatement;
		next = createEmptyStatementSet();
		final Optional<S> statement = getFirstChildStatement(loopStatement.getAstNode(), loopStatement);
		if (statement.isPresent()) {
			next.add(statement.get());
			addFlowsControlEdgeWithLabel(loopStatement, statement.get().getAstNode(), loopStatement.getAstNode());
		}
	}
}
