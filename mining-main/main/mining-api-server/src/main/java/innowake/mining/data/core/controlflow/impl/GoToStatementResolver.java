/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.Optional;

import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolver for GoTo statements. The target can be a parent node to all statements that are executed or a single statement itself.
 * 
 * @param <S> the actual statement type
 * @param <C> the actual control flow context type
 */
public class GoToStatementResolver<S extends Statement, C extends ControlFlowContext<S, C>> extends AbstractControlFlowResolver<S, C> {

	protected final S goToStatement;

	/**
	 * Constructor to instantiate targetNode and GotoStatement
	 * 
	 * @param currentStatement a GOTO statement
	 * @param context a language specific {@link ControlFlowContext}
	 */
	protected GoToStatementResolver(final S currentStatement, final C context) {
		super(context);
		this.goToStatement = currentStatement;
		for (final AstNodePojo target : context.getJumpStorage().getJumpTargets(currentStatement.getAstNode())) {
			if (context.getJumpStorage().shouldCalculateJumpTarget(currentStatement, target)) {
				final Optional<S> targetStatement;
				if (context.getLanguageSpecificHandler().isControlflowRelevantStatement(target)) {
					targetStatement = context.getLanguageSpecificHandler().createNextStatement(currentStatement, Optional.of(target));
				} else {
					targetStatement = getFirstChildStatement(target, currentStatement);
				}
				targetStatement.ifPresent(statement -> {
					next.add(statement);
					handleLinkingAndNestedFunctionCalls(
							currentStatement, context.getLabelResolver().resolveLabelFor(currentStatement.getAstNode()), statement.getAstNode());
				});
			}
		}
	}
}
