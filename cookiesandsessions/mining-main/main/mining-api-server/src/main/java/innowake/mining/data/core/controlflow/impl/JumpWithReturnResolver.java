/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolver for jump statements with return.
 * The target of the jump has to be a parent node to all statements that are executed before the return.
 * 
 * @param <S> the actual statement type
 * @param <C> the actual control flow context type
 */
public class JumpWithReturnResolver<S extends Statement, C extends ControlFlowContext<S, C>> extends BranchStatementResolver<S, C> {

	/**
	 * Constructor
	 * @param jumpStatement the jump statement
	 * @param context the context
	 */
	public JumpWithReturnResolver(final S jumpStatement, final C context) {
		super(jumpStatement, context);
	}

	@Override
	protected Set<Tuple2<AstNodePojo, S>> getBranches(final S currentStatement) {
		hasDefaultBranch = true;
		final Set<Tuple2<AstNodePojo, S>> branches = new HashSet<>();
		for (final AstNodePojo target : context.getJumpStorage().getJumpTargets(currentStatement.getAstNode())) {
			if (context.getJumpStorage().shouldCalculateJumpTarget(currentStatement, target)) {
				final Optional<S> targetStatement;
				if (context.getLanguageSpecificHandler().isControlflowRelevantStatement(target)) {
					targetStatement = context.getLanguageSpecificHandler().createNextStatement(currentStatement, Optional.of(target));
				} else {
					targetStatement = getFirstChildStatement(target, currentStatement);
				}
				targetStatement.ifPresent(statement -> {
					branches.add(Tuple2.of(target, statement));
					addFlowsControlEdgeWithLabel(currentStatement, statement.getAstNode(), target);
				});
			}
		}
		return branches;
	}
}
