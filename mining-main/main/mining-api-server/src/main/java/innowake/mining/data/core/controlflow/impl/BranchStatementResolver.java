/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Base class for branching statement resolver
 */
class BranchStatementResolver<S extends Statement, C extends ControlFlowContext<S, C>> extends AbstractControlFlowResolver<S, C> {

	protected boolean hasDefaultBranch = false;
	protected final S branchStatement;
	private final Set<Tuple2<AstNodePojo, S>> branches;
	
	/**
	 * Constructor.
	 * 
	 * @param branchStatement the branch statement
	 * @param context the context of the control flow resolver
	 */
	public BranchStatementResolver(final S branchStatement, final C context) {
		super(context);
		this.branchStatement = branchStatement;
		branches = getBranches(branchStatement);
		if ( ! hasDefaultBranch) {
			result.addLastSimpleStatement(branchStatement, createDefaultLabel(branchStatement.getAstNode()));
		}
	}
	
	@Override
	protected ControlFlowSubResult<S> createControlFlow() {
		for (final Tuple2<AstNodePojo, S> branch : branches) {
			parentForNextNode = branch.a;
			next.add(branch.b);
			super.createControlFlow();
		}
		return result;
	}

	/**
	 * Creates the label for default branch edges.
	 * @param statement the branch statement.
	 * @return the label for the default branch edge.
	 */
	@Nullable
	protected String createDefaultLabel(final AstNodePojo statement) {
		return context.getLabelResolver().resolveDefaultLabelFor(statement);
	}

	/**
	 * Extracts the branches of the current branch statement.
	 * @param currentStatement the current branch statement.
	 * @return the branches of the current branch statement.
	 */
	protected Set<Tuple2<AstNodePojo, S>> getBranches(final S currentStatement) {
		final Set<Tuple2<AstNodePojo, S>> result = new HashSet<>();
		for (final AstNodePojo node : AstNodeUtils.getChildren(currentStatement.getAstNode(), AstNodeUtils.BRANCH)) {
			final Optional<S> statement = getFirstChildStatement(node, currentStatement);
			if (statement.isPresent()) {
				result.add(Tuple2.of(node, statement.get()));
				addFlowsControlEdgeWithLabel(currentStatement, statement.get().getAstNode(), node);
				if (node.getSuperTypes().contains(AstNodeUtils.DEFAULT_BRANCH)) {
					hasDefaultBranch = true;
				}
			}
		}
		return result;
	}
}
