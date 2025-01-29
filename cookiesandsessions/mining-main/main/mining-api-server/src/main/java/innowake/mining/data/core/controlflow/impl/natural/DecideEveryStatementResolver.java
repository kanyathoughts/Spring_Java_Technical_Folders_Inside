/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractControlFlowResolver;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResultImpl;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Control Flow Resolver for Natural DECIDE FOR/ON EVERY statements
 */
class DecideEveryStatementResolver extends AbstractControlFlowResolver<NaturalStatement, NaturalControlFlowContext> {
	
	private final List<Tuple2<AstNodePojo, NaturalStatement>> branches = new ArrayList<>();
	private final Optional<Tuple2<AstNodePojo, NaturalStatement>> defaultBranch;
	
	public DecideEveryStatementResolver(final NaturalStatement branchStatement, final NaturalControlFlowContext context) {
		super(context);
		next = createEmptyStatementSet();
		defaultBranch = initBranches(branchStatement);
	}

	@Override
	protected ControlFlowSubResult<NaturalStatement> createControlFlow() {
		ControlFlowSubResult<NaturalStatement> completeResult = new ControlFlowSubResultImpl<>();
		for (int i = 0; i < branches.size(); i++) {
			result = new ControlFlowSubResultImpl<>();
			final Tuple2<AstNodePojo, NaturalStatement> branch = branches.get(i);
			next.add(branch.b);
			parentForNextNode = branch.a;
			super.createControlFlow();
			for (int j = i + 1; j < branches.size(); j++) {
				final Tuple2<AstNodePojo, NaturalStatement> upcomingBranch = branches.get(j);
				final String labelForDecide = context.getLabelResolver().resolveLabelFor(upcomingBranch.a);
				final AstNodePojo nextNode = upcomingBranch.b.getAstNode();
				for (final var lastSimpleStatement : result.getLastSimpleStatements()) {
					addFlowsControlEdgeWithLabel(lastSimpleStatement.getKey(), nextNode, mergeEdgeLabels(lastSimpleStatement.getValue(), labelForDecide));
					final Optional<NaturalStatement> nextStatement = context.getLanguageSpecificHandler()
							.createNextStatement(lastSimpleStatement.getKey(), Optional.of(nextNode));
					if (nextStatement.isPresent()) {
						next.add(nextStatement.get());
					}
				}
			}
			mergeControlFlowResult(completeResult);
			completeResult = result;
		}
		if (defaultBranch.isPresent()) {
			result = new ControlFlowSubResultImpl<>();
			next.add(defaultBranch.get().b);
			parentForNextNode = defaultBranch.get().a;
			super.createControlFlow();
			mergeControlFlowResult(completeResult);
			completeResult = result;
		}
		return completeResult;
	}
	
	@Nullable
	private String mergeEdgeLabels(@Nullable final String labelStart, @Nullable final String decideLabel) {
		if (labelStart == null) {
			return decideLabel;
		}
		final StringBuilder label = new StringBuilder("(")
				.append(labelStart)
				.append(") AND (")
				.append(decideLabel)
				.append(")");
		return label.toString();
	}

	/**
	 * Fills the branches List with branches and returns the default branch.
	 * @param currentStatement the current branch statement.
	 * @return the default branch of the current branch statement.
	 */
	protected Optional<Tuple2<AstNodePojo, NaturalStatement>> initBranches(final NaturalStatement currentStatement) {
		Optional<Tuple2<AstNodePojo, NaturalStatement>> result = Optional.empty();
		for (final AstNodePojo node : AstNodeUtils.getChildren(currentStatement.getAstNode(), AstNodeUtils.BRANCH)) {
			final Optional<AstNodePojo> firstStatementInBranch = AstNodeUtils.getFirstChild(node, AstNodeUtils.STATEMENT);
			final Optional<NaturalStatement> statement = context.getLanguageSpecificHandler().createNextStatement(currentStatement, firstStatementInBranch);
			if (statement.isPresent()) {
				if (node.getSuperTypes().contains(AstNodeUtils.DEFAULT_BRANCH)) {
					result = Optional.of(Tuple2.of(node, statement.get()));
				} else {
					branches.add(Tuple2.of(node, statement.get()));
				}
				addFlowsControlEdgeWithLabel(currentStatement,  statement.get().getAstNode(), node);
			}
		}
		return result;
	}
}
