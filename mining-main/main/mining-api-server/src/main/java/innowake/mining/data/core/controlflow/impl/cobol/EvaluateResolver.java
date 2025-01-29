/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves the control flow EVALUATE statements.
 */
public class EvaluateResolver extends AbstractCobolBranchStatementResolver {

	EvaluateResolver(final AstNodePojo evaluateStatement, final CobolControlFlowContext context) {
		super(evaluateStatement, context);
	}
	
	@Override
	protected Set<AstNodePojo> getBranches(final AstNodePojo currentNode) {
		final Set<AstNodePojo> branches = new HashSet<>();
		final Set<AstNodePojo> fallThroughBranches = new HashSet<>();
		for (final AstNodePojo node : currentNode.getChildren()) {
			if (node.getSuperTypes().contains(AstNodeUtils.BRANCH)) {
				addFlowsControlEdgeWithLabel(currentNode, node, node);
				hasDefaultBranch = hasDefaultBranch || node.getSuperTypes().contains(AstNodeUtils.DEFAULT_BRANCH);
				fallThroughBranches.add(node);
				final Optional<AstNodePojo> firstStatement = AstNodeUtils.getFirstChild(node, AstNodeUtils.STATEMENT);
				if ( ! firstStatement.isPresent()) {
					continue;
				}
				final AstNodePojo firstStatementNode = firstStatement.get();
				fallThroughBranches.forEach(b -> {
					addFlowsControlEdgeWithoutLabel(b, firstStatementNode);
				});
				branches.add(firstStatementNode);
				fallThroughBranches.clear();
			}
		}
		return branches;
	}

	@Override
	@Nullable
	protected String createDefaultLabel() {
		return CobolLabelResolver.OTHER;
	}
}
