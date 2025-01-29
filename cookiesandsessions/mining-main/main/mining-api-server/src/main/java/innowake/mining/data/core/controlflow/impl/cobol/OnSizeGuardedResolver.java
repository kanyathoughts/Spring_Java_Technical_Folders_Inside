/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves the control flow for statements that can contain ON SIZE ERROR handling.
 */
public class OnSizeGuardedResolver extends AbstractCobolBranchStatementResolver {
	
	@Nullable
	private String defaultLabel = null;
	
	OnSizeGuardedResolver(final AstNodePojo onSizeGuardedStatement, final CobolControlFlowContext context) {
		super(onSizeGuardedStatement, context);
	}
	
	@Override
	protected Set<AstNodePojo> getBranches(final AstNodePojo currentNode) {
		/* Since the current node is actually just an artificial wrapper node, the handling for a branch statement with no default branch does not work */
		hasDefaultBranch = true;
		final Optional<AstNodePojo>  actualStatement = AstNodeUtils.getFirstChild(currentNode, AstNodeUtils.STATEMENT);
		if ( ! actualStatement.isPresent()) {
			return Collections.emptySet();
		}
		final AstNodePojo actualStatementNode = actualStatement.get();
		final Set<AstNodePojo> branches = new HashSet<>();
		int count = 0;
		for (final AstNodePojo node : currentNode.getChildren()) {
			if (node.getSuperTypes().contains(AstNodeUtils.BRANCH)) {
				defaultLabel = defaultLabel == null ? CobolLabelResolver.resolveDefaultLabelFor(node) : null;
				count++;
				addFlowsControlEdgeWithLabel(actualStatementNode, node, node);
				final Optional<AstNodePojo> firstStatement = AstNodeUtils.getFirstChild(node, AstNodeUtils.STATEMENT);
				if (firstStatement.isPresent()) {
					final AstNodePojo firstStatementNode = firstStatement.get();
					addFlowsControlEdgeWithLabel(node, firstStatementNode, firstStatementNode);
					branches.add(firstStatementNode);
				} else {
					context.getFinalResult().addErrorStatement(Tuple2.of(node, "No statements found in this ON SIZE block."));
				}
			}
		}
		/* There can only be ON SIZE ERROR and NOT ON SIZE ERROR, if both are present the statement will have to continue in one of the branches. */
		if (count < 2) {
			result.addLastSimpleStatement(new DefaultStatement(actualStatementNode), createDefaultLabel());
		}
		if (count > 0) {
			/* if at least on size error handling statement is present this node is a branch statement and the actual node needs to get the super type set. */
			super.addBranchStatementSuperType(actualStatementNode);
		}
		return branches;
	}
	
	@Override
	protected void addBranchStatementSuperType(AstNodePojo branchStatement) {
		/* Since the current node is actually just an artificial wrapper node, the handling for the branch statement super type needs to be different. */
	}

	@Override
	@Nullable
	protected String createDefaultLabel() {
		return defaultLabel;
	}
}
