/* 
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. 
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves the control flow for IF/ELSE statements.
 */
public class IfElseResolver extends AbstractCobolBranchStatementResolver {

	IfElseResolver(final AstNodePojo ifStatement, final CobolControlFlowContext context) {
		super(ifStatement, context);
	}

	@Override
	protected Set<AstNodePojo> getBranches(final AstNodePojo currentNode) {
		final Set<AstNodePojo> branches = new HashSet<>();
		for (final AstNodePojo node : currentNode.getChildren()) {
			if (node.getSuperTypes().contains(AstNodeUtils.BRANCH)) {
				addFlowsControlEdgeWithLabel(currentNode, node, node);
				hasDefaultBranch = hasDefaultBranch || node.getSuperTypes().contains(AstNodeUtils.DEFAULT_BRANCH);
				final Optional<AstNodePojo> firstStatement = AstNodeUtils.getFirstChild(node, AstNodeUtils.STATEMENT);
				if ( ! firstStatement.isPresent()) {
					context.getFinalResult().addErrorStatement(Tuple2.of(node, "No statements found in THEN/ELSE block."));
					continue;
				}
				final AstNodePojo firstStatementNode = firstStatement.get();
				addFlowsControlEdgeWithoutLabel(node, firstStatementNode);
				branches.add(firstStatementNode);
			}
		}
		return branches;
	}

	@Override
	@Nullable
	protected String createDefaultLabel() {
		return CobolLabelResolver.FALSE;
	}
}
