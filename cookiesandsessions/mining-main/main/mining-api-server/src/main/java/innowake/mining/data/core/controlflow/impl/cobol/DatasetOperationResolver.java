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
 * Resolves the control flow for READ, WRITE, REWRITE, START and RETURN statements.
 */
public class DatasetOperationResolver extends AbstractCobolBranchStatementResolver {
	
	@Nullable
	private String defaultLabel = null;

	DatasetOperationResolver(final AstNodePojo datasetStatement, final CobolControlFlowContext context) {
		super(datasetStatement, context);
	}
	
	@Override
	protected Set<AstNodePojo> getBranches(final AstNodePojo currentNode) {
		final Set<AstNodePojo> branches = new HashSet<>();
		int count = 0;
		for (final AstNodePojo node : currentNode.getChildren()) {
			if (node.getSuperTypes().contains(AstNodeUtils.BRANCH)) {
				defaultLabel = defaultLabel == null ? CobolLabelResolver.resolveDefaultLabelFor(node) : null;
				count++;
				final Optional<AstNodePojo> firstStatement = AstNodeUtils.getFirstChild(node, AstNodeUtils.STATEMENT);
				if (firstStatement.isPresent()) {
					final AstNodePojo firstStatementNode = firstStatement.get();
					addFlowsControlEdgeWithLabel(currentNode, firstStatementNode, node);
					branches.add(firstStatementNode);
				} else {
					context.getFinalResult().addErrorStatement(Tuple2.of(node, "No statements found in this conditional block."));
				}
			}
		}
		/* These statements can have two branches, one for if a condition is met, the other for if the condition is not met. If there are two branches
		 * than the statement cannot continue without executing one of the branches. */
		hasDefaultBranch = count == 2;
		return branches;
	}

	@Override
	@Nullable
	protected String createDefaultLabel() {
		return defaultLabel;
	}
}
