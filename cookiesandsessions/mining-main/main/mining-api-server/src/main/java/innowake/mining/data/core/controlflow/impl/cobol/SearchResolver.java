/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.CobolAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResultImpl;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves the control flow for SEARCH statements.
 */
public class SearchResolver extends AbstractCobolBranchStatementResolver {
	
	private final AstNodePojo searchStatement;
	@Nullable
	private AstNodePojo atEnd;

	SearchResolver(final AstNodePojo searchStatement, final CobolControlFlowContext context) {
		super(searchStatement, context);
		this.searchStatement = searchStatement;
	}
	
	@Override
	protected ControlFlowSubResult<DefaultStatement> createControlFlow() {
		final ControlFlowSubResult<DefaultStatement> branchResult = super.createControlFlow();
		result = new ControlFlowSubResultImpl<>();
		branchResult.getLastSimpleStatements().forEach(statement -> addControlFlowEdge(statement.getKey().getAstNode(), searchStatement, statement.getValue()));
		if (atEnd != null) {
			final AstNodePojo atEndNullSave = atEnd;
			next.add(atEndNullSave);
			addControlFlowEdge(searchStatement, atEndNullSave, createDefaultLabel());
			super.createControlFlow();
		} else {
			result.addLastSimpleStatement(new DefaultStatement(searchStatement), createDefaultLabel());
		}
		return result;
	}
	
	@Override
	protected Set<AstNodePojo> getBranches(final AstNodePojo currentNode) {
		/* Since the default branch for search works differently than normal branch statements, the handling for non existing default branches
		 * that the super class provides cannot be used. So this flag needs to be set to true and the actual handling for the default branch needs 
		 * to be done after the flow of the search loop is calculated. */
		hasDefaultBranch = true;
		final Set<AstNodePojo> branches = new HashSet<>();
		for (final AstNodePojo node : currentNode.getChildren()) {
			if (node.getSuperTypes().contains(AstNodeUtils.BRANCH)) {
				final Optional<AstNodePojo> firstStatement = AstNodeUtils.getFirstChild(node, AstNodeUtils.STATEMENT);
				if (firstStatement.isPresent()) {
					final AstNodePojo firstStatementNode = firstStatement.get();
					if (CobolAstNodeType.AT_END.getType().equals(node.getType())) {
						atEnd = firstStatementNode;
					} else {
						addFlowsControlEdgeWithLabel(currentNode, firstStatementNode, node);
						branches.add(firstStatementNode);
					}
				} else {
					context.getFinalResult().addErrorStatement(Tuple2.of(node, "No statements found in this WHEN block."));
				}
			}
		}
		return branches;
	}

	@Override
	@Nullable
	protected String createDefaultLabel() {
		return CobolLabelResolver.AT_END;
	}
}
