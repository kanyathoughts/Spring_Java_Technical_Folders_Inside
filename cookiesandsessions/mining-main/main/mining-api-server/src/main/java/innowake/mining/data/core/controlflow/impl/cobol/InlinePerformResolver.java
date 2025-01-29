/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.HashSet;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Resolves the control flow for inline PERFORM statements.
 */
public class InlinePerformResolver extends AbstractCobolControlFlowResolver {

	InlinePerformResolver(final AstNodePojo performStatement, @Nullable final String label, final CobolControlFlowContext context) {
		super(new HashSet<>(), context);
		final Optional<AstNodePojo> firstStatement = AstNodeUtils.getFirstChild(performStatement, AstNodeUtils.STATEMENT);
		if (firstStatement.isPresent()) {
			final AstNodePojo firstStatementNode = firstStatement.get();
			addControlFlowEdge(performStatement, firstStatementNode, label);
			next.add(firstStatementNode);
		}
	}
	
	@Override
	protected Optional<AstNodePojo> nextNode(final AstNodePojo previousNode) {
		return AstNodeUtils.getNextSiblingDeep(previousNode, AstNodeUtils.STATEMENT);
	}	
	
}
