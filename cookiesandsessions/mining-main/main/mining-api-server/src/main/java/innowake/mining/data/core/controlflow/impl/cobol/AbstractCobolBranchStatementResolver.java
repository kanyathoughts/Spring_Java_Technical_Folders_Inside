/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Base class for branching statement resolver
 */
abstract class AbstractCobolBranchStatementResolver extends AbstractCobolControlFlowResolver {

	protected boolean hasDefaultBranch = false;
	
	protected AbstractCobolBranchStatementResolver(final AstNodePojo branchStatement, final CobolControlFlowContext context) {
		super(Collections.emptySet(), context);
		next = getBranches(branchStatement);
		if ( ! next.isEmpty()) {
			addBranchStatementSuperType(branchStatement);
		}
		if ( ! hasDefaultBranch) {
			result.addLastSimpleStatement(new DefaultStatement(branchStatement), createDefaultLabel());
		}
	}

	/**
	 * Called by the constructor to add the {@link AstNodeUtils#BRANCH_STATEMENT} super type.
	 *
	 * @param branchStatement the branch statement
	 */
	protected void addBranchStatementSuperType(final AstNodePojo branchStatement) {
		branchStatement.addSuperType(AstNodeUtils.BRANCH_STATEMENT);
	}

	@Nullable
	protected abstract String createDefaultLabel();

	protected abstract Set<AstNodePojo> getBranches(final AstNodePojo currentNode);
	
	@Override
	protected Optional<AstNodePojo> nextNode(AstNodePojo previousNode) {
		return AstNodeUtils.getNextSiblingDeep(previousNode, AstNodeUtils.STATEMENT);
	}	
}
