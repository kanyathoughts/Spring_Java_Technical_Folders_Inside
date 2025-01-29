/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import innowake.mining.data.core.CAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractControlFlowResolver;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.LanguageSpecificHandler;
import innowake.mining.data.core.controlflow.impl.StatementSet;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * C specific logic for the control flow calculation.
 */
public class CSpecificHandler implements LanguageSpecificHandler<DefaultStatement, CControlFlowContext> {

	private static final Set<String> controlFlowIrrelevantCStatements = new HashSet<>();
	private static final Set<String> wrappingStatements = new HashSet<>();

	static {
		wrappingStatements.add(CAstNodeType.FUNCTION_DEFINITION);
		wrappingStatements.add(CAstNodeType.IFELSE_STATEMENT);
		wrappingStatements.add(CAstNodeType.SWITCH_STATEMENT);
		wrappingStatements.add(CAstNodeType.IDENTIFIER_LABELED_STATEMENT);
		controlFlowIrrelevantCStatements.add(CAstNodeType.THEN_BRANCH);
		controlFlowIrrelevantCStatements.add(CAstNodeType.ELSE_BRANCH);
		controlFlowIrrelevantCStatements.add(CAstNodeType.CASE_BRANCH);
		controlFlowIrrelevantCStatements.add(CAstNodeType.DEFAULT_BRANCH);
	}

	@Override
	public StatementSet<DefaultStatement> createEmptyStatementSet() {
		return new CStatement();
	}

	@Override
	public boolean statementChildrenContainsNextStatement(final DefaultStatement currentStatement) {
		return wrappingStatements.contains(currentStatement.getAstNode().getType());
	}

	@Override
	public Optional<DefaultStatement> createNextStatement(final DefaultStatement currentStatement, final Optional<AstNodePojo> nextNode) {
		if ( ! nextNode.isPresent()) {
			return Optional.empty();
		}
		final DefaultStatement nextStatement = new DefaultStatement(nextNode.get());
		return Optional.of(nextStatement);
	}

	@Override
	public boolean handle(final DefaultStatement currentStatement,
			final AbstractControlFlowResolver<DefaultStatement, CControlFlowContext> currentControlFlowResolver) {
		return false;
	}

	@Override
	public boolean isControlflowRelevantStatement(final AstNodePojo statementNode) {
		return ! controlFlowIrrelevantCStatements.contains(statementNode.getType());
	}

	@Override
	public boolean shouldVisitFunctionCalls(final AstNodePojo statementNode) {
		return ! (statementNode.getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT) || statementNode.getSuperTypes().contains(AstNodeUtils.BRANCH_STATEMENT));
	}

	@Override
	public boolean isNoFallthroughStatement(AstNodePojo statementNode) {
		return false;
	}
}
