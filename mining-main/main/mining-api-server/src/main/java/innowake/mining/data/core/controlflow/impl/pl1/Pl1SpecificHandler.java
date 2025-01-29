/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import innowake.mining.data.core.Pl1AstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractControlFlowResolver;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.LanguageSpecificHandler;
import innowake.mining.data.core.controlflow.impl.StatementSet;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Pl1 specific logic for the control flow calculation.
 */
public class Pl1SpecificHandler implements LanguageSpecificHandler<DefaultStatement, Pl1ControlFlowContext> {

	private static final Set<String> controlFlowIrrelevantPl1Statements = new HashSet<>();
	private static final Set<String> wrappingStatements = new HashSet<>();

	static {
		controlFlowIrrelevantPl1Statements.add(Pl1AstNodeType.OTHERWISE_STMT);
		controlFlowIrrelevantPl1Statements.add(Pl1AstNodeType.SELECT_STMT);
		controlFlowIrrelevantPl1Statements.add(Pl1AstNodeType.WHEN_STMT);
		controlFlowIrrelevantPl1Statements.add(Pl1AstNodeType.DO_STMT);
		wrappingStatements.add(Pl1AstNodeType.PROCEDURE_BLOCK);
		wrappingStatements.add(Pl1AstNodeType.SELECT_GROUP);
		wrappingStatements.add(Pl1AstNodeType.BEGIN_BLOCK);
	}

	@Override
	public StatementSet<DefaultStatement> createEmptyStatementSet() {
		return new Pl1StatementSet();
	}

	@Override
	public boolean statementChildrenContainsNextStatement(final DefaultStatement currentStatement) {
		return wrappingStatements.contains(currentStatement.getAstNode().getType())
				&& ! currentStatement.getAstNode().getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT);
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
			final AbstractControlFlowResolver<DefaultStatement, Pl1ControlFlowContext> currentControlFlowResolver) {
		return false;
	}

	@Override
	public boolean isControlflowRelevantStatement(final AstNodePojo statementNode) {
		return ! controlFlowIrrelevantPl1Statements.contains(statementNode.getType());
	}

	@Override
	public boolean shouldVisitFunctionCalls(final AstNodePojo statementNode) {
		return ! (statementNode.getSuperTypes().contains(AstNodeUtils.LOOP_STATEMENT) || statementNode.getSuperTypes().contains(AstNodeUtils.BRANCH_STATEMENT));
	}

	@Override
	public boolean isNoFallthroughStatement(final AstNodePojo statementNode) {
		return statementNode.getType().equals(Pl1AstNodeType.PROCEDURE_BLOCK);
	}

}
