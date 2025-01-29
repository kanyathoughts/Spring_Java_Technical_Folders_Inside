/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.java;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import innowake.mining.data.core.JavaAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractControlFlowResolver;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.LanguageSpecificHandler;
import innowake.mining.data.core.controlflow.impl.StatementSet;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Java specific logic for the control flow calculation.
 */
public class JavaSpecificHandler implements LanguageSpecificHandler<DefaultStatement, JavaControlFlowContext> {

	private static final Set<String> wrappingStatements = new HashSet<>();
	private static final Set<String> controlFlowIrrelevantCStatements = new HashSet<>();

	static {
		wrappingStatements.add(JavaAstNodeType.FUNCTION_DEFINITION);
		wrappingStatements.add(JavaAstNodeType.IFELSE_STATEMENT);
		wrappingStatements.add(JavaAstNodeType.SWITCH_STATEMENT);
		wrappingStatements.add(JavaAstNodeType.BLOCK);
		wrappingStatements.add(JavaAstNodeType.IDENTIFIER_LABELED_STATEMENT);
		controlFlowIrrelevantCStatements.add(JavaAstNodeType.THEN_BRANCH);
		controlFlowIrrelevantCStatements.add(JavaAstNodeType.CASE_BRANCH);
		controlFlowIrrelevantCStatements.add(JavaAstNodeType.ELSE_BRANCH);
		controlFlowIrrelevantCStatements.add(JavaAstNodeType.BLOCK);
	}
	
	@Override
	public StatementSet<DefaultStatement> createEmptyStatementSet() {
		return new JavaStatementSet();
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
			final AbstractControlFlowResolver<DefaultStatement, JavaControlFlowContext> currentControlFlowResolver) {
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
	public boolean isNoFallthroughStatement(final AstNodePojo statementNode) {
		return false;
	}
}
