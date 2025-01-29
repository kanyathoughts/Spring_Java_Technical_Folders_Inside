/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import java.util.AbstractMap;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.JclAstNodeType;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractControlFlowResolver;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;

import innowake.mining.data.core.controlflow.impl.LanguageSpecificHandler;
import innowake.mining.data.core.controlflow.impl.StatementSet;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * JCL specific logic for the control flow calculation.
 */
public class JclSpecificHandler implements LanguageSpecificHandler<DefaultStatement, JclControlFlowContext> {

	private static final Set<String> wrappingStatements = new HashSet<>();
	private static final Set<String> controlFlowIrrelevantCStatements = new HashSet<>();
	private static final Logger LOG = LoggerFactory.getLogger(JclSpecificHandler.class);

	static {
		wrappingStatements.add(JclAstNodeType.JOB);
		wrappingStatements.add(JclAstNodeType.STEP_IF);
		wrappingStatements.add(JclAstNodeType.STEP_EXEC);
		wrappingStatements.add(JclAstNodeType.CONDITION);
		controlFlowIrrelevantCStatements.add(JclAstNodeType.IF_BRANCH);
		controlFlowIrrelevantCStatements.add(JclAstNodeType.ELSE_BRANCH);
	}

	@Override
	public StatementSet<DefaultStatement> createEmptyStatementSet() {
		return new JclStatementSet();
	}

	@Override
	public boolean statementChildrenContainsNextStatement(final DefaultStatement currentStatement) {
		return wrappingStatements.contains(currentStatement.getAstNode().getType());
	}

	@Override
	public Optional<DefaultStatement> createNextStatement(final DefaultStatement currentStatement, final Optional<AstNodePojo> nextNode) {
		if (nextNode.isPresent()) {
			final String type = nextNode.get().getType();
			final List<AstNodePojo> children = nextNode.get().getChildren();
			if (type.equals(JclAstNodeType.JOB) && ! children.isEmpty()
					&& children.get(0).getType().equals(JclAstNodeType.CONDITION)) {
				return Optional.of(new DefaultStatement(children.get(0)));
			}
			if (type.equals(JclAstNodeType.STEP_EXEC) && ! children.isEmpty()
					&& children.get(0).getType().equals(JclAstNodeType.CONDITION)) {
				return Optional.of(new DefaultStatement(children.get(0)));
			}
		}
		if ( ! nextNode.isPresent()) {
			return Optional.empty();
		}
		final DefaultStatement nextStatement = new DefaultStatement(nextNode.get());
		return Optional.of(nextStatement);
	}

	@Override
	public boolean handle(final DefaultStatement currentStatement,
			final AbstractControlFlowResolver<DefaultStatement, JclControlFlowContext> currentControlFlowResolver) {
		if (currentStatement.getAstNode().getType().equals(JclAstNodeType.CONDITION)) {
			final Optional<AstNodePojo> parent = currentStatement.getAstNode().getParent();
			if (parent.isEmpty()) {
				LOG.warn("parent is null for current statement", currentStatement.getAstNode());
				return true;
			}
			currentControlFlowResolver.addNext(new DefaultStatement(parent.get()));
			currentControlFlowResolver.addControlFlowEdge(new AbstractMap.SimpleEntry<>(currentStatement, JclLabelResolver.FALSE), parent.get());

			final Optional<AstNodePojo> nextNode = currentControlFlowResolver.nextNode(parent.get());
			Optional<DefaultStatement> nextStatement = createNextStatement(currentStatement, nextNode);
			final boolean isNextStatement = ! nextStatement.isPresent() || isControlflowRelevantStatement(nextStatement.get().getAstNode());
			nextStatement = isNextStatement ? nextStatement : currentControlFlowResolver.nextStatement(nextStatement.get(), currentStatement);
			if (nextStatement.isPresent()) {
				currentControlFlowResolver.addControlFlowEdge(new AbstractMap.SimpleEntry<>(currentStatement, JclLabelResolver.TRUE),
						nextStatement.get().getAstNode());
				currentControlFlowResolver.addNext(nextStatement.get());
			} else {
				currentControlFlowResolver.getContext().getFinalResult().addReturnStatement(currentStatement.getAstNode());
			}
			return true;
		}
		return false;
	}

	@Override
	public boolean isControlflowRelevantStatement(final AstNodePojo statementNode) {
		return ! controlFlowIrrelevantCStatements.contains(statementNode.getType());
	}

	@Override
	public boolean shouldVisitFunctionCalls(final AstNodePojo statementNode) {
		return ! statementNode.getSuperTypes().contains(AstNodeUtils.BRANCH_STATEMENT);
	}

	@Override
	public boolean isNoFallthroughStatement(final AstNodePojo statementNode) {
		return false;
	}

}
