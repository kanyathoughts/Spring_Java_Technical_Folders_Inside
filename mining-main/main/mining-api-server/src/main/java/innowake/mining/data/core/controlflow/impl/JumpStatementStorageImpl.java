/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.ndt.core.parsing.ast.model.statement.CallInternalStatement;
import innowake.ndt.core.parsing.ast.model.statement.JumpStatement;

/**
 * A Base implementation for handling CFG for {@link CallInternalStatement} and {@link JumpStatement}
 * @param <S> {@link Statement}
 */
public class JumpStatementStorageImpl<S extends Statement> implements JumpStatementStorage<S>{

	final Map<AstNodePojo, Set<S>> alreadyCalculated = new HashMap<>();
	final StatementSet<S> statementSet;
	
	/** 
	 * Constructor to instantiate language specific Statement Set
	 * @param <C> ControlFlowContext object for Control Flow Resolver.
	 * @param languageSpecificHandler to get the Statement Set
	 */
	public <C extends ControlFlowContext<S,C>> JumpStatementStorageImpl(final LanguageSpecificHandler<S,C> languageSpecificHandler) {
		statementSet = languageSpecificHandler.createEmptyStatementSet();
	}
	
	@Override
	public Set<AstNodePojo> getJumpTargets(final AstNodePojo jumpStatementAstNode) {
		if (jumpStatementAstNode.getSuperTypes().contains(AstNodeUtils.CALL_INTERNAL_STATEMENT)) {
			return jumpStatementAstNode.getOutgoingRelations().stream()
					.filter(edge -> AstRelationshipType.BINDING.equals(edge.getType()))
					.filter(edge -> CallInternalStatement.CALLS.equals(edge.getLabel().orElse(null)))
					.map(AstRelationshipPojo::getDstNode)
					.collect(Collectors.toSet());
		} else if (jumpStatementAstNode.getSuperTypes().contains(AstNodeUtils.JUMP_STATEMENT)) {
			return jumpStatementAstNode.getOutgoingRelations().stream()
					.filter(edge -> AstRelationshipType.BINDING.equals(edge.getType()))
					.filter(edge -> JumpStatement.JUMPS_TO.equals(edge.getLabel().orElse(null)))
					.map(AstRelationshipPojo::getDstNode)
					.collect(Collectors.toSet());
		}
		return Collections.emptySet();
	}

	@Override
	public boolean shouldCalculateJumpTarget(final S statement, final AstNodePojo target) {
		return alreadyCalculated.computeIfAbsent(target, node -> new HashSet<>()).add(statement);
	}
}
