/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.Optional;

import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Logic for language specific control flow calculation.
 * @param <S> the type of statements
 * @param <C> the control flow context
 */
public interface LanguageSpecificHandler<S extends Statement, C extends ControlFlowContext<S,C>> {
	
	/**
	 * Creates a statement set for the used type of statements.
	 * 
	 * @return a new statement set.
	 */
	StatementSet<S> createEmptyStatementSet();

	/**
	 * Determines wether the children of a node should be visited to search the next statement.
	 * 
	 * @param currentStatement the statement with the current node
	 * @return {@code true} if the children can contain statements that should be visited next; {@code false} otherwise
	 */
	boolean statementChildrenContainsNextStatement(S currentStatement);

	/**
	 * Creates the next statement based on the current statement and the node of the next statement.
	 * @param currentStatement the current statement
	 * @param nextNode the node for the next statement
	 * @return the next statement if present; {@link Optional#empty()} otherwise
	 */
	Optional<S> createNextStatement(S currentStatement, Optional<AstNodePojo> nextNode);

	/**
	 * Performs language specific control flow calculation if needed.
	 *
	 * @param currentStatement the current statement
	 * @param currentControlFlowResolver the current control flow resolver
	 * @return {@code true} if the statement has been handled language specifically; {@code false} if the generic default handling should be used
	 */
	boolean handle(S currentStatement, AbstractControlFlowResolver<S, C> currentControlFlowResolver);

	/**
	 * Determines whether a statement should appear in the control flow.
	 * 
	 * @param statementNode the statement to check
	 * @return {{@code true} if the statement is relevant for the control flow; {@code false} otherwise 
	 */
	boolean isControlflowRelevantStatement(AstNodePojo statementNode);
	
	/**
	 * Determines whether a statement should be checked for the nested function calls present in it.
	 *
	 * @param statementNode the statement to check
	 * @return {{@code true} if the statement contains nested function calls; {{@code false} if the statement does not contain nested calls.
	 */
	boolean shouldVisitFunctionCalls(final AstNodePojo statementNode);
	
	/**
	 * Determines whether a statement should be checked if no fall through Statement .
	 *
	 * @param statementNode the statement to check
	 * @return {{@code true} if the statement contains fall through Statement; {{@code false} if the statement does not fall through Statement.
	 */
	boolean isNoFallthroughStatement(final AstNodePojo statementNode);
}
