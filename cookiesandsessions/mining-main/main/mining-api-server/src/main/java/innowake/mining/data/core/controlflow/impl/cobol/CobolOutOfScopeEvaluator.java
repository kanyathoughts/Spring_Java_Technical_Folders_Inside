/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.Optional;

import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Functional interface for special control flow resolver to prevent a statement from running out of its scope.
 */
public interface CobolOutOfScopeEvaluator {
	
	/**
	 * Evaluates if the next statement is out of the scope of the current resolver and returns the AstNode representing the last block of the scope if the next
	 * statement is no longer in the current scope.
	 *
	 * @param currentNode the current statement that is still handled by the resolver
	 * @param nextNode the next statement
	 * @return the section / paragraph of which the control flow would be leaving or {@link Optional#empty()} if the next statement is still in scope
	 */
	Optional<AstNodePojo> getLastBlockIfOutOfScope(AstNodePojo currentNode, AstNodePojo nextNode);
	
}
