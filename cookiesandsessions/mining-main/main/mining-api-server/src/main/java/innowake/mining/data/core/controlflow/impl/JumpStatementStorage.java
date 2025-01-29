/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.Set;

import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Storage for jump specific information.
 * @param <S> the actual statement type
 */
public interface JumpStatementStorage<S extends Statement> {

	/**
	 * Returns all target nodes that a jump statement can jump to
	 *
	 * @param jumpStatement the jump statement
	 * @return the target nodes
	 */
	Set<AstNodePojo> getJumpTargets(AstNodePojo jumpStatement);

	/**
	 * Returns if a jump needs to be calculated, this should return false if the jump statement has already been handled.
	 * This method takes the state of the statement in consideration.
	 *
	 * @param jumpStatement the jump statement
	 * @param targetStatement the target of the jump statement
	 * @return {@code true} if the control flow for this jump needs to be calculated; {@code false} if it has already been calculated. 
	 */
	boolean shouldCalculateJumpTarget(S jumpStatement, AstNodePojo targetStatement);
	
}
