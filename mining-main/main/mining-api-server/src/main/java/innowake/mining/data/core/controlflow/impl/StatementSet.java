/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

/**
 * Set for the control flow calculations that ensures that statements representing the same AST node are getting merged.
 * @param <S> the type of statements
 */
public interface StatementSet<S extends Statement> extends Iterable<S> {

	/**
	 * Adds a statement to the set. If the set already contains a statement representing the same AST node the statements get merged.
	 * 
	 * @param statement the statement to add
	 */
	void add(S statement);

	/**
	 * @return {@code true} if the set is empty; {@code false} otherwise
	 */
	boolean isEmpty();
}
