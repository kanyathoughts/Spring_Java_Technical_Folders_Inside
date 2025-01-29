/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.Map;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Contains the statements where following the control flow ended.
 * @param <S> the actual type of the statements
 */
public interface ControlFlowSubResult<S extends Statement> {
	
	/**
	 * Adds a statement to the simple last statements.
	 * 
	 * @param statement the last statement that was part of the control flow and can be linked to the next statement of the parent control flow
	 */
	public void addLastSimpleStatement(S statement);
	
	public void addLastSimpleStatement(S statement, @Nullable String label);
	
	/**
	 * Adds a statement to the unhandled last statements together with the section or paragraph where it was considered unhandled.
	 * 
	 * @param statement the last statement that was part of the control flow and influences the parent control flow
	 * @param section section or paragraph where the statement was considered unhandled
	 */
	public void addLastUnhandledStatement(S statement, AstNodePojo section);
	
	public void addLastUnhandledStatement(S statement, AstNodePojo section, @Nullable String label);
	
	/**
	 * Returns the statements where no next statement could be determined.
	 * For the final result that is returned after determining the complete control flow, these can be treated as return statements.
	 *
	 * @return last statements of the control flow calculation
	 */
	public Set<Map.Entry<S, String>> getLastSimpleStatements();
	
	/**
	 * Returns the last statements of a sub control flow that have influence on the control flow and need to be handled in the parent statement
	 * For the final result that is returned after determining the complete control flow, this should be empty.
	 *
	 * @return last statements that still need to be handled
	 */
	public Set<Map.Entry<Map.Entry<S, String>, AstNodePojo>> getLastUnhandledStatements();
	
}
