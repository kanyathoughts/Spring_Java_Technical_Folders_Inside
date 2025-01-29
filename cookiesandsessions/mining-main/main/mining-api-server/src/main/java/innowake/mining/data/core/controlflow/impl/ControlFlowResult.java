/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.Set;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
. * Contains the statements where following the control flow ended.
 */
public interface ControlFlowResult {
	
	/**
	 * Adds a statement to the return statements.
	 * 
	 * @param returnStatement the statement that was identified as a return statement
	 */
	public void addReturnStatement(AstNodePojo returnStatement);

	/**
	 * Adds a statement to the halt statements.
	 * 
	 * @param haltStatement the statement that was identified as a halt statement
	 */
	public void addHaltStatement(AstNodePojo haltStatement);
	
	/**
	 * Returns the statements where the program gives control back to the caller.
	 *
	 * @return the return statements
	 */
	public Set<AstNodePojo> getReturnStatements();
	
	/**
	 * Returns the statements where the program ends the execution.
	 *
	 * @return the halt statements
	 */
	public Set<AstNodePojo> getHaltStatements();
	
	/**
	 * Adds a statement to the error statements. These are statements where no further control flow could be determined but was expected.
	 * 
	 * @param error the statement where no further control flow could be determined, together with an error message
	 */
	public void addErrorStatement(Tuple2<AstNodePojo, String> error);
	
	/**
	 * Returns the error statements. These are statements were no further control flow could be determined but was expected.
	 *
	 * @return the error statements
	 */
	public Set<Tuple2<AstNodePojo, String>> getErrorStatements();
}
