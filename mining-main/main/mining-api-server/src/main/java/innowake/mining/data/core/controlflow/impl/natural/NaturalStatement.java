/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import java.util.HashSet;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Natural implementation of the statement class that wraps AST node and stores additional information on return points.
 */
public class NaturalStatement extends DefaultStatement {
	
	private final Set<AstNodePojo> lastInputStatementNodes = new HashSet<>();

	/**
	 * Constructor
	 * 
	 * @param statementNode the node of the statement
	 */
	public NaturalStatement(final AstNodePojo statementNode) {
		super(statementNode);
	}
	
	/**
	 * Resets all known last input statements and add a new one.
	 * 
	 * @param inputStmt the new last input statement
	 */
	public void resetAndAddNewLastInputStatementNode(final AstNodePojo inputStmt) {
		lastInputStatementNodes.clear();
		lastInputStatementNodes.add(inputStmt);
	}
	
	/**
	 * Merges the last known input statements.
	 * 
	 * @param inputStatementNodes the last known input statements to merge.
	 */
	public void mergeLastInputStatementNodes(final Set<AstNodePojo> inputStatementNodes) {
		inputStatementNodes.forEach(lastInputStatementNodes::add);
	}

	/**
	 * @return the last known input statements
	 */
	public Set<AstNodePojo> getLastInputStatementNodes() {
		return lastInputStatementNodes;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + lastInputStatementNodes.hashCode();
		return result;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		if ( ! lastInputStatementNodes.equals(((NaturalStatement)obj).lastInputStatementNodes)) {
			return false;
		}
		return super.equals(obj);
	}
}
