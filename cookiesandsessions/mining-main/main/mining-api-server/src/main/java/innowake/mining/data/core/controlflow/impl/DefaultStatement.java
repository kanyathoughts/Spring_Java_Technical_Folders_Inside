/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * The default case of a Statement for control flow calculation.
 */
public class DefaultStatement implements Statement {
	
	private final AstNodePojo statementNode;
	
	public DefaultStatement(final AstNodePojo statementNode) {
		this.statementNode = statementNode;
	}

	@Override
	public AstNodePojo getAstNode() {
		return statementNode;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + statementNode.hashCode();
		return result;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final DefaultStatement other = (DefaultStatement) obj;
		return statementNode.equals(other.statementNode);
	}

	@Override
	public String toString() {
		return statementNode.getType() + ": " + statementNode.getLabel()
			+ " @" + statementNode.getLocation().getRetracedOffset().orElse(null) 
			+ "+" + statementNode.getLocation().getRetracedLength().orElse(null) 
			+ " " + statementNode.getSuperTypes().toString();
	}

}
