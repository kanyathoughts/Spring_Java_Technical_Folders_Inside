/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Statement for control flow calculation containing the AST node.
 */
public interface Statement {
	
	/**
	 * @return the ast node
	 */
	AstNodePojo getAstNode();

}
