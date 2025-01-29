/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import innowake.mining.data.core.controlflow.impl.ControlFlowContext;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.JumpStatementStorageImpl;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Context for the Pl1 control flow calculation.
 */
public class Pl1ControlFlowContext extends ControlFlowContext<DefaultStatement, Pl1ControlFlowContext> {

	/**
	 * Constructor to instantiate {@link Pl1ControlFlowContext}
	 * 
	 * @param root node of type AST node
	 */
	public Pl1ControlFlowContext(final AstNodePojo root) {
		super(new JumpStatementStorageImpl<>(new Pl1SpecificHandler()), new Pl1LabelResolver(), new Pl1SpecificHandler());
	}

}
