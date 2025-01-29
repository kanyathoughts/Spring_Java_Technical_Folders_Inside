/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import innowake.mining.data.core.controlflow.impl.ControlFlowContext;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.JumpStatementStorageImpl;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Context for the JCL control flow calculation.
 */
public class JclControlFlowContext extends ControlFlowContext<DefaultStatement, JclControlFlowContext> {

	/**
	 * Constructor to instantiate {@link JclControlFlowContext}
	 * @param root root node of the AST
	 */
	public JclControlFlowContext(final AstNodePojo root) {
		super(new JumpStatementStorageImpl<>(new JclSpecificHandler()), new JclLabelResolver(), new JclSpecificHandler());
	}

}
