/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import innowake.mining.data.core.controlflow.impl.ControlFlowContext;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.JumpStatementStorageImpl;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Context for the C control flow calculation.
 */
public class CControlFlowContext extends ControlFlowContext<DefaultStatement, CControlFlowContext> {

	/**
	 * Constructor to instantiate {@link CControlFlowContext}
	 * @param root root node of the AST
	 */
	public CControlFlowContext(final AstNodePojo root) {
		super(new JumpStatementStorageImpl<>(new CSpecificHandler()), new CLabelResolver(), new CSpecificHandler());
	}
}
