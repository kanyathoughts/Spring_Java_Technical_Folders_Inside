/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.java;

import innowake.mining.data.core.controlflow.impl.ControlFlowContext;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.JumpStatementStorageImpl;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Context for calculation of Java Control Flow
 */
public class JavaControlFlowContext extends ControlFlowContext<DefaultStatement, JavaControlFlowContext> {

	/**
	 * Constructor to instantiate {@link JavaControlFlowContext}
	 * @param node AST node to start from
	 */
	public JavaControlFlowContext(final AstNodePojo node) {
		super(new JumpStatementStorageImpl<>(new JavaSpecificHandler()), new JavaLabelResolver(), new JavaSpecificHandler());
	}
}
