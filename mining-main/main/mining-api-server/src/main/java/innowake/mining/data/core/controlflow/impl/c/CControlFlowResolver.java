/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.c;

import java.util.List;
import java.util.Set;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractControlFlowResolver;
import innowake.mining.data.core.controlflow.impl.ControlFlowResult;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.StatementSet;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Control flow resolver for C that contains the entry point to start a control flow calculation.
 */
public class CControlFlowResolver extends AbstractControlFlowResolver<DefaultStatement, CControlFlowContext> {

	/**
	 * Constructor to instantiate {@link CControlFlowResolver}
	 * 
	 * @param context of type {@link CControlFlowContext}
	 */
	protected CControlFlowResolver(final CControlFlowContext context) {
		super(context);
	}

	/**
	 * Constructor to instantiate {@link CControlFlowResolver}.
	 * 
	 * @param entryStatements type of {@link StatementSet} initially of type {@link DefaultStatement}
	 * @param cContext context of type {@link CControlFlowContext}
	 */
	public CControlFlowResolver(final StatementSet<DefaultStatement> entryStatements, final CControlFlowContext cContext) {
		super(entryStatements, cContext);
	}

	/**
	 * Follows the control flow, links the AST nodes and returns the statements where the control flow ends.
	 *
	 * @param entryStatements the entry points of the C program.
	 * @param rootNode the root node
	 * @return the control flow result and the control flow edges
	 */
	public static Tuple2<ControlFlowResult, List<ControlFlowPrototype>> getControlFlow(final Set<AstNodePojo> entryStatements, final AstNodePojo rootNode) {
		final CControlFlowContext cContext = new CControlFlowContext(rootNode);
		final StatementSet<DefaultStatement> entryStatementSet = cContext.getLanguageSpecificHandler().createEmptyStatementSet();
		entryStatements.forEach(statement -> entryStatementSet.add(new DefaultStatement(statement)));
		final CControlFlowResolver resolver = new CControlFlowResolver(entryStatementSet, cContext);
		final ControlFlowSubResult<DefaultStatement> subResult = resolver.createControlFlow();
		final ControlFlowResult finalResult = resolver.context.getFinalResult();
		subResult.getLastSimpleStatements().forEach(statement -> finalResult.addReturnStatement(statement.getKey().getAstNode()));
		for (final var unhandledStatement : subResult.getLastUnhandledStatements()) {
			if (unhandledStatement.getValue().getSuperTypes().contains(AstNodeUtils.RETURN_STATEMENT)) {
				finalResult.addReturnStatement(unhandledStatement.getKey().getKey().getAstNode());
			}
		}
		return Tuple2.of(finalResult, resolver.context.getControlFlowEdges());
	}
	
}
