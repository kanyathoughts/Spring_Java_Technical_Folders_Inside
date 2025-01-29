/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

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
 * Control flow resolver for Pl1 that contains the entry point to start a control flow calculation.
 */
public class Pl1ControlFlowResolver extends AbstractControlFlowResolver<DefaultStatement, Pl1ControlFlowContext> {

	/**
	 * Constructor to instantiate {@link Pl1ControlFlowResolver}
	 * 
	 * @param context of type {@link Pl1ControlFlowContext}
	 */
	protected Pl1ControlFlowResolver(final Pl1ControlFlowContext context) {
		super(context);
	}

	/**
	 * Constructor to instantiate {@link Pl1ControlFlowResolver}.
	 * 
	 * @param entryStatements type of {@link StatementSet} initially of type {@link DefaultStatement}
	 * @param pl1Context context of type {@link Pl1ControlFlowContext}
	 */
	public Pl1ControlFlowResolver(final StatementSet<DefaultStatement> entryStatements, final Pl1ControlFlowContext pl1Context) {
		super(entryStatements, pl1Context);
	}

	/**
	 * Follows the control flow, links the AST nodes and returns the statements where the control flow ends.
	 *
	 * @param entryStatements the entry points of the Pl1 program.
	 * @param rootNode the root node
	 * @return the control flow result and the control flow edges
	 */
	public static Tuple2<ControlFlowResult, List<ControlFlowPrototype>> getControlFlow(final Set<AstNodePojo> entryStatements, final AstNodePojo rootNode) {
		final Pl1ControlFlowContext pl1Context = new Pl1ControlFlowContext(rootNode);
		final StatementSet<DefaultStatement> entryStatementSet = pl1Context.getLanguageSpecificHandler().createEmptyStatementSet();
		entryStatements.forEach(statement -> entryStatementSet.add(new DefaultStatement(statement)));
		final Pl1ControlFlowResolver resolver = new Pl1ControlFlowResolver(entryStatementSet, pl1Context);
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
