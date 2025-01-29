/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.natural;

import java.util.List;
import java.util.Set;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.controlflow.impl.AbstractControlFlowResolver;
import innowake.mining.data.core.controlflow.impl.ControlFlowResult;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.StatementSet;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Control flow resolver for Natural that contains the entry point to start a control flow calculation.
 */
public class NaturalControlFlowResolver extends AbstractControlFlowResolver<NaturalStatement, NaturalControlFlowContext> {

	/**
	 * Follows the control flow, links the AST nodes and returns the statements where the control flow ends.
	 *
	 * @param entryStatements the entry points of the Natural program.
	 * @param rootNode the root node
	 * @return the control flow result and the control flow edges
	 */
	public static Tuple2<ControlFlowResult, List<ControlFlowPrototype>> getControlFlow(final Set<AstNodePojo> entryStatements, final AstNodePojo rootNode) {
		final NaturalControlFlowContext naturalContext = new NaturalControlFlowContext(rootNode);
		final StatementSet<NaturalStatement> entryStatementSet = naturalContext.getLanguageSpecificHandler().createEmptyStatementSet();
		entryStatements.forEach(statement -> entryStatementSet.add(new NaturalStatement(statement)));
		final NaturalControlFlowResolver resolver = new NaturalControlFlowResolver(entryStatementSet, naturalContext);
		final ControlFlowSubResult<?> subResult = resolver.createControlFlow();
		final ControlFlowResult finalResult = resolver.context.getFinalResult();
		subResult.getLastSimpleStatements().forEach(statement -> finalResult.addReturnStatement(statement.getKey().getAstNode()));
		return Tuple2.of(finalResult, resolver.context.getControlFlowEdges());
	}

	private NaturalControlFlowResolver(final StatementSet<NaturalStatement> entryStatements, final NaturalControlFlowContext context) {
		super(entryStatements, context);
	}

}
