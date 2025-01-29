/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.jcl;

import java.util.List;
import java.util.Set;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.controlflow.impl.AbstractControlFlowResolver;
import innowake.mining.data.core.controlflow.impl.ControlFlowResult;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.data.core.controlflow.impl.StatementSet;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Control flow resolver for JCL that contains the entry point to start a control flow calculation.
 */
public class JclControlFlowResolver extends AbstractControlFlowResolver<DefaultStatement, JclControlFlowContext> {

	/**
	 * Constructor to instantiate {@link JclControlFlowResolver}
	 * 
	 * @param context of type {@link JclControlFlowContext}
	 */
	protected JclControlFlowResolver(final JclControlFlowContext context) {
		super(context);
	}

	/**
	 * Constructor to instantiate {@link JclControlFlowResolver}.
	 * 
	 * @param entryStatements type of {@link StatementSet} initially of type {@link DefaultStatement}
	 * @param jclContext context of type {@link JclControlFlowContext}
	 */
	public JclControlFlowResolver(final StatementSet<DefaultStatement> entryStatements, final JclControlFlowContext jclContext) {
		super(entryStatements, jclContext);
	}

	/**
	 * Follows the control flow, links the {@link AstNodePojo}s and returns the statements where the control flow ends.
	 *
	 * @param entryStatements the entry points of the JCL program.
	 * @param rootNode the root node
	 * @return the control flow result and the control flow edges
	 */
	public static Tuple2<ControlFlowResult, List<ControlFlowPrototype>> getControlFlow(final Set<AstNodePojo> entryStatements, final AstNodePojo rootNode) {
		final JclControlFlowContext jclContext = new JclControlFlowContext(rootNode);
		final StatementSet<DefaultStatement> entryStatementSet = jclContext.getLanguageSpecificHandler().createEmptyStatementSet();
		entryStatements.forEach(statement -> entryStatementSet.add(new DefaultStatement(statement)));
		final JclControlFlowResolver resolver = new JclControlFlowResolver(entryStatementSet, jclContext);
		final ControlFlowSubResult<DefaultStatement> subResult = resolver.createControlFlow();
		final ControlFlowResult finalResult = resolver.context.getFinalResult();
		subResult.getLastSimpleStatements().forEach(statement -> finalResult.addReturnStatement(statement.getKey().getAstNode()));
		return Tuple2.of(finalResult, resolver.context.getControlFlowEdges());
	}

}
