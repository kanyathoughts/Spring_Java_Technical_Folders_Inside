/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.List;
import java.util.Set;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.controlflow.impl.ControlFlowResult;
import innowake.mining.data.core.controlflow.impl.ControlFlowSubResult;
import innowake.mining.data.core.controlflow.impl.DefaultStatement;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Determines the control flow of the procedure division of a program.
 */
public class ProcedureDivisionResolver extends AbstractCobolControlFlowResolver {
	
	private ProcedureDivisionResolver(final Set<AstNodePojo> entryStatements, final CobolJumpStatementStorage jumpStatementStorage) {
		super(entryStatements, new CobolControlFlowContext(jumpStatementStorage));
	}
	
	/**
	 * Follows the control flow, links the {@link AstNodePojo}s and returns the statements where the control flow ends.
	 *
	 * @param entryStatements the entry points of the procedure division.
	 * @param jumpStatementStorage contains all jump statements together with their targets
	 * @return the control flow edges
	 */
	public static Tuple2<ControlFlowResult, List<ControlFlowPrototype>> getControlFlow(final Set<AstNodePojo> entryStatements, final CobolJumpStatementStorage jumpStatementStorage) {
		final ProcedureDivisionResolver resolver = new ProcedureDivisionResolver(entryStatements, jumpStatementStorage);
		final ControlFlowSubResult<DefaultStatement> subResult = resolver.createControlFlow();
		final ControlFlowResult finalResult = resolver.context.getFinalResult();
		subResult.getLastSimpleStatements().forEach(statement -> finalResult.addReturnStatement(statement.getKey().getAstNode()));
		return Tuple2.of(finalResult, resolver.context.getControlFlowEdges());
	}
	
}
