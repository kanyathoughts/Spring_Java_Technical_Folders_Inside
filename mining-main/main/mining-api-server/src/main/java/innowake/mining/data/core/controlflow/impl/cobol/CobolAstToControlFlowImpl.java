/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.cobol;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.CobolAstNodeType;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.impl.AbstractAstToControlFlowImpl;
import innowake.mining.data.core.controlflow.impl.ControlFlowResult;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Traverses AST nodes and calculates control flow,
 */
public final class CobolAstToControlFlowImpl extends AbstractAstToControlFlowImpl {
	
	Optional<CobolJumpStatementStorage> jumpStatementStorage = Optional.empty();
	
	/**
	 * Constructor.
	 * @param rootNode the root node of the AST
	 */
	public CobolAstToControlFlowImpl(final AstNodePojo rootNode) {
		super(rootNode);
		final Optional<AstNodePojo> procedureDivision = new AstNodeCollector(n -> "ProcedureDivision".equals(n.getType())).first(rootNode);
		if (procedureDivision.isPresent()) {
			addEntryPoints(procedureDivision.get());
			jumpStatementStorage = Optional.of(new CobolJumpStatementStorage(procedureDivision.get()));
		}
	}
	
	@Override
	protected void addEntryPoints(final AstNodePojo procedureDivisionNode) {
		final Optional<AstNodePojo> firstEntry = AstNodeUtils.getFirstChild(procedureDivisionNode, AstNodeUtils.STATEMENT);
		if (firstEntry.isPresent()) {
			addEntryPoint(firstEntry.get());
		}
		super.addEntryPoints(procedureDivisionNode);
	}
	
	@Override
	protected void addEntryPoint(AstNodePojo entryNode) {
		if (CobolAstNodeType.SIZE_GUARDED.getType().equals(entryNode.getType())) {
			final var child = AstNodeUtils.getFirstChild(entryNode, AstNodeUtils.STATEMENT);
			if (child.isPresent()) {
				entryNode = child.get();
			}
		}
		super.addEntryPoint(entryNode);
	}
	
	@Override
	protected Tuple2<ControlFlowResult, List<ControlFlowPrototype>> resolveControlFlow(Set<AstNodePojo> entries, AstNodePojo rootNode) {
		return ProcedureDivisionResolver.getControlFlow(entries, jumpStatementStorage.get());
	}
	
}
