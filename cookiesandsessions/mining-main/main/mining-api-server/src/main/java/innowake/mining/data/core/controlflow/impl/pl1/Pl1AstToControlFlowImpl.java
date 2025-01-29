/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl.pl1;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeCollector;
import innowake.mining.data.core.controlflow.impl.AbstractAstToControlFlowImpl;
import innowake.mining.data.core.controlflow.impl.ControlFlowResult;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;
import innowake.ndt.pl1parser.ast.model.statement.PackageStatement;
import innowake.ndt.pl1parser.ast.model.statement.block.ProcedureBlock;

/**
 * Traverses AST nodes and calculates control flow for Pl/I program.
 */
public final class Pl1AstToControlFlowImpl extends AbstractAstToControlFlowImpl {

	private static final String PROCEDURE_BLOCK = "ProcedureBlock";
	/**
	 * Constructor to instantiate {@link Pl1AstToControlFlowImpl}.
	 * 
	 * @param rootNode the root node of the AST
	 */
	public Pl1AstToControlFlowImpl(final AstNodePojo rootNode) {
		super(rootNode);
	}

	@Override
	protected void addEntryPoints(final AstNodePojo entryNode) {
		final Pl1SpecificHandler pl1SpecificHandler = new Pl1SpecificHandler();
		final Optional<AstNodePojo> packageBlocks =
				new AstNodeCollector(n -> n.getType().equals("PackageBlock") && pl1SpecificHandler.isControlflowRelevantStatement(n)).firstDeep(entryNode);
		if (packageBlocks.isPresent()) {
			final Optional<AstNodePojo> packageStatement = new AstNodeCollector(n -> n.getType().equals("PackageStatement")).firstDeep(entryNode);
			if (packageStatement.isPresent()) {
				List<?> targetNames =
						(List<?>) packageStatement.get().getProperties().getOrDefault(PackageStatement.TARGET_PROCEDURES_NAMES, Collections.emptyList());
				if (targetNames.isEmpty()) {
					packageBlocks.get().getChildren().stream()
					.filter(node -> node.getType().equals(PROCEDURE_BLOCK))
					.forEach(this::addEntryPoint);
				} else {
					packageBlocks.get().getChildren().stream()
							.filter(node -> node.getType().equals(PROCEDURE_BLOCK) && targetNames.contains(node.getProperties().get(ProcedureBlock.PROCEDURE_NAME)))
							.forEach(this::addEntryPoint);
				}
			} else {
				packageBlocks.get().getChildren().stream()
					.filter(node -> node.getType().equals(PROCEDURE_BLOCK))
					.forEach(this::addEntryPoint);
			}
		} else {
			entryNode.getChildren().stream()
				.filter(node -> node.getType().equals(PROCEDURE_BLOCK))
				.forEach(this::addEntryPoint);
		}
	}

	@Override
	protected Tuple2<ControlFlowResult, List<ControlFlowPrototype>> resolveControlFlow(Set<AstNodePojo> entries, AstNodePojo rootNode) {
		return Pl1ControlFlowResolver.getControlFlow(entries, rootNode);
	}

}
