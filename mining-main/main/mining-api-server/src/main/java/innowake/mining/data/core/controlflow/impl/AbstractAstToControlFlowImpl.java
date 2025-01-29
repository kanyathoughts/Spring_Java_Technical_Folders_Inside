/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.data.core.controlflow.api.AstToControlFlow;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.ControlFlowPrototype;

/**
 * Base implementation to traverses AST nodes and calculates control flow.
 */
public abstract class AbstractAstToControlFlowImpl implements AstToControlFlow {

	protected final Set<AstNodePojo> entries = new HashSet<>();
	protected final AstNodePojo rootNode;
	
	private final ArrayList<ControlFlowPrototype> flow = new ArrayList<>();
	private final ArrayList<AstModuleRelationshipPojoPrototype> terminals = new ArrayList<>();

	/**
	 * Constructor.
	 * 
	 * @param rootNode the root node of the AST
	 */
	protected AbstractAstToControlFlowImpl(final AstNodePojo rootNode) {
		this.rootNode = rootNode;
		addEntryPoints(rootNode);
	}

	@Override
	public void calculateControlFlow() {
		if (entries.isEmpty()) {
			/* If a program without any relevant statements is called it returns immediately */
			terminals.add(new AstModuleRelationshipPojoPrototype()
					.setModule(rootNode.getModule()).setNode(rootNode.getId()).setType(AstModuleRelationshipType.ENTRY));
			terminals.add(new AstModuleRelationshipPojoPrototype()
					.setModule(rootNode.getModule()).setNode(rootNode.getId()).setType(AstModuleRelationshipType.RETURN));
		} else {
			for (final var entryNode : entries) {
				terminals.add(new AstModuleRelationshipPojoPrototype()
						.setModule(rootNode.getModule()).setNode(entryNode.getId()).setType(AstModuleRelationshipType.ENTRY));
			}
			final Tuple2<ControlFlowResult, List<ControlFlowPrototype>> result = resolveControlFlow(entries, rootNode);
			final ControlFlowResult controlFlowResult = result.a;
			if (controlFlowResult.getErrorStatements().isEmpty()) {
				flow.addAll(result.b);
				controlFlowResult.getReturnStatements().forEach(s -> terminals.add(new AstModuleRelationshipPojoPrototype()
					.setModule(rootNode.getModule()).setNode(s.getId()).setType(AstModuleRelationshipType.RETURN)));
				controlFlowResult.getHaltStatements().forEach(s -> terminals.add(new AstModuleRelationshipPojoPrototype()
					.setModule(rootNode.getModule()).setNode(s.getId()).setType(AstModuleRelationshipType.HALT)));
			} else {
				final String errorMsg = controlFlowResult.getErrorStatements().stream().map(error -> error.b).distinct().collect(Collectors.joining(" / "));
				throw new IllegalArgumentException(errorMsg);
			}
		}
	}
	
	protected abstract Tuple2<ControlFlowResult, List<ControlFlowPrototype>> resolveControlFlow(final Set<AstNodePojo> entries, final AstNodePojo rootNode);
	
	protected void addEntryPoint(final AstNodePojo entryNode) {
		entries.add(entryNode);
	}
	
	protected void addEntryPoints(final AstNodePojo entryNode) {
		AstNodeUtils.getChildrenDeep(entryNode, AstNodeUtils.ENTRY).forEach(this::addEntryPoint);
	}
	
	protected void addControlFlowEdge(final AstNodePojo src, final AstNodePojo dst) {
		flow.add(new ControlFlowPrototype().setSrc(src).setDst(dst));
	}

	@Override
	public Collection<ControlFlowPrototype> getFlow() {
		return Collections.unmodifiableList(flow);
	}

	@Override
	public Collection<AstModuleRelationshipPojoPrototype> getTerminals() {
		return Collections.unmodifiableList(terminals);
	}

}
