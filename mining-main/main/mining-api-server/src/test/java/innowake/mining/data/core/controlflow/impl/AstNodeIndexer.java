/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.controlflow.impl;

import java.util.HashMap;
import java.util.Map;

import innowake.mining.data.core.api.AbstractTraverser;
import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * A traverser for creating IDs for AST Nodes.
 */
public class AstNodeIndexer extends AbstractTraverser<AstNodePojo, Map<AstNodePojo, Long>> {

	private final Map<AstNodePojo, Long> indexes = new HashMap<>();
	
	private long count = 0;
	
	/**
	 * Constructor.
	 */
	public AstNodeIndexer() {
		super(node -> node.getChildren());
	}
	
	@Override
	protected Map<AstNodePojo, Long> visit(final AstNodePojo node) {
		if ( ! indexes.containsKey(node)) {
			indexes.put(node, Long.valueOf(++count));
		}
		return indexes;
	}
}
