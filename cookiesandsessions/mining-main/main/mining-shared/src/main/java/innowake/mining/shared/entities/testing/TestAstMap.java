/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.testing;

import java.util.HashMap;
import java.util.UUID;

import innowake.mining.shared.entities.ast.AstNodePojo;

/**
 * Holds the Nodes of a mocked or on-the-fly generated AST during testing.
 */
public class TestAstMap extends HashMap<UUID, AstNodePojo> {
	
	public AstNodePojo addNode(final AstNodePojo node) {
		put(node.getId(), node);
		return node;
	}
	
}
