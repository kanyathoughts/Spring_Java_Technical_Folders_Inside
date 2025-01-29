/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.ast;

import innowake.mining.shared.Definable;

/**
 * AST relationship prototype with AST nodes.
 */
public class ControlFlowPrototype extends AstRelationshipPojoPrototype {
	
	public final Definable<AstNodePojo> srcNode = new Definable<AstNodePojo>(false, "AstRelationship.srcNode").overrides(src, d -> d.getNonNull().getId());
	public final Definable<AstNodePojo> dstNode = new Definable<AstNodePojo>(false, "AstRelationship.dstNode").overrides(dst, d -> d.getNonNull().getId());
	
	public ControlFlowPrototype() {
		final var typeFlow = new Definable<AstRelationshipType>(false, "AstRelationship.type.FLOW").overrides(type, Definable::getNonNull);
		typeFlow.set(AstRelationshipType.FLOW);
	}
	
	public ControlFlowPrototype setSrc(final AstNodePojo src) {
		this.srcNode.set(src);
		return this;
	}
	
	public ControlFlowPrototype setDst(final AstNodePojo dst) {
		this.dstNode.set(dst);
		return this;
	}
	
}
