/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.ast;

import innowake.mining.shared.Definable;

/**
 * AST Node relationship referencing nodes which have not yet been persisted.
 */
public class StoreAstRelationship extends AstRelationshipPojoPrototype {
	
	public final Definable<AstNodePojoPrototype> srcNode = new Definable<AstNodePojoPrototype>(false, "AstRelationship.srcNode").overrides(src, d -> d.getNonNull().id.get());
	public final Definable<AstNodePojoPrototype> dstNode = new Definable<AstNodePojoPrototype>(false, "AstRelationship.dstNode").overrides(dst, d -> d.getNonNull().id.get());
	
	public StoreAstRelationship setSrc(final AstNodePojoPrototype src) {
		this.srcNode.set(src);
		return this;
	}
	
	public StoreAstRelationship setDst(final AstNodePojoPrototype dst) {
		this.dstNode.set(dst);
		return this;
	}
	
}
