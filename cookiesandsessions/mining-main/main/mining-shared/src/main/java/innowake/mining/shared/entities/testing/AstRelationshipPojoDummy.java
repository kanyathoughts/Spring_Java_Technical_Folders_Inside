/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.testing;

import java.util.Collections;
import java.util.Map;
import java.util.UUID;

import innowake.mining.shared.KeyedSupplier;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * AST relationship for testing.
 */
public class AstRelationshipPojoDummy extends AstRelationshipPojoPrototype {
	
	public AstRelationshipPojoDummy prepare(final BuildingConsumer<AstRelationshipPojoDummy> builder) {
		return builder.prepare(this);
	}
	
	public AstRelationshipPojo build() {
		return build(this);
	}
	
	public AstRelationshipPojo build(final Map<UUID, AstNodePojo> ast) {
		return build(this, ast);
	}
	
	public static AstRelationshipPojo build(final AstRelationshipPojoPrototype proto) {
		return build(proto, Collections.emptyMap());
	}
	
	public static AstRelationshipPojo build(final AstRelationshipPojoPrototype proto, final Map<UUID, AstNodePojo> ast) {
		return new AstRelationshipPojo(
				proto.id.orElseNonNull(UUID::randomUUID),
				proto.src.optional().map(id -> new KeyedSupplier<>(id, () -> ast.get(id))).orElse(null),
				proto.dst.optional().map(id -> new KeyedSupplier<>(id, () -> ast.get(id))).orElse(null),
				proto.type.getNonNull(),
				proto.label.orElse(null));
	}
	
}
