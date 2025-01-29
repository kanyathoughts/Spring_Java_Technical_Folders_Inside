/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.testing;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.mining.shared.Definable;
import innowake.mining.shared.KeyedSupplier;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AstNodeLocation;

/**
 * AST node for testing.
 */
public class AstNodePojoDummy extends AstNodePojoPrototype {
	
	private static final AstNodeLocation LOCATION_VOID = new AstNodeLocation(null, null);
	private static final KeyedSupplier<List<UUID>, List<AstNodePojo>> CHILDREN_EMPTY = new KeyedSupplier<>(Collections.emptyList(), Collections::emptyList);
	
	public final Definable<List<AstNodePojo>> children = new Definable<>(false, "AstNode.children");
	
	public AstNodePojoDummy prepare(final BuildingConsumer<AstNodePojoDummy> builder) {
		return builder.prepare(this);
	}
	
	public AstNodePojo build() {
		return build(Collections.emptyList());
	}
	
	public AstNodePojo build(final Collection<AstRelationshipPojo> withRelations) {
		return build(this, withRelations, children.optional());
	}
	
	public static AstNodePojo build(AstNodePojoPrototype proto) {
		return build(proto, Collections.emptyList(), Optional.empty());
	}
	
	public static AstNodePojo build(AstNodePojoPrototype proto,
			final Collection<AstRelationshipPojo> relations, final Optional<List<AstNodePojo>> children) {
		return new AstNodePojo(
				proto.id.orElseNonNull(UUID::randomUUID),
				proto.module.orElseNonNull(EntityId.VOID), null, null,
				proto.location.orElseNonNull(LOCATION_VOID), 
				proto.parent.optional().map(KeyedSupplier<UUID, AstNodePojo>::new).orElse(null),
				proto.includedModule.orElse(null), null, null,
				proto.sibling.orElse(null), null, null,
				children.map(c -> new KeyedSupplier<>(c.stream().map(AstNodePojo::getId).collect(Collectors.toList()), () -> c)).orElse(CHILDREN_EMPTY),
				relations, null, null,
				proto.type.orElse("DUMMY"),
				proto.superTypes.orElseNonNull(Collections::emptySet),
				proto.label.orElse("DUMMY"),
				proto.properties.orElseNonNull(Collections::emptyMap));
	}
	
}
