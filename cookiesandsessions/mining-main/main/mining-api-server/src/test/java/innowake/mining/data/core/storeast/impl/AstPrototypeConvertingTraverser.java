/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.AbstractTraverser;
import innowake.mining.shared.KeyedSupplier;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.StoreAstPrototype;

/**
 * Creates AST Pojos from a Store-AST result.
 */
public class AstPrototypeConvertingTraverser extends AbstractTraverser<StoreAstPrototype, AstNodePojo> {

	@Nullable
	private UUID root;
	
	private Set<StoreAstPrototype> visited;
	private Map<UUID, AstNodePojo> elements;
	private Map<UUID, List<AstRelationshipPojo>> relationsIn;
	private Map<UUID, List<AstRelationshipPojo>> relationsOut;

	public AstPrototypeConvertingTraverser() {
		super(StoreAstPrototype::children);
		visited = new HashSet<>();
		elements = new HashMap<>();
		relationsIn = new HashMap<>();
		relationsOut = new HashMap<>();
	}
	
	public void convert(final StoreAstPrototype root) {
		traverse(root);
		this.root = root.id.getNonNull();
		for (final var node : visited) {
			for (final var proto : node.getRelationships()) {
				final var rel = new AstRelationshipPojo(UUID.randomUUID(),
						supply(proto.src.getNonNull()), supply(proto.dst.getNonNull()),
						proto.type.getNonNull(), proto.label.orElse(null));
				relationsIn.computeIfAbsent(rel.getDst(), k -> new ArrayList<>()).add(rel);
				relationsOut.computeIfAbsent(rel.getSrc(), k -> new ArrayList<>()).add(rel);
			}
		}
	}
	
	public AstNodePojo get(@Nullable final UUID id) {
		if (id == null) {
			return elements.get(root);
		}
		return elements.get(id);
	}
	
	private KeyedSupplier<UUID, AstNodePojo> supply(final UUID id) {
		return new KeyedSupplier<>(id, () -> elements.get(id));
	}
	
	private KeyedSupplier<List<UUID>, List<AstNodePojo>> supply(final List<UUID> ids) {
		return new KeyedSupplier<>(ids, () -> ids.stream().map(elements::get).collect(Collectors.toList()));
	}
	
	@Override
	protected AstNodePojo visit(final StoreAstPrototype element) {
		final var children = element.children().stream().map(p -> p.id.getOrSet(UUID::randomUUID)).collect(Collectors.toList());
		final var id = element.id.getOrSet(UUID::randomUUID);
		final var node = new AstNodePojo(id,
				element.module.orElse(EntityId.VOID), null, null,
				element.location.getNonNull(),
				element.parent.optional().map(this::supply).orElse(null),
				element.includedModule.orElse(null), null, null,
				element.sibling.orElse(null),
				element.parent.optional().map(elements::get).flatMap(parent -> element.sibling.optional().map(n -> n + 1)
					.filter(n -> n < parent.getChildIds().size()).map(n -> parent.getChildIds().get(n))).map(this::supply).orElse(null),
				element.parent.optional().map(elements::get).flatMap(parent -> element.sibling.optional().filter(n -> n > 0)
					.map(n -> parent.getChildIds().get(n - 1))).map(this::supply).orElse(null),
				supply(children),
				null,
				relationsIn.computeIfAbsent(id, k -> new ArrayList<>()),
				relationsOut.computeIfAbsent(id, k -> new ArrayList<>()),
				element.type.getNonNull(),
				element.superTypes.optional().map(s -> (Set<String>) new HashSet<>(s)).orElseGet(Collections::emptySet),
				element.label.getNonNull(),
				element.properties.orElseNonNull(Collections::emptyMap));
		elements.put(node.getId(), node);
		visited.add(element);
		return node;
	}

}
