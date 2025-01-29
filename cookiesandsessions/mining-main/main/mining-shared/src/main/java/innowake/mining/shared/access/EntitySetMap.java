/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

/**
 * Constitutes an {@link EntityMap} where ID references not just one but a Set of entities.
 * @param <T> Type of the entity.
 */
public class EntitySetMap<T> extends EntityMap<Set<T>> {
	
	/**
	 * Adds an entity to the set of entities mapped by a given ID.
	 * @param id ID(s) the entity is referenced by.
	 * @param entity Entity object.
	 * @return This EntitySetMap.
	 */
	public EntitySetMap<T> add(final EntityId id, final T entity) {
		if (id.hasUid()) {
			uids.computeIfAbsent(id.getUid(), k -> new HashSet<>()).add(entity);
		}
		if (id.hasNid()) {
			nids.computeIfAbsent(id.getNid(), k -> new HashSet<>()).add(entity);
		}
		return this;
	}
	
	/**
	 * Adds entities from a collection to the sets of entities in the map.
	 * @param entities Collection of entities to add.
	 * @param idSupplier Function providing one or more IDs for each entity.
	 * @return This EntitySetMap.
	 */
	public EntitySetMap<T> addAll(final Collection<T> entities, final Function<T, Collection<EntityId>> idSupplier) {
		entities.forEach(e -> idSupplier.apply(e).forEach(id -> add(id, e)));
		return this;
	}
	
}
