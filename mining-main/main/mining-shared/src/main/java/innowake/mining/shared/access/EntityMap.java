/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.UUID;
import java.util.function.Function;

/**
 * Constitutes a set of entities addressable through different IDs.
 * @param <T> Type of the entity.
 */
public class EntityMap<T> {
	
	protected final HashMap<UUID, T> uids = new HashMap<>();
	protected final TreeMap<Long, T> nids = new TreeMap<>();
	
	/**
	 * Adds an entity to the map.
	 * @param id ID(s) the entity is referenced by.
	 * @param entity Entity object.
	 * @return This EntityMap.
	 */
	public EntityMap<T> put(final EntityId id, final T entity) {
		if (id.hasUid()) {
			uids.put(id.getUid(), entity);
		}
		if (id.hasNid()) {
			nids.put(id.getNid(), entity);
		}
		return this;
	}
	
	/**
	 * Adds entities from a collection to the map.
	 * @param entities Collection of entities to add.
	 * @param idSupplier Function providing one or more IDs for each entity.
	 * @return This EntityMap.
	 */
	public EntityMap<T> putAll(final Collection<T> entities, final Function<T, Collection<EntityId>> idSupplier) {
		entities.forEach(e -> idSupplier.apply(e).forEach(id -> put(id, e)));
		return this;
	}
	
	/**
	 * Removes entities associated with any of the IDs wrapped in an {@link EntityId} from the map.
	 * @param id ID(s) to remove.
	 * @return This EntityMap.
	 */
	public EntityMap<T> remove(final EntityId id) {
		if (id.hasUid()) {
			uids.remove(id.getUid());
		}
		if (id.hasNid()) {
			nids.remove(id.getNid());
		}
		return this;
	}
	
	/**
	 * Retrieves an entity from the map using the IDs wrapped in an {@link EntityId}.
	 * @param id ID of the entity to retrieve.
	 * @return Entity found for the UID or, if not present the NID.
	 */
	public Optional<T> get(final EntityId id) {
		T value = null;
		if (id.hasUid()) {
			value = uids.get(id.getUid());
		}
		if (value == null && id.hasNid()) {
			value = nids.get(id.getNid());
		}
		return Optional.ofNullable(value);
	}
	
	/**
	 * Retrieves an entity by it's UID.
	 * @param uid UUID of the entity.
	 * @return Respective entity, if found.
	 */
	public Optional<T> get(final UUID uid) {
		return Optional.ofNullable(uids.get(uid));
	}
	
	/**
	 * Retrieves an entity by it's NID.
	 * @param nid Numeric ID of the entity.
	 * @return Respective entity, if found.
	 */
	public Optional<T> get(final Long nid) {
		return Optional.ofNullable(nids.get(nid));
	}
	
	/**
	 * Checks if this entity map is entirely empty, that is, has no IDs of any kind defined.
	 * @return If the map is empty.
	 */
	public boolean isEmpty() {
		return uids.isEmpty() && nids.isEmpty();
	}
	
	/**
	 * Checks if this map references any entities by UID.
	 * @return If the map contains any UUID keys.
	 */
	public boolean hasUids() {
		return ! uids.isEmpty();
	}
	
	/**
	 * Checks if this map references any entities by NID.
	 * @return If the map contains any Numeric ID keys.
	 */
	public boolean hasNids() {
		return ! nids.isEmpty();
	}
	
	/**
	 * Retrieves a read-only copy of the UID mappings.
	 * @return Map of UUIDs to entities.
	 */
	public Map<UUID, T> uidMap() {
		return Collections.unmodifiableMap(uids);
	}
	
	/**
	 * Retrieves a read-only copy of the NID mappings.
	 * @return Map of Numeric IDs to entities.
	 */
	public Map<Long, T> nidMap() {
		return Collections.unmodifiableMap(nids);
	}
	
}
