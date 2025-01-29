/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.shared.entities;

import java.util.Map;
import java.util.UUID;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;

/**
 * Base class for a Mining entity request with unique ID.
 * @param <P> parent type
 */
public abstract class MiningPojoPrototype<P extends MiningPojoPrototype<?>> implements PojoPrototype {
	
	public final Definable<UUID> uid;
	public final Definable<Map<String, Object>> customProperties;
	
	protected MiningPojoPrototype(final String descriptionPrefix) {
		uid = new Definable<>(false, descriptionPrefix + ".uid");
		customProperties = new Definable<>(false, descriptionPrefix + ".customProperties");
	}
	
	public P setUid(final UUID uid) {
		this.uid.set(uid);
		return self();
	}
	
	public P setCustomProperties(final Map<String, Object> customProperties) {
		this.customProperties.set(customProperties);
		return self();
	}
	
	@SuppressWarnings("unchecked")
	protected P self() {
		return (P) this;
	}
	
	public P withId(final EntityId id) {
		uid.set(id.getUid());
		return self();
	}
	
	/**
	 * Gets the ID of the entity.
	 * Note that a prototype Pojo may not have all or any IDs defined and it may not have been stored in the database yet.
	 * @return Wrapped identifier(s) of the entity.
	 */
	public EntityId identityProvisional() {
		if (! uid.isDefined()) {
			return EntityId.VOID;
		}
		return EntityId.of(uid.get(), null);
	}
	
}
