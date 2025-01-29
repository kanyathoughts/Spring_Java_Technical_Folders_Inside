/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.shared.entities;

import java.util.UUID;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Base class for a Mining entity request with unique and/or numeric ID.
 * @param <P> parent type
 */
public abstract class MiningSequentialPojoPrototype<P extends MiningSequentialPojoPrototype<?>> extends MiningPojoPrototype<P> {

	public final Definable<Long> nid;

	protected MiningSequentialPojoPrototype(final String descriptionPrefix) {
		super(descriptionPrefix);
		nid = new Definable<>(false, descriptionPrefix + ".nid");
	}

	/**
	 * Sets the given {@code nid}.
	 *
	 * @param nid the numeric id
	 * @return this instance for method chaining
	 */
	public P setNid(final Long nid) {
		this.nid.set(nid);
		return self();
	}

	/**
	 * Sets the given {@code id}. If {@code id} is a {@link UUID} string, then {@code id} is converted to an {@link UUID} and set as the {@code uid}. Otherwise
	 * {@code id} is converted to a long and set as the {@code nid}.
	 *
	 * @param id an {@link UUID} string or a long
	 * @return this instance for method chaining
	 */
	@Schema(name = "id", ref = "EntityId") /* Prevents that swagger sets the type to P */
	public P setId(final String id) {
		if (id.length() == EntityId.UUID_STRING_LENGTH) {
			uid.set(UUID.fromString(id));
		} else {
			nid.set(Long.valueOf(id));
		}
		return self();
	}
	
	/**
	 * Applies the identifiers in the given {@code id}. If the {@code uid} or {@code nid} are not set in {@code id}, then the uid and nid
	 * of this pojo are unset.
	 *
	 * @param id the {@link EntityId} to apply
	 * @return this instance for method chaining
	 */
	@Override
	public P withId(final EntityId id) {
		if (id.hasUid()) {
			uid.set(id.getUid());
		} else {
			uid.unset();
		}
		if (id.hasNid()) {
			nid.set(id.getNid());
		} else {
			nid.unset();
		}
		return self();
	}
	
	/**
	 * Gets the ID of the entity.
	 * @return Wrapped identifier(s) of the entity.
	 */
	@Override
	public EntityId identityProvisional() {
		return EntityId.of(uid.orElse(null), nid.orElse(null));
	}

}
