/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.io.Serializable;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.LongConsumer;
import java.util.function.LongFunction;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;

/**
 * Wraps different identifiers of the same Entity.
 */
public class EntityId implements Serializable {
	
	/** Unspecified Entity ID **/
	public static final EntityId VOID = new EntityId((UUID) null, null);
	
	/** Length of a {@link UUID} in string form */
	public static final int UUID_STRING_LENGTH = 36; 
	
	@Nullable
	private final UUID uid;
	@Nullable
	private final Long nid;
	
	protected EntityId(@Nullable final UUID uid, @Nullable final Long nid) {
		this.uid = uid;
		this.nid = nid;
	}
	
	public static Optional<EntityId> optional(@Nullable final UUID uid, @Nullable final Long nid) {
		if (uid != null || nid != null) {
			return Optional.of(new EntityId(uid, nid));
		}
		return Optional.empty();
	}
	
	/**
	 * Creates a new Identity.
	 * @param uid Unique ID.
	 * @param nid Numeric ID.
	 * @return New Identity or {@code EntityId.VOID} if both IDs are {@code null}.
	 */
	public static EntityId of(@Nullable final UUID uid, @Nullable final Long nid) {
		return optional(uid, nid).orElse(VOID);
	}
	
	/**
	 * Creates a new Identity or returns {@code null} in case both IDs are {@code null}.
	 * @param uid Unique ID.
	 * @param nid Numeric ID.
	 * @return New Identity or {@code null} if both IDs are {@code null}.
	 */
	@Nullable
	public static EntityId orNull(@Nullable final UUID uid, @Nullable final Long nid) {
		return optional(uid, nid).orElse(null);
	}
	
	@JsonCreator
	public static EntityId of(
			@JsonProperty("uid") @Nullable final String uid,
			@JsonProperty("nid") @Nullable final Long nid) {
		return of(uid != null ? UUID.fromString(uid) : null, nid);
	}
	
	/**
	 * Creates an identity with unique identifier.
	 * @param uid Unique ID.
	 * @return Wrapped identifier.
	 */
	public static EntityId of(final UUID uid) {
		return of(uid, null);
	}
	
	/**
	 * Creates an identity with numeric identifier.
	 * @param nid Numeric ID.
	 * @return Wrapped identifier.
	 */
	@JsonCreator
	public static EntityId of(final Long nid) {
		return of((UUID) null, nid);
	}
	
	/**
	 * Creates an identity from a String.
	 * @param id Either a UUID or numeric ID.
	 * @return Wrapped identifier.
	 */
	@JsonCreator
	public static EntityId of(final String id) {
		if (id.length() == UUID_STRING_LENGTH) {
			return of(UUID.fromString(id));
		} else {
			return of(Long.valueOf(id));
		}
	}
	
	/**
	 * Determines if the unique ID is available.
	 * @return Availability of the unique ID.
	 */
	public boolean hasUid() {
		return uid != null;
	}
	
	/**
	 * Gets the unique identifier of the Entity.
	 * @return Unique ID.
	 * @throws NoSuchElementException If no unique identifier is defined.
	 */
	public UUID getUid() {
		if (uid != null) {
			return uid;
		}
		throw new NoSuchElementException("Entity not referenced through unique identifier: " + this);
	}

	/**
	 * Gets an Optional of the unique identifier of the Entity.
	 * @return Optional Unique ID.
	 */
	@JsonProperty("uid")
	public Optional<UUID> getUidOptional() {
		return Optional.ofNullable(uid);
	}
	
	/**
	 * Determines if the numeric ID is available.
	 * @return Availability of the numeric ID.
	 */
	public boolean hasNid() {
		return nid != null;
	}
	
	/**
	 * Gets the numeric identifier of the Entity.
	 * @return Numeric ID.
	 * @throws NoSuchElementException If no numeric identifier is defined.
	 */
	public Long getNid() {
		if (nid != null) {
			return nid;
		}
		throw new NoSuchElementException("Entity not referenced through numeric identifier");
	}

	/**
	 * Gets an Optional of the numeric identifier of the Entity.
	 * @return Optional Numeric ID.
	 */
	@JsonProperty("nid")
	public Optional<Long> getNidOptional() {
		return Optional.ofNullable(nid);
	}
	
	/**
	 * Gets the identifier of the Entity.
	 * @return UUID or numeric ID, depending on which is defined.
	 */
	@Nullable
	public Object value() {
		return hasUid() ? uid : nid;
	}
	
	/**
	 * Extracts the Unique IDs from a Collection of identities.
	 * @param ids IDs to include.
	 * @return Available Unique IDs.
	 */
	public static Set<UUID> allUids(final Collection<EntityId> ids) {
		return ids.stream().filter(EntityId::hasUid).map(EntityId::getUid).collect(Collectors.toSet());
	}
	
	/**
	 * Extracts the Numeric IDs from a Collection of identities.
	 * @param ids IDs to include.
	 * @return Available Numeric IDs.
	 */
	public static Set<Long> allNids(final Collection<EntityId> ids) {
		return ids.stream().filter(EntityId::hasNid).map(EntityId::getNid).collect(Collectors.toSet());
	}
	
	/**
	 * @return {@code true} if either the {@code uid}, {@code nid} or both are present, otherwise {@code false}.
	 */
	@JsonIgnore
	public boolean isEmpty() {
		return this == VOID || uid == null && nid == null;
	}

	/**
	 * Acquires a result by different means deepening on whether the supplied ID is a UUID or numeric.
	 * @param <T> Result type.
	 * @param ifUUID Function to perform for a UUID.
	 * @param ifLong Function to perform for a numeric ID.
	 * @return Result.
	 */
	public <T> T apply(final Function<UUID, T> ifUUID, final LongFunction<T> ifLong) {
		return apply(ifUUID, ifLong, null);
	}
	
	/**
	 * Acquires a result by different means deepening on whether the supplied ID is a UUID or numeric.
	 * @param <T> Result type.
	 * @param ifUUID Function to perform for a UUID.
	 * @param ifLong Function to perform for a numeric ID.
	 * @param orElse Supplier of result if the ID is undefined (void).
	 * @return Result.
	 */
	public <T> T apply(final Function<UUID, T> ifUUID, final LongFunction<T> ifLong, @Nullable final Supplier<T> orElse) {
		if (hasUid()) {
			return ifUUID.apply(uid);
		} else if (nid != null) {
			return ifLong.apply(nid.longValue());
		} else if (orElse != null) {
			return orElse.get();
		}
		throw new NullPointerException("Entity has no identifier");
	}
	
	/**
	 * Performs a different action deepening on whether the supplied ID is a UUID or numeric.
	 * @param ifUUID Action to perform for a UUID.
	 * @param ifLong Action to perform for a numeric ID.
	 */
	public void perform(final Consumer<UUID> ifUUID, final LongConsumer ifLong) {
		if (hasUid()) {
			ifUUID.accept(uid);
		} else if (nid != null) {
			ifLong.accept(nid.longValue());
		} else {
			throw new NullPointerException("Entity has no identifier");
		}
	}

	/**
	 * Returns {@code true} if the given collection {@code ids} contains the specified {@code key}. For better performance {@code ids} should be a {@link Set}.
	 *
	 * @param ids collection of {@link EntityId EntityIds} to search
	 * @param key the {@link EntityId} to find
	 * @return {@code true} if {@code ids} contains {@code key}. Otherwise {@code false}
	 */
	public static boolean contains(final Collection<EntityId> ids, final EntityId key) {
		if (ids.contains(key)) {
			return true;
		}

		return key.hasNid() && ids.stream().anyMatch(id -> id.hasNid() && id.getNid().equals(key.getNid()));
	}

	@Override
	public int hashCode() {
		if (uid != null) {
			return uid.hashCode();
		} else if (nid != null) {
			return nid.hashCode();
		}
		return super.hashCode();
	}
	
	@Override
	public boolean equals(@Nullable final Object obj) {
		if (super.equals(obj)) {
			return true;
		}

		if (obj instanceof EntityId) {
			EntityId other = (EntityId) obj;
			final boolean nullUID = uid == null || other.uid == null;
			final boolean nullNID = nid == null || other.nid == null;
			return (! (nullNID && nullUID) 
						|| uid == null && nid == null && other.uid == null && other.nid == null)
					&& (nullUID || Objects.equals(uid, other.uid)) 
					&& (nullNID || Objects.equals(nid, other.nid));
		} else if (uid != null && obj instanceof UUID) {
			return uid.equals(obj);
		} else if (nid != null && obj instanceof Long) {
			return nid.equals(obj);
		}

		return false;
	}
	
	/**
	 * Compares this ID specification to another one and applies this one to a Consumer
	 * if the other is void (see {@link #VOID}) or throws an Exception if this and the other ID do not match.
	 * @param other ID to compare this one to.
	 * @param target Consumer to accept this ID in case the one to compare is void.
	 * @throws IllegalArgumentException In case the ID to compare is not void and does not match this one.
	 */
	public void matchOrApply(final EntityId other, final Consumer<EntityId> target) {
		if (! (other.hasUid() || other.hasNid())) {
			target.accept(this);
		} else if (! this.equals(other)) {
			throw new IllegalArgumentException("Identity of the entity " + other.toString()
				+ " does not correspond to the identity in the request " + this.toString());
		}
	}
	
	/**
	 * Merges UID or NID of an other EntityId with this EntityId
	 * with the values of the other taking precedence. 
	 * @param other EntityId to merge.
	 * @return The other, if it defines both IDs, or this, if the other defines no different value for either ID, or a combined EntityId.
	 */
	public EntityId merge(final EntityId other) {
		if (this == VOID || other.hasUid() && other.hasNid()) {
			return other;
		}
		if (other.hasUid() && ! Objects.equals(other.uid, this.uid)) {
			return new EntityId(other.uid, this.nid);
		}
		if (other.hasNid() && ! Objects.equals(other.nid, this.nid)) {
			return new EntityId(this.uid, other.nid);
		}
		return this;
	}
	
	@Override
	public String toString() {
		return "[uid=" + uid + ",nid=" + nid + "]";
	}
	
}
