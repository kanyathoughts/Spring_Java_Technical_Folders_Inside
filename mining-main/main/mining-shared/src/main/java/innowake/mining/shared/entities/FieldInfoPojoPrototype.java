/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;

/**
 * {@code field_info} entity request class.
 */
public class FieldInfoPojoPrototype implements PojoPrototype {

	public final Definable<UUID> id = new Definable<>(false, "FieldInfo.id");
	public final Definable<EntityId> module = new Definable<>(false, "FieldInfo.module");
	public final Definable<String> name = new Definable<>(false, "FieldInfo.name");
	public final Definable<Integer> ordinal = new Definable<>(false, "FieldInfo.ordinal");
	public final Definable<String> reference = new Definable<>(true, "FieldInfo.reference");
	public final Definable<String> comment = new Definable<>(true, "FieldInfo.comment");
	public final Definable<Map<String, Object>> properties = new Definable<>(true, "FieldInfo.properties");

	public FieldInfoPojoPrototype setId(final UUID id) {
		this.id.set(id);
		return this;
	}

	public FieldInfoPojoPrototype setModule(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}

	public FieldInfoPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}

	public FieldInfoPojoPrototype setOrdinal(final int ordinal) {
		this.ordinal.set(ordinal);
		return this;
	}

	public FieldInfoPojoPrototype setReference(@Nullable final String reference) {
		this.reference.set(reference);
		return this;
	}

	public FieldInfoPojoPrototype setComment(@Nullable final String comment) {
		this.comment.set(comment);
		return this;
	}

	public FieldInfoPojoPrototype setProperties(@Nullable final Map<String, Object> properties) {
		this.properties.set(properties);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("module", module.orElse(null))
				.append("name", name.orElse(null))
				.append("ordinal", ordinal.orElse(null))
				.append("reference", reference.orElse(null))
				.append("comment", comment.orElse(null))
				.append("properties", properties.orElse(null))
				.toString();
	}
}