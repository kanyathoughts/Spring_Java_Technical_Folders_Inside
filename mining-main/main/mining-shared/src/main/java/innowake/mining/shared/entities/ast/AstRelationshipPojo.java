/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.ast;

import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.KeyedSupplier;

/**
 * {@code ast_relationship} entity class. Formerly know as {@code RefersTo}, {@code Redefines} and {@code FlowsControl} (AstNode ===> AstNode).
 */
public final class AstRelationshipPojo {

	private final UUID id;
	private final KeyedSupplier<UUID, AstNodePojo> src;
	private final KeyedSupplier<UUID, AstNodePojo> dst;
	private final AstRelationshipType type;
	@Nullable
	private final String label;

	@JsonCreator
	public AstRelationshipPojo(
			@JsonProperty("id") final UUID id,
			@JsonProperty("src") final KeyedSupplier<UUID, AstNodePojo> src,
			@JsonProperty("dst") final KeyedSupplier<UUID, AstNodePojo> dst,
			@JsonProperty("type") final AstRelationshipType type,
			@JsonProperty("label") final String label) {
		this.id = id;
		this.src = src;
		this.dst = dst;
		this.type = type;
		this.label = label;
	}

	public UUID getId() {
		return id;
	}

	public UUID getSrc() {
		return src.getKey();
	}
	
	@JsonIgnore
	public AstNodePojo getSrcNode() {
		return src.get();
	}

	public UUID getDst() {
		return dst.getKey();
	}
	
	@JsonIgnore
	public AstNodePojo getDstNode() {
		return dst.get();
	}

	public AstRelationshipType getType() {
		return type;
	}

	public Optional<String> getLabel() {
		return label != null ? Optional.of(label) : Optional.empty();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("src", src)
				.append("dst", dst)
				.append("type", type)
				.append("label", label)
				.toString();
	}
}