/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.ast;

import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.mining.shared.Definable;
import innowake.mining.shared.entities.PojoPrototype;

/**
 * {@code ast_relationship} entity class. Formerly know as {@code RefersTo}, {@code Redefines} and {@code FlowsControl} (AstNode ===> AstNode).
 */
public class AstRelationshipPojoPrototype implements PojoPrototype {

	public final Definable<UUID> id = new Definable<>(false, "AstRelationship.id");
	public final Definable<UUID> src = new Definable<>(false, "AstRelationship.src");
	public final Definable<UUID> dst = new Definable<>(false, "AstRelationship.dst");
	public final Definable<AstRelationshipType> type = new Definable<>(false, "AstRelationship.type");
	public final Definable<String> label = new Definable<>(true, "AstRelationship.label");

	public AstRelationshipPojoPrototype setSrc(final UUID src) {
		this.src.set(src);
		return this;
	}

	public AstRelationshipPojoPrototype setDst(final UUID dst) {
		this.dst.set(dst);
		return this;
	}

	public AstRelationshipPojoPrototype setType(final AstRelationshipType type) {
		this.type.set(type);
		return this;
	}

	public AstRelationshipPojoPrototype setLabel(final String label) {
		this.label.set(label);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id.orElse(null))
				.append("src", src.orElse(null))
				.append("dst", dst.orElse(null))
				.append("type", type.orElse(null))
				.append("label", label.orElse(null))
				.toString();
	}
}
