/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.ast;

import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.PojoPrototype;

/**
 * {@code ast_module_relationship} entity class. Formerly know as {@code FlowsControl} edges from {@code Modules} to {@code EntryPoints}, {@code HaltPoint} to 
 * {@code AstNode} and {@code ReturnPoint} to {@code AstNode}.
 */
public final class AstModuleRelationshipPojoPrototype implements PojoPrototype {

	public final Definable<UUID> node = new Definable<>(false, "AstModuleRelationship.node");
	public final Definable<AstModuleRelationshipType> type = new Definable<>(false, "AstModuleRelationship.type");
	public final Definable<EntityId> module = new Definable<>(false, "AstModuleRelationship.module");

	@JsonAlias("node")
	public AstModuleRelationshipPojoPrototype setNode(final UUID node) {
		this.node.set(node);
		return this;
	}

	public AstModuleRelationshipPojoPrototype setType(final AstModuleRelationshipType type) {
		this.type.set(type);
		return this;
	}

	@JsonAlias("module")
	public AstModuleRelationshipPojoPrototype setModule(final EntityId module) {
		this.module.set(module);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("node", node.orElse(null))
				.append("type", type.orElse(null))
				.append("module", module.orElse(null))
				.toString();
	}
}
