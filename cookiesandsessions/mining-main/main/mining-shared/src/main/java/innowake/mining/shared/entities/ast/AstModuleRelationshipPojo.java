/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.ast;

import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@code ast_module_relationship} entity class. Formerly know as {@code FlowsControl} edges from {@code Modules} to {@code EntryPoints}, {@code HaltPoint} to 
 * {@code AstNode} and {@code ReturnPoint} to {@code AstNode}.
 */
public final class AstModuleRelationshipPojo {

	private final UUID node;
	private final AstModuleRelationshipType type;
	private final UUID module;

	@JsonCreator
	public AstModuleRelationshipPojo(
			@JsonProperty("node") final UUID node,
			@JsonProperty("type") final AstModuleRelationshipType type,
			@JsonProperty("module") final UUID module) {
		this.node = node;
		this.type = type;
		this.module = module;
	}

	public UUID getNode() {
		return node;
	}

	public AstModuleRelationshipType getType() {
		return type;
	}

	public UUID getModule() {
		return module;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("node", node)
				.append("type", type)
				.append("module", module)
				.toString();
	}
}
