/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.ast;

import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.PojoPrototype;
import innowake.mining.shared.lang.DiffSet;
import innowake.mining.shared.model.AstNodeLocation;

/**
 * {@code ast_node} entity class. Formerly know as {@code AstNodeV2} and {@code AstNode}.
 */
public class AstNodePojoPrototype implements PojoPrototype {

	public final Definable<UUID> id = new Definable<>(false, "AstNode.id");
	public final Definable<EntityId> module = new Definable<>(false, "AstNode.module");
	public final Definable<AstNodeLocation> location = new Definable<>(false, "AstNode.location");
	public final Definable<UUID> parent = new Definable<>(true, "AstNode.parent");
	public final Definable<EntityId> includedModule = new Definable<>(true, "AstNode.includedModule");
	public final Definable<Integer> sibling = new Definable<>(true, "AstNode.sibling");
	public final Definable<String> type = new Definable<>(false, "AstNode.type");
	public final Definable<Set<String>> superTypes = new Definable<>(false, "AstNode.superTypes");
	public final Definable<String> label = new Definable<>(false, "AstNode.label");
	public final Definable<Map<String, Object>> properties = new Definable<>(true, "AstNode.properties");
	
	public AstNodePojoPrototype setId(final UUID id) {
		this.id.set(id);
		return this;
	}

	@JsonAlias("moduleId")
	public AstNodePojoPrototype setModule(final EntityId module) {
		this.module.set(module);
		return this;
	}

	@JsonAlias({"location", "advancedModuleLocation"})
	public AstNodePojoPrototype setLocation(final AstNodeLocation location) {
		this.location.set(location);
		return this;
	}

	public AstNodePojoPrototype setParent(final UUID parent) {
		this.parent.set(parent);
		return this;
	}
	
	@JsonAlias("includedModule")
	public AstNodePojoPrototype setIncludedModule(final EntityId includedModule) {
		this.includedModule.set(includedModule);
		return this;
	}

	public AstNodePojoPrototype setType(final String type) {
		this.type.set(type);
		return this;
	}

	public AstNodePojoPrototype setSuperTypes(final Set<String> superTypes) {
		this.superTypes.set(superTypes);
		return this;
	}
	
	public AstNodePojoPrototype addSuperType(final String superType) {
		superTypes.getOrSet(DiffSet::new).add(superType);
		return this;
	}

	public AstNodePojoPrototype setLabel(final String label) {
		this.label.set(label);
		return this;
	}

	public AstNodePojoPrototype setProperties(final Map<String, Object> properties) {
		this.properties.set(properties);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id.orElse(null))
				.append("label", label.orElse(null))
				.append("type", type.orElse(null))
				.append("parent", parent.orElse(null))
				.append("sibling", sibling.orElse(null))
				.append("module", module.orElse(null))
				.append("inclusionCalleeModule", includedModule.orElse(null))
				.append("superTypes", superTypes.orElse(null))
				.append("location", location.orElse(null))
				.append("properties", properties.orElse(null))
				.toString();
	}

}
