/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.lib.core.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;

/**
 * {@code dependency_definition} entity request class.
 */
public final class DependencyDefinitionPojoPrototype implements PojoPrototype {

	public final Definable<UUID> id = new Definable<>(false, "DependencyDefinition.id");
	public final Definable<EntityId> module = new Definable<>(false, "DependencyDefinition.module");
	public final Definable<Map<String, Object>> attributes = new Definable<>(false, "DependencyDefinition.properties");
	public final Definable<Binding> bindingType = new Definable<>(false, "DependencyDefinition.type");
	public final Definable<ModuleLocation> location = new Definable<>(true, "DependencyDefinition.location");
	public final Definable<List<ModuleFilter>> moduleFilters = new Definable<>(false, "DependencyDefinition.moduleFilters");
	public final Definable<RelationshipType> type = new Definable<>(false, "DependencyDefinition.type");
	public final Definable<Set<ResolutionFlag>> resolutionFlags = new Definable<>(false, "DependencyDefinition.resolutionFlags");
	public final Definable<Boolean> resolved = new Definable<>(false, "DependencyDefinition.resolved");
	public final Definable<List<ModuleFilter>> reachedFromModules = new Definable<>(true, "DependencyDefinition.reachedFromModules");

	public DependencyDefinitionPojoPrototype setId(final UUID id) {
		this.id.set(id);
		return this;
	}

	@JsonAlias("moduleId")
	public DependencyDefinitionPojoPrototype setModule(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}

	public DependencyDefinitionPojoPrototype setAttributes(final Map<String, Object> attributes) {
		this.attributes.set(attributes);
		return this;
	}

	public DependencyDefinitionPojoPrototype setBindingType(final Binding bindingType) {
		this.bindingType.set(bindingType);
		return this;
	}

	public DependencyDefinitionPojoPrototype setLocation(@Nullable final ModuleLocation location) {
		this.location.set(location);
		return this;
	}

	public DependencyDefinitionPojoPrototype setModuleFilters(final List<ModuleFilter> moduleFilters) {
		this.moduleFilters.set(moduleFilters);
		return this;
	}

	public DependencyDefinitionPojoPrototype setResolutionFlags(final Set<ResolutionFlag> resolutionFlags) {
		this.resolutionFlags.set(resolutionFlags);
		return this;
	}

	public DependencyDefinitionPojoPrototype setRelationshipType(final RelationshipType relationship) {
		this.type.set(relationship);
		return this;
	}

	public DependencyDefinitionPojoPrototype setResolved(final boolean resolved) {
		this.resolved.set(Boolean.valueOf(resolved));
		return this;
	}

	public DependencyDefinitionPojoPrototype setReachedFromModules(final List<ModuleFilter> reachedFromModules) {
		this.reachedFromModules.set(reachedFromModules);
		return this;
	}
}
