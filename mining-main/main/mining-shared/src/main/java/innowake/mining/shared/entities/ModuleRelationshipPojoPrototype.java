/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import com.fasterxml.jackson.annotation.JsonAlias;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * {@code source_metrics} entity request class, which holds the calculated source metrics of a {@code module} entity.
 */
public final class ModuleRelationshipPojoPrototype implements PojoPrototype {
	
	public final Definable<UUID> id = new Definable<>(false, "ModuleReference.id");
	public final Definable<EntityId> srcModule = new Definable<>(false, "ModuleReference.srcModule");
	public final Definable<ModuleLocation> srcLocation = new Definable<>(true, "ModuleReference.srcLocation");
	public final Definable<EntityId> dstModule = new Definable<>(false, "ModuleReference.dstModule");
	public final Definable<ModuleLocation> dstLocation = new Definable<>(true, "ModuleReference.dstLocation");
	public final Definable<RelationshipType> type = new Definable<>(false, "ModuleReference.type");
	public final Definable<Map<String, Object>> properties = new Definable<>(true, "ModuleReference.properties");
	public final Definable<Binding> dependencyBinding = new Definable<>(true, "ModuleReference.dependencyBinding");
	public final Definable<String> dependencyAttributes = new Definable<>(true, "ModuleReference.dependencyAttributes");
	public final Definable<List<EntityId>> validIfReachedFrom = new Definable<>(true, "ModuleReference.onlyIfReachedFrom");
	public final Definable<UUID> dependencyDefinition = new Definable<>(true, "ModuleReference.dependencyDefinition");

	public ModuleRelationshipPojoPrototype setId(final UUID id) {
		this.id.set(id);
		return this;
	}

	@JsonAlias("srcModuleId")
	public ModuleRelationshipPojoPrototype setSrcModule(final EntityId srcModule) {
		this.srcModule.set(this.srcModule.orElseNonNull(EntityId.VOID).merge(srcModule));
		return this;
	}

	public ModuleRelationshipPojoPrototype setSrcLocation(final ModuleLocation srcLocation) {
		this.srcLocation.set(srcLocation);
		return this;
	}

	@JsonAlias("dstModuleId")
	public ModuleRelationshipPojoPrototype setDstModule(final EntityId dstModule) {
		this.dstModule.set(this.dstModule.orElseNonNull(EntityId.VOID).merge(dstModule));
		return this;
	}

	public ModuleRelationshipPojoPrototype setDstLocation(final ModuleLocation dstLocation) {
		this.dstLocation.set(dstLocation);
		return this;
	}

	public ModuleRelationshipPojoPrototype setRelationship(final RelationshipType relationship) {
		this.type.set(relationship);
		return this;
	}
	
	public ModuleRelationshipPojoPrototype setType(final String relationship) {
		this.type.set(RelationshipType.valueOf(relationship));
		return this;
	}

	public ModuleRelationshipPojoPrototype setProperties(final Map<String, Object> properties) {
		this.properties.set(properties);
		return this;
	}

	public ModuleRelationshipPojoPrototype setDependencyBinding(final Binding dependencyBinding) {
		this.dependencyBinding.set(dependencyBinding);
		return this;
	}

	public ModuleRelationshipPojoPrototype setDependencyAttributes(final String dependencyAttributes) {
		this.dependencyAttributes.set(dependencyAttributes);
		return this;
	}

	public ModuleRelationshipPojoPrototype setValidIfReachedFrom(final List<EntityId> conditionalModules) {
		this.validIfReachedFrom.set(conditionalModules);
		return this;
	}

	/** 
	 * Sets the id of the dependency definition from which this relationship was created.
	 * 
	 * @param dependencyDefinition the dependency definition id
	 * @return this prototype object
	 */
	public ModuleRelationshipPojoPrototype setDependencyDefinition(@Nullable final UUID dependencyDefinition) {
		this.dependencyDefinition.set(dependencyDefinition);
		return this;
	}
	
	/**
	 * Gets the ID of the entity.
	 * @return Wrapped identifier(s) of the entity.
	 */
	public UUID getUUID() {
		return id.getNonNull();
	}
}
