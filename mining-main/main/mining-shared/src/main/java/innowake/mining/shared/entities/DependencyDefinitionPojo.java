/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.lang.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;

/**
 * {@code dependency_definition} entity class.
 */
@MiningDataType(name = MiningEnitityNames.DEPENDENCY_DEFINITION)
public final class DependencyDefinitionPojo {

	private final UUID id;
	private final EntityId module;
	private final Map<String, Object> attributes;
	private final Binding bindingType;
	@Nullable private final ModuleLocation location;
	private final List<ModuleFilter> moduleFilters;
	private final RelationshipType relationship;
	private final Set<ResolutionFlag> resolutionFlags;
	private final boolean resolved;
	private List<ModuleFilter> reachedFromModules;

	@JsonCreator
	public DependencyDefinitionPojo(
			@JsonProperty("id") final UUID id,
			@JsonProperty("moduleEntity") @Nullable final EntityId module,
			@JsonProperty("module") @Nullable final UUID moduleUid,
			@JsonProperty("moduleId") @Nullable final Long moduleNid,
			@JsonProperty("attributes") final Map<String, Object> attributes,
			@JsonProperty("bindingType") final Binding bindingType,
			@JsonProperty("location") @Nullable final ModuleLocation location,
			@JsonProperty("moduleFilters") final List<ModuleFilter> moduleFilters,
			@JsonProperty("relationship") final RelationshipType relationship,
			@JsonProperty("resolutionFlags") final Set<ResolutionFlag> resolutionFlags,
			@JsonProperty("resolved") final boolean resolved,
			@JsonProperty("reachedFromModules") @Nullable final List<ModuleFilter> reachedFromModules) {
		this.module = module != null ? module : EntityId.of(moduleUid, moduleNid);
		this.id = id;
		this.attributes = attributes;
		this.bindingType = bindingType;
		this.location = location;
		this.moduleFilters = moduleFilters;
		this.relationship = relationship;
		this.resolutionFlags = resolutionFlags;
		this.resolved = resolved;
		this.reachedFromModules = reachedFromModules == null ? Collections.emptyList() : reachedFromModules;
	}

	public DependencyDefinitionPojo(final UUID id, final Binding bindingType, @Nullable final ModuleLocation location, final List<ModuleFilter> moduleFilters, 
			final RelationshipType relationship) {
		this(id, null, null, null, Collections.emptyMap(), bindingType, location, moduleFilters, relationship, Collections.emptySet(), 
				false, Collections.emptyList());
	}

	public DependencyDefinitionPojo(final UUID id, final Binding bindingType, final List<ModuleFilter> moduleFilters, final RelationshipType relationship) {
		this(id, bindingType, null, moduleFilters, relationship);
	}

	/**
	 * @return the id of this dependency definition.
	 */
	public UUID getId() {
		return id;
	}

	/**
	 * @return the {@link EntityId} of this dependency definition.
	 */
	@JsonIgnore
	public EntityId getModule() {
		return module;
	}

	/**
	 * @return the {@link UUID} of this dependency definition.
	 */
	@JsonProperty("module")
	public UUID getModuleUid() {
		return module.getUid();
	}

	/**
	 * @return the numeric id of this dependency definition.
	 */
	@JsonProperty("moduleId")
	public Long getModuleNid() {
		return module.getNid();
	}

	/**
	 * @return the attributes of this dependency definition
	 */
	public Map<String, Object> getAttributes() {
		return attributes;
	}

	/**
	 * @return the binding type of this dependency definition.
	 */
	public Binding getBindingType() {
		return bindingType;
	}

	/**
	 * @return the {@link ModuleLocation} of this dependency definition if available.
	 */
	public Optional<ModuleLocation> getLocation() {
		return Optional.ofNullable(location);
	}

	/**
	 * @return the list of module filter describing the target of the dependency
	 */
	public List<ModuleFilter> getModuleFilters() {
		return moduleFilters;
	}

	/**
	 * @return the module relationship type of this dependency definition.
	 */
	public RelationshipType getRelationshipType() {
		return relationship;
	}

	/**
	 * @return the resolution flags of this dependency definition.
	 */
	public Set<ResolutionFlag> getResolutionFlags() {
		return resolutionFlags;
	}

	/**
	 * @return the resolution flags of this dependency definition.
	 */
	public boolean isResolved() {
		return resolved;
	}

	/**
	 * @return list of reaching modules (conditional dependencies) of the dependency. Empty if there are none.
	 */
	public List<ModuleFilter> getReachedFromModules() {
		return reachedFromModules;
	}

	/**
	 * Sets the list of reaching modules (conditional dependencies) of the dependency.
	 *
	 * @param reachedFromModules list of reaching modules (conditional dependencies) of the dependency. Empty if there are none.
	 * @return this {@link DependencyDefinitionPojo}
	 */
	public DependencyDefinitionPojo setReachedFromModules(final List<ModuleFilter> reachedFromModules) {
		this.reachedFromModules = reachedFromModules;
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("module", module)
				.append("attributes", attributes)
				.append("bindingType", bindingType)
				.append("location", location)
				.append("moduleFilters", moduleFilters)
				.append("relationship", relationship)
				.append("resolutionFlags", resolutionFlags)
				.append("resolved", resolved)
				.append("reachedFromModules", reachedFromModules)
				.toString();
	}

	/**
	 * @return a string representation of this object for key generation for map. This omits the id,resolved and reachedFromModules.
	 */
	public String buildString() {
		return bindingType.toString() + location + moduleFilters + relationship + resolutionFlags + attributes;
	}
}
