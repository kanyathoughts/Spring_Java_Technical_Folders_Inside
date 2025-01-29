/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;

/**
 * Represents the definition of a Custom Property.
 */
public class CustomPropertyPojo {
	
	private final UUID id;
	private final UUID project;
	private final Optional<UUID> parent;
	private final Optional<String> parentName;
	private final int ordinal;
	private final String name;
	private final Map<String, Object> properties;
	
	public CustomPropertyPojo(
			@JsonProperty("id") final UUID id,
			@JsonProperty("project") final UUID project,
			@JsonProperty("parent") @Nullable final UUID parent,
			@JsonProperty("parent") @Nullable final String parentName,
			@JsonProperty("ordinal") final int ordinal,
			@JsonProperty("name") final String name,
			@JsonProperty("properties") final Map<String, Object> properties) {
		this.id = id;
		this.project = project;
		this.parent = Optional.ofNullable(parent);
		this.parentName = Optional.ofNullable(parentName);
		this.ordinal = ordinal;
		this.name = name;
		this.properties = properties;
	}
	
	/**
	 * @return Unique ID of the Custom Property definition.
	 */
	public UUID getId() {
		return id;
	}
	
	/**
	 * @return ID of the Project the Custom Property definition belongs to.
	 */
	public UUID getProject() {
		return project;
	}
	
	/**
	 * @return ID of the Property definition (Custom Property Class) this Property is a part of, if any.
	 */
	public Optional<UUID> getParent() {
		return parent;
	}
	
	/**
	 * @return Name of the Property definition (Custom Property Class) this Property is a part of, if any.
	 */
	public Optional<String> getParentName() {
		return parentName;
	}

	/**
	 * @return Position of this Property among its siblings, see {@link #getParent()}.
	 */
	public int getOrdinal() {
		return ordinal;
	}
	
	/**
	 * @return Key under which the values of the Property are stored.
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * @return Definition details for this Custom Property, see {@link innowake.mining.shared.access.CustomPropertiesService.Properties}.
	 */
	public Map<String, Object> getProperties() {
		return properties;
	}
	
}
