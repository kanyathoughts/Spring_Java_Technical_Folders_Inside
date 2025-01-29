/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import java.util.Map;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Id;

/**
 * Abstract edge entity class.
 */
public abstract class Reference {

	@Id(sequence = "Reference_Sequence")
	@Nullable
	private Long id;
	@Nullable private ModuleLocation fromModuleLocationLink;
	@Nullable private ModuleLocation toModuleLocationLink;
	@Nullable private Map<String, String> properties;
	
	/**
	 * Used by proxy creation to instantiate.
	 */
	public Reference() {}
	
	/**
	 * 
	 * Instantiates the member fields.
	 * 
	 * @param fromModuleLocationLink instance of {@link ModuleLocation}
	 * @param toModuleLocation instance of {@link ModuleLocation}
	 * @param properties map of {@link String}
	 */
	public Reference(final ModuleLocation fromModuleLocationLink, final ModuleLocation toModuleLocation, final Map<String, String> properties) {
		this.fromModuleLocationLink = fromModuleLocationLink;
		this.toModuleLocationLink = toModuleLocation;
		this.properties = properties;
	}
	
	/**
	 * Returns the id.
	 *
	 * @return the id
	 */
	@Nullable
	public Long getId() {
		return id;
	}
	
	/**
	 * Sets the id.
	 *
	 * @param id the id
	 */
	public void setId(final Long id) {
		this.id = id;
	}
	
	/**
	 * Returns the {@link ModuleLocation}.
	 *
	 * @return the {@link ModuleLocation}
	 */
	@Nullable
	public ModuleLocation getFromModuleLocationLink() {
		return fromModuleLocationLink;
	}
	
	/**
	 * Sets the {@link ModuleLocation}.
	 *
	 * @param fromModuleLocationLink the {@link ModuleLocation}
	 */
	public void setFromModuleLocationLink(final ModuleLocation fromModuleLocationLink) {
		this.fromModuleLocationLink = fromModuleLocationLink;
	}
	
	/**
	 * Returns the {@link ModuleLocation}.
	 *
	 * @return the {@link ModuleLocation}
	 */
	@Nullable
	public ModuleLocation getToModuleLocationLink() {
		return toModuleLocationLink;
	}
	
	/**
	 * Sets the the {@link ModuleLocation}.
	 *
	 * @param toModuleLocationLink the {@link ModuleLocation}
	 */
	public void setToModuleLocationLink(final ModuleLocation toModuleLocationLink) {
		this.toModuleLocationLink = toModuleLocationLink;
	}
	
	/**
	 * Returns the properties.
	 *
	 * @return the property map.
	 */
	@Nullable
	public Map<String, String> getProperties() {
		return properties;
	}
	
	/**
	 * Sets the properties.
	 *
	 * @param properties the map of {@linkplain String}
	 */
	public void setProperties(final Map<String, String> properties) {
		this.properties = properties;
	}
	
	
}
