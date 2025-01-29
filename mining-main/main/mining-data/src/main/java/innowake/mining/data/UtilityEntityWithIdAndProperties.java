/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.discovery.config.utility.UtilityEntity;
import innowake.mining.shared.model.ModuleFieldName;

/**
 * Extended UtilityEntity class to set id and properties.
 */
public class UtilityEntityWithIdAndProperties extends UtilityEntity {

	private final long id;
	private final Map<String, Object> properties;
	
	/**
	 * Constructor using default values for missing utility entities.
	 * Used when a call to a utility is found in the database, but no information
	 * about the utility is available in utilities.xml.
	 * 
	 * @param name the name of the (missing) utility
	 * @param id the database id of the utility
	 */
	public UtilityEntityWithIdAndProperties(final String name, final long id) {
		super("",
				"No definition for " + name + " in utilities.xml",
				"",
				null,
				Collections.emptyList(),
				Collections.emptyList(),
				name,
				"",
				"false",
				Collections.emptyList(),
				Collections.emptyList(),
				Collections.emptyList());
		
		this.id = id;
		this.properties = Collections.emptyMap();
	}
	
	/**
	 * Constructor to initialize parent class variables along with id and properties.
	 * 
	 * @param entity UtilityEntity loaded from utilities xml object retrieved from DB
	 * @param id The id if Project
	 * @param properties properties retrieved from DB query
	 */
	public UtilityEntityWithIdAndProperties(final UtilityEntity entity, final long id, @Nullable final Map<String, Object> properties) {
		super(entity);
		this.id = id;
		this.properties = properties != null ? properties : Collections.emptyMap();
	}
	
	/**
	 * Gets the Entity id.
	 * 
	 * @return id
	 */
	public long getId() {
		return id;
	}
	
	/**
	 * Gets the Entity Properties.
	 * 
	 * @return properties
	 */
	public Map<String, Object> getProperties() {
		return properties;
	}
	
	/**
	 * Gets the Entity field value.
	 * 
	 * @param field utility field for which value is returned from loaded xml
	 * @return object Returns a generic object containing ID/NAME/CATEGORY/INTERFACE/INBOUND/OUTBOUND
	 */
	public Object getFieldValue(final ModuleFieldName field) {
		switch (field) {
			case ID:
				return Long.valueOf(getId());
			case NAME:
				return getModuleName();
			case CATEGORIES:
				return getCategories() == null ? Collections.emptyList() : getCategories();
			case INTERFACE:
				return getUtilityInterface() == null ? "" : getUtilityInterface().getName();
			case INBOUND:
			case OUTBOUND:
				return Optional.ofNullable(getProperties().get(field.name())).orElse("");
			default:
				throw new IllegalArgumentException("unknown field: " + field);
		}
	}
}
