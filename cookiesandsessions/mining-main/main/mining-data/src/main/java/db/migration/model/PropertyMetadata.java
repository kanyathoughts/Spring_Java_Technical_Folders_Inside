/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package db.migration.model;

import java.util.Map;

import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.api.lang.NonNullByDefault;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.model.CustomPropertyDataType;

/**
 * Metadata information for properties in the meta-model.
 */
@NonNullByDefault(value = false)
public class PropertyMetadata {

	private String name;
	private CustomPropertyDataType type;
	private String description;
	private Boolean mandatory;
	private String min;
	private String max;
	private Boolean readOnly;
	private String linkedClass;
	private CustomPropertyDataType linkedType;
	private Boolean notNull;
	private String defaultValue;
	private Map<Object, Object> customFields;

	/**
	 * Returns the technical name of the custom property.
	 * <p>
	 * This attribute is mandatory.
	 * 
	 * @return the technical name of the custom property.
	 */
	@NonNull
	public String getName() {
		return Assert.assertNotNull(name, "Name must be set");
	}

	/**
	 * Sets the technical name of the custom property. 
	 * <p>
	 * This attribute is mandatory.
	 *
	 * @param name the name to set
	 */
	public void setName(@NonNull final String name) {
		this.name = name;
	}

	/**
	 * Returns the data type of the custom property.
	 *
	 * @return the data type
	 */
	public CustomPropertyDataType getType() {
		return type;
	}

	/**
	 * Sets the data type of the custom property. 
	 *
	 * @param type the data type to set
	 * @see CustomPropertyDataType
	 */
	public void setType(final Integer type) {
		this.type = type != null ? CustomPropertyDataType.fromOrientType(type) : null;
	}

	/**
	 * Returns the description of the custom property.
	 *
	 * @return the description
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Sets the description of the custom property 
	 *
	 * @param description the description to set
	 */
	public void setDescription(final String description) {
		this.description = description;
	}

	/**
	 * Signifies if the custom property is mandatory.
	 *
	 * @return {@link Boolean#TRUE} if the custom property is mandatory, {@link Boolean#FALSE} otherwise
	 */
	public Boolean isMandatory() {
		return mandatory;
	}

	/**
	 * Sets the mandatory flag. 
	 *
	 * @param mandatory {@link Boolean#TRUE} if the custom property is mandatory, {@link Boolean#FALSE} otherwise
	 */
	public void setMandatory(@NonNull final Boolean mandatory) {
		this.mandatory = mandatory;
	}

	/**
	 * Returns the minimum value of a numerical custom property.
	 *
	 * @return the minimum value
	 */
	public String getMin() {
		return min;
	}

	/**
	 * Sets the minimum value of a custom property.
	 *
	 * @param min the minimum value represented as a string
	 */
	public void setMin(final String min) {
		this.min = min;
	}

	/**
	 * Returns the maximum value of a custom property.
	 *
	 * @return the maximum value
	 */
	public String getMax() {
		return max;
	}

	/**
	 * Sets the maximum value of a custom property.
	 *
	 * @param max the maximum value represented as a string
	 */
	public void setMax(final String max) {
		this.max = max;
	}

	/**
	 * Signifies if the custom property is read-only.
	 *
	 * @return {@link Boolean#TRUE} if the custom property is read-only, {@link Boolean#FALSE} otherwise
	 */
	public Boolean isReadOnly() {
		return readOnly;
	}

	/**
	 * Sets the ready only flag of the custom property.
	 *
	 * @param readOnly {@link Boolean#TRUE} if the custom property is read-only, {@link Boolean#FALSE} otherwise
	 */
	public void setReadOnly(@NonNull final Boolean readOnly) {
		this.readOnly = readOnly;
	}

	/**
	 * Returns linked class associated with type
	 *
	 * @return the linked class
	 */
	public String getLinkedClass() {
		return linkedClass;
	}

	/**
	 * Sets the linked class associated with type
	 *
	 * @param linkedClass linked class name
	 */
	public void setLinkedClass(final String linkedClass) {
		this.linkedClass = linkedClass;
	}

	/**
	 * Signifies if the property is not null.
	 *
	 * @return {@link Boolean#TRUE} if the property is not null, {@link Boolean#FALSE} otherwise
	 */
	public Boolean isNotNull() {
		return notNull;
	}

	/**
	 * Sets the not null flag of the property.
	 *
	 * @param notNull {@link Boolean#TRUE} if the property is not null, {@link Boolean#FALSE} otherwise
	 */
	public void setNotNull(final Boolean notNull) {
		this.notNull = notNull;
	}

	/**
	 *  Return the default value of the property.
	 *
	 * @return the default value
	 */
	public String getDefaultValue() {
		return defaultValue;
	}

	/**
	 * Sets the default value for this property.
	 *
	 * @param defaultValue default value
	 */
	public void setDefaultValue(final String defaultValue) {
		this.defaultValue = defaultValue;
	}

	/**
	 * Returns custom fields of the property
	 *
	 * @return the custom fields
	 */
	public Map<Object, Object> getCustomFields() {
		return customFields;
	}

	/**
	 * Sets custom fields of the property
	 *
	 * @param customFields map of custom fields
	 */
	public void setCustomFields(final Map<Object, Object> customFields) {
		this.customFields = customFields;
	}
	

	/**
	 * Returns the linked type of the custom property.
	 *
	 * @return the linked type
	 */
	public CustomPropertyDataType getLinkedType() {
		return linkedType;
	}

	/**
	 * Sets the linked data type for EMBEDDEDLIST. 
	 *
	 * @param linkedType the linked type to set
	 * @see CustomPropertyDataType
	 */
	public void setLinkedType(final Integer linkedType) {
		this.linkedType = linkedType != null ? CustomPropertyDataType.fromOrientType(linkedType) : null;
	}

}
