/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.NonNull;
import innowake.lib.core.api.lang.NonNullByDefault;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;

/**
 * Metadata information for custom properties in the metamodel.
 */
@MiningDataType(name = "CustomPropertyMetadata")
@NonNullByDefault(value = false)
public class CustomPropertyMetadata {

	@MiningDataPoint(displayName = "Name", description = "The name of the custom property")
	@Usage(value = Usages.MINING_UI_CUSTOM_PROPERTIES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "0")
	})
	private String name;
	
	@MiningDataPoint(displayName = "Label", description = "The label of the custom property")
	@Usage(value = Usages.MINING_UI_CUSTOM_PROPERTIES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "1")
	})
	private String label;
	
	@MiningDataPoint(displayName = "Data Type", description = "The data type of the custom property")
	@Usage(value = Usages.MINING_UI_CUSTOM_PROPERTIES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "3")
	})
	private CustomPropertyDataType dataType;
	private String dataSource;
	
	@MiningDataPoint(displayName = "Description", description = "The description of the custom property")
	@Usage(value = Usages.MINING_UI_CUSTOM_PROPERTIES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Description"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "2")
	})
	private String description;
	
	@MiningDataPoint(displayName = "Field Type", description = "The field type of the custom property")
	@Usage(value = Usages.MINING_UI_CUSTOM_PROPERTIES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "4")
	})
	private CustomPropertyFieldType fieldType;
	
	@MiningDataPoint(displayName = "customCategory", description = "The name of the custom Category")
	@Usage(value = Usages.MINING_UI_CUSTOM_PROPERTIES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "7")
	})
	private String customCategory;

	private Boolean pluginVisible;
	private Boolean mandatory;
	private Object min;
	private Object max;
	private Boolean readOnly;
	private Map<String, Object> showWhen; 
	private List<String> customViewNames;
	
	@MiningDataPoint(displayName = "Custom View Index", description = "The custom view index of the custom property")
	@Usage(value = Usages.MINING_UI_CUSTOM_PROPERTIES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "5")
	})
	private Integer customViewIndex;
	private String validationRegex;
	private String validationErrorMessage;
	
	@MiningDataPoint(displayName = "Auto Completion Key", description = "The auto completion key of the custom property")
	@Usage(value = Usages.MINING_UI_CUSTOM_PROPERTIES_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "6")
	})
	private String autoCompletionKey;
	
	private String defaultValueKey;
	
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
	 * Returns the label of the custom property.
	 * <p>
	 * This is used as text shown to the user.
	 * <p>
	 * This attribute is mandatory.
	 *
	 * @return the label of the custom property
	 */
	@NonNull 
	public String getLabel() {
		return Assert.assertNotNull(label, "Label must be set");
	}
	
	/**
	 * Sets the label of the custom property.
	 * <p>
	 * This attribute is mandatory.
	 *
	 * @param label the label to set
	 */
	public void setLabel(@NonNull final String label) {
		this.label = label;
	}
	
	/**
	 * Signifies if the custom property is shown in the UI of the mining plugin.
	 * <p>
	 * This attribute is mandatory.
	 * 
	 * @return {@code true} if it should be shown, {@code false} otherwise
	 */
	@NonNull
	public Boolean isPluginVisible() {
		return Assert.assertNotNull(pluginVisible, "PluginVisible must be set");
	}

	/**
	 * Sets the plugin visible flag. 
	 * <p>
	 * This attribute is mandatory.
	 *
	 * @param pluginVisible the string representation of a {@link Boolean}.
	 */
	public void setPluginVisible(@NonNull final String pluginVisible) {
		this.pluginVisible = Boolean.valueOf(pluginVisible);
	}

	/**
	 * Sets the plugin visible flag. 
	 * <p>
	 * This attribute is mandatory.
	 *
	 * @param pluginVisible {@code true} if it should be visible, {@code false} otherwise
	 */
	public void setPluginVisible(final boolean pluginVisible) {
		this.pluginVisible = Boolean.valueOf(pluginVisible);
	}

	/**
	 * Returns the data type of the custom property.
	 *
	 * @return the data type
	 */
	public CustomPropertyDataType getDataType() {
		return dataType;
	}
	
	/**
	 * Sets the data type of the custom property. 
	 *
	 * @param dataType the data type to set
	 * @see CustomPropertyDataType
	 */
	public void setDataType(final Integer dataType) {
		this.dataType = dataType != null ? CustomPropertyDataType.fromOrientType(dataType) : null;
	}

	/**
	 * Sets the data type of the custom property. 
	 *
	 * @param typeName the name of the data type to set
	 * @see CustomPropertyDataType
	 */
	@JsonProperty
	public void setDataType(final String typeName) {
		this.dataType = typeName != null ? CustomPropertyDataType.fromName(typeName) : null;
	}
	
	/**
	 * Sets the data type of the custom property. 
	 *
	 * @param dataType the data type to set
	 * @see CustomPropertyDataType
	 */
	public void setDataType(final CustomPropertyDataType dataType) {
		this.dataType = dataType;
	}
	
	/**
	 * Returns the field type of the custom property which determines
	 * how it is displayed and by what means it can be edited on the UI.
	 *
	 * @return the field type
	 */
	public CustomPropertyFieldType getFieldType() {
		return fieldType;
	}
	
	/**
	 * Sets the field type of the custom property which determines
	 * how it is displayed and by what means it can be edited on the UI.
	 * 
	 * @param fieldType the field type to set
	 */
	public void setFieldType(final CustomPropertyFieldType fieldType) {
		this.fieldType = fieldType;
	}

	/**
	 * Returns the data source of the custom property
	 *
	 * @return the data source
	 */
	public String getDataSource() {
		return dataSource;
	}
	
	/**
	 * Sets the data source of the custom property.
	 *
	 * @param dataSource the data source to set
	 */
	public void setDataSource(final String dataSource) {
		this.dataSource = dataSource;
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
	 * Sets the mandatory flag. 
	 *
	 * @param mandatory {@code true} if the custom property is mandatory, {@code false} otherwise
	 */
	public void setMandatory(final boolean mandatory) {
		this.mandatory = Boolean.valueOf(mandatory);
	}

	
	/**
	 * Returns the minimum value of a numerical custom property.
	 *
	 * @return the minimum value
	 */
	public Object getMin() {
		return min;
	}
	
	/**
	 * Sets the minimum value of a custom property.
	 *
	 * @param min the minimum value
	 */
	public void setMin(final Object min) {
		this.min = min;
	}

	/**
	 * Returns the maximum value of a custom property.
	 *
	 * @return the maximum value
	 */
	public Object getMax() {
		return max;
	}
	
	/**
	 * Sets the maximum value of a custom property.
	 *
	 * @param max the maximum value
	 */
	public void setMax(final Object max) {
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
	 * Sets the ready only flag of the custom property.
	 *
	 * @param readOnly {@code true} if the custom property is read-only, {@code false} otherwise
	 */
	public void setReadOnly(final boolean readOnly) {
		this.readOnly = Boolean.valueOf(readOnly);
	}

	
	/**
	 * Returns a map that defines conditions when to show this property on the UI.
	 * The contents of the map depend on the UI usage of the property.
	 *
	 * @return a map of conditions
	 */
	@NonNull
	public Map<String, Object> getShowWhen() {
		return showWhen == null ? Collections.emptyMap() : showWhen;
	}

	
	/**
	 * Sets a map that defines conditions when to show this property on the UI.
	 * The contents of the map depend on the UI usage of the property.
	 *
	 * @param showWhen a map of conditions
	 */
	public void setShowWhen(final Map<String, Object> showWhen) {
		this.showWhen = showWhen;
	}

	
	/**
	 * Returns a list of "view" or category names that can be used to group custom properties on the UI.
	 * Properties that share the same "custom view name" should be shown in the same group.
	 * Properties with multiple custom view names may be shown in multiple groups.
	 *
	 * @return a list of view names
	 */
	@NonNull
	public List<String> getCustomViewNames() {
		return customViewNames == null ? Collections.emptyList() : customViewNames;
	}

	
	/**
	 * Sets a list of "view" or category names that can be used to group custom properties on the UI.
	 *
	 * @param customViewNames a list of view names
	 */
	public void setCustomViewNames(final List<String> customViewNames) {
		this.customViewNames = customViewNames;
	}

	
	/**
	 * Returns an index that defines the order of fields inside a custom view. Fields with lower custom
	 * view index should be shown first.
	 *
	 * @return the view index of this custom property
	 */
	@NonNull
	public Integer getCustomViewIndex() {
		return customViewIndex == null ? Integer.valueOf(0) : customViewIndex;
	}

	
	/**
	 * Sets the index that defines the order of fields inside a custom view. Fields with lower custom
	 * view index should be shown first.
	 *
	 * @param customViewIndex the view index of this custom property
	 */
	public void setCustomViewIndex(final Integer customViewIndex) {
		this.customViewIndex = customViewIndex;
	}

	
	/**
	 * Returns a regular expression that shall be used to validate the contents of the field.
	 * If the regular expression does not match the current value of the field, then the UI
	 * should display the error message defined in {@link #getValidationErrorMessage()} to the user,
	 * or a generic error message if not specified.
	 * <p>
	 * Note that the validation is purely for informational purpose. The server does not perform
	 * validation on the value of the custom property.
	 *
	 * @return a regular expression that can be used for validation
	 */
	public String getValidationRegex() {
		return validationRegex;
	}

	
	/**
	 * Sets a regular expression that shall be used to validate the contents of the field.
	 *
	 * @param validationRegex a regular expression that can be used for validation
	 */
	public void setValidationRegex(final String validationRegex) {
		this.validationRegex = validationRegex;
	}

	
	/**
	 * Returns an error message that is to be displayed on the UI in case the regular expression
	 * defined in {@link #getValidationRegex()} does not match.
	 *
	 * @return the user-visible error message (or a message key for i18n)
	 */
	public String getValidationErrorMessage() {
		return validationErrorMessage;
	}

	
	/**
	 * Sets an error message that is to be displayed on the UI in case the regular expression
	 * defined in {@link #getValidationRegex()} does not match.
	 *
	 * @param validationErrorMessage the user-visible error message (or a message key for i18n)
	 */
	public void setValidationErrorMessage(final String validationErrorMessage) {
		this.validationErrorMessage = validationErrorMessage;
	}

	/**
	 * Returns the auto completion key for annotation tags.
	 *
	 * @return auto completion key
	 */
	public String getAutoCompletionKey() {
		return autoCompletionKey;
	}

	/**
	 * Sets the auto completion key for annotation tags.
	 *
	 * @param autoCompletionKey The key
	 */
	public void setAutoCompletionKey(final String autoCompletionKey) {
		this.autoCompletionKey = autoCompletionKey;
	}

	public String getCustomCategory() {
		return customCategory;
	}

	public void setCustomCategory(String customCategory) {
		this.customCategory = customCategory;
	}

	/**
	 * Marks a this as an URL by setting the DataType to String and the FieldType to URL
	 */
	public void markAsUrl() {
		dataType = CustomPropertyDataType.STRING;
		fieldType = CustomPropertyFieldType.URL;
	}
	
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append("CustomPropertyMeta [name=");
		builder.append(name);
		builder.append(", label=");
		builder.append(label);
		builder.append(", dataType=");
		builder.append(dataType);
		builder.append(", dataSource=");
		builder.append(dataSource);
		builder.append(", description=");
		builder.append(description);
		builder.append(", fieldType=");
		builder.append(fieldType);
		builder.append(", pluginVisible=");
		builder.append(pluginVisible);
		builder.append(", mandatory=");
		builder.append(mandatory);
		builder.append(", min=");
		builder.append(min);
		builder.append(", max=");
		builder.append(max);
		builder.append(", readOnly=");
		builder.append(readOnly);
		builder.append(", showWhen=");
		builder.append(showWhen);
		builder.append(", customViewNames=");
		builder.append(customViewNames);
		builder.append(", customViewIndex=");
		builder.append(customViewIndex);
		builder.append(", validationRegex=");
		builder.append(validationRegex);
		builder.append(", validationErrorMessage=");
		builder.append(validationErrorMessage);
		builder.append(", autoCompletionKey=");
		builder.append(autoCompletionKey);
		builder.append("]");
		return builder.toString();
	}	
	
	
}
