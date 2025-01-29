/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.io;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * This class can be used to add meta-data for operation parameters.
 */
public class ParameterDescription {
	public enum ParameterType {
		STRING,
		NUMBER,
		BOOLEAN;
	}
	
	/* fields are loosely based on io.swagger.annotations.ApiParam */
	private final String name;
	private final String description;
	private final ParameterType type;
	private final boolean required;
	private final String defaultValue;
	private final boolean allowMultiple;
	private final String example;
	private final List<String> allowableValues;
	
	/**
	 * Add meta-data for parameters.
	 * 
	 * @param name the name of the parameter
	 * @param description the description of the parameter
	 * @param type the {@link ParameterType}
	 * @param required {@code true} if required
	 * @param defaultValue the default value of the parameter
	 * @param allowMultiple {@code true} if multiple allowed
	 * @param example an example for the parameter
	 * @param allowableValues {@link List} of allowable values
	 */
	@JsonCreator
	public ParameterDescription(
			@JsonProperty("name") final String name,
			@JsonProperty("description") final String description,
			@JsonProperty("type") final ParameterType type,
			@JsonProperty("required") final boolean required,
			@JsonProperty("defaultValue") final String defaultValue,
			@JsonProperty("allowMultiple") final boolean allowMultiple,
			@JsonProperty("example") final String example,
			@JsonProperty("allowableValues") final List<String> allowableValues
			) {
		super();
		this.name = name;
		this.description = description;
		this.type = type;
		this.required = required;
		this.defaultValue = defaultValue;
		this.allowMultiple = allowMultiple;
		this.example = example;
		this.allowableValues = allowableValues;
	}
	
	/**
	 * Get the parameter name.
	 *
	 * @return the name
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Get the parameter description.
	 *
	 * @return the parameter description
	 */
	public String getDescription() {
		return description;
	}
	
	/**
	 * Get the {@link ParameterType}.
	 *
	 * @return the {@link ParameterType}.
	 */
	public ParameterType getType() {
		return type;
	}
	
	/**
	 * Get if the parameter is required.
	 *
	 * @return {@code true} if required
	 */
	public boolean isRequired() {
		return required;
	}
	
	/**
	 * Get the default value.
	 *
	 * @return the default value
	 */
	public String getDefaultValue() {
		return defaultValue;
	}
	
	/**
	 * Get the parameter name.
	 *
	 * @return {@code true} if multiple allowed
	 */
	public boolean isAllowMultiple() {
		return allowMultiple;
	}
	
	/**
	 * Get example of the parameter.
	 *
	 * @return the example string
	 */
	public String getExample() {
		return example;
	}
	
	/**
	 * Get the allowable values.
	 *
	 * @return {@link List} of allowable values
	 */
	public List<String> getAllowableValues() {
		return allowableValues;
	}

	@Override
	public String toString() {
		return "ParameterDescription [" +
				"name='" + name + "'" +
				", description='" + description + "'" +
				", type=" + type +
				", required=" + required +
				", defaultValue='" + defaultValue + "'" +
				", allowMultiple=" + allowMultiple +
				", example='" + example + "'" +
				", allowableValues=" + allowableValues +
				"]";
	}
}
