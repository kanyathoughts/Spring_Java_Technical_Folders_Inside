/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition;

import org.apache.commons.lang.StringUtils;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Definition of a mining data point, including the nested path from the root type that was requested.
 */
public class MiningDataPointDefinitionWithPath extends MiningDataPointDefinition {

	private String path = "";
	
	public MiningDataPointDefinitionWithPath(final MiningDataPointDefinition def, final String path) {
		super(def);
		/* if this data point is an Alias with a sub-selection, then the sub-selection is appended to the path */
		final AliasDefinition aliasDefinition = getAliasFor();
		if (aliasDefinition != null && ! StringUtils.isEmpty(aliasDefinition.getSubSelection())) {
			this.path = path + "." + aliasDefinition.getSubSelection();
		} else {
			this.path = path;
		}
	}
	
	/**
	 * MiningDataPointDefinitionWithPath constructor
	 * Constructor for JSON de-serialization. Do not use this constructor when creating a MiningDataPointDefinitionWithPath programmatically,
	 * prefer one of the other constructors instead.
	 * @param name the name of the data point
	 * @param parentTypeName the name of the type on which the data point is defined
	 * @param scalarType the type of the data point (if data point is of scalar type)
	 * @param referenceTypeName the name of the type of this data point (if the data point is of complex type)
	 * @param isArray whether the data point represents an array
	 * @param isNullable whether the data point may contain a null value
	 * @param aliasDefinition the alias definition (if this data point is an alias)
	 * @param path the path to this data point
	 */
	@JsonCreator
	public MiningDataPointDefinitionWithPath(@JsonProperty("name") final String name,
											 @JsonProperty("parentTypeName") final String parentTypeName,
											 @JsonProperty("scalarType") final ScalarType scalarType,
											 @JsonProperty("referenceTypeName") final String referenceTypeName,
											 @JsonProperty("isArray") final boolean isArray,
											 @JsonProperty("isNullable") final boolean isNullable,
											 @JsonProperty("aliasFor") final AliasDefinition aliasDefinition,
											 @JsonProperty("path") final String path) {
		super(name, parentTypeName, scalarType, referenceTypeName, isArray, isNullable, aliasDefinition);
		this.path = path;
	}
	
	/**
	 * Returns the nested path to this data point from the root type that was requested.
	 *
	 * @return path to this data point
	 */
	public String getPath() {
		return path;
	}

	@Override
	public String toString() {
		return path + "(" + super.toString() + ")";
	}
}
