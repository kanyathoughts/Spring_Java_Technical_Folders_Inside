/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

/**
 * Information contained in a {@link MiningDataPointDefinition} that is an alias for another data point.
 */
public class AliasDefinition {

	private final String aliasFor;
	private final String subSelection;
	private final String jsonPath;
	private final List<String> parameters;
	
	/**
	 * Create an alias definition for the given {@linkplain #getAliasFor() data point},
	 * with an additional {@linkplain #getSubSelection() sub selection} and {@linkplain #getParameters() parameters}.
	 * 
	 * @param aliasFor the path to the actual data point for which this data point is an alias
	 * @param subSelection an additional sub-selection to append to the aliased data point
	 * @param jsonPath for data points with {@link MiningDataPointDefinition.ScalarType#JSON} an optional path inside the JSON value that is selected
	 * @param parameters the list of parameters for the aliased data point
	 */
	@JsonCreator
	public AliasDefinition(@JsonProperty("aliasFor") final String aliasFor,
						   @JsonProperty("subSelection") final String subSelection,
						   @JsonProperty("jsonPath") final String jsonPath,
						   @JsonProperty("parameters") final List<String> parameters) {
		this.aliasFor = aliasFor;
		this.subSelection = subSelection;
		this.jsonPath = jsonPath;
		this.parameters = parameters;
	}

	/**
	 * Returns the path to the actual data point for which this data point is an alias.
	 *
	 * @return the path or name of the actual data point
	 */
	public String getAliasFor() {
		return aliasFor;
	}
	
	/**
	 * Returns an additional path to append to the data point identified by {@link #getAliasFor()}
	 * when selecting this data point.
	 *
	 * @return an additional sub-selection to append to the aliased data point
	 */
	public String getSubSelection() {
		return subSelection;
	}

	/**
	 * Retuns an additional path inside a JSON structure that is selected for this data point. Only for data points that alias
	 * a data point with type {@link MiningDataPointDefinition.ScalarType#JSON}.
	 * @return the JSON path or empty string
	 */
	public String getJsonPath() {
		return jsonPath;
	}

	/**
	 * Parameters to be passed to the data point identified by {@link #getAliasFor()}.
	 * Can be empty, in which case no parameters are passed.
	 *
	 * @return the list of parameters for the aliased data point
	 */
	public List<String> getParameters() {
		return parameters;
	}

	@Override
	public String toString() {
		return "AliasDefinition [aliasFor=" + aliasFor + ", subSelection=" + subSelection + ", parameters=" + parameters + "]";
	}
	
	
}
