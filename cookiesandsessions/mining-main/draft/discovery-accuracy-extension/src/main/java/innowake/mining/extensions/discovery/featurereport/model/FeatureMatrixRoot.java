/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.extensions.discovery.featurereport.model;

import java.util.Set;

import javax.annotation.Generated;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyDescription;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

/**
 * Mining Feature Matrix
 * <p>
 * A repository of mining feature coverage for each supported languages
 * 
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
		"features", "featureMatrix"
})
@Generated("jsonschema2pojo")
public class FeatureMatrixRoot {

	/**
	 * Set of mining features
	 * (Required)
	 * 
	 */
	@JsonProperty("features")
	@JsonDeserialize(as = java.util.LinkedHashSet.class)
	@JsonPropertyDescription("List of mining features")
	private Set<String> features = null;

	/**
	 * Matrix containing the supported features for each technology-type
	 * (Required)
	 * 
	 */
	@JsonProperty("featureMatrix")
	@JsonPropertyDescription("Matrix containing the supported features for each technology-type")
	private Set<FeatureMatrix> featureMatrixs;

	/**
	 * Matrix containing the supported features for each technology-type
	 * (Required)
	 * @return list of featureMatrix for each technology-type
	 * 
	 */
	@JsonProperty("featureMatrix")
	public Set<FeatureMatrix> getFeatureMatrixs() {
		return featureMatrixs;
	}

	/**
	 * Matrix containing the supported features for each technology-type
	 * (Required)
	 * @param featureMatrixs set list of features
	 * 
	 */
	@JsonProperty("featureMatrix")
	public void setFeatureMatrixs(Set<FeatureMatrix> featureMatrixs) {
		this.featureMatrixs = featureMatrixs;
	}

	/**
	 * List of mining features
	 * (Required)
	 * @return list of supported feature
	 * 
	 */
	@JsonProperty("features")
	public Set<String> getFeatures() {
		return features;
	}

	 /**
     * List of mining features
     * (Required)
	 * @param features contain all the supported features
     * 
     */
	@JsonProperty("features")
	public void setFeatures(Set<String> features) {
		this.features = features;
	}
}
