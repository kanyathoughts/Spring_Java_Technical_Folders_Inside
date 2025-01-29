/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.feature.matrix;

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang.StringUtils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

/**
 * Supported fields of FeatureMatrix in mining
 *
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
		"moduleType", "technology", "type", "fullySupportedFeatures", "contains", "representation", "dependencies"
})
public class FeatureMatrix {

	private String moduleType = StringUtils.EMPTY;
	private String technology = StringUtils.EMPTY;
	private String type = StringUtils.EMPTY;
	private Set<String> fullySupportedFeatures = new LinkedHashSet<>();
	private Set<String> contains = new HashSet<>();
	private Set<String> representation = new HashSet<>();
	private Set<Dependency> dependencies = new HashSet<>();

	public FeatureMatrix() {
		super();
	}

	public String getModuleType() {
		return moduleType;
	}

	public void setModuleType(String moduleType) {
		this.moduleType = moduleType;
	}

	public String getTechnology() {
		return technology;
	}

	public void setTechnology(String technology) {
		this.technology = technology;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public Set<String> getFullySupportedFeatures() {
		return fullySupportedFeatures;
	}

	public void setFullySupportedFeatures(Set<String> fullySupportedFeatures) {
		this.fullySupportedFeatures = fullySupportedFeatures;
	}

	public Set<String> getContains() {
		return contains;
	}

	public void setContains(Set<String> contains) {
		this.contains = contains;
	}

	public Set<String> getRepresentation() {
		return representation;
	}

	public void setRepresentation(Set<String> representation) {
		this.representation = representation;
	}

	public Set<Dependency> getDependencies() {
		return dependencies;
	}

	public void setDependencies(Set<Dependency> dependencies) {
		this.dependencies = dependencies;
	}

}
