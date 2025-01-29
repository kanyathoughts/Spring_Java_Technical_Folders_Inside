/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.extensions.discovery.featurereport.model;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.Generated;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyDescription;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import innowake.mining.shared.model.Relationship;

/**
 * Supported fields of FeatureMatrix in mining
 *
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
		"moduleType", "technology", "type", "fullySupportedFeatures", "contains", "representation", "dependencies"
})
@Generated("jsonschema2pojo")
public class FeatureMatrix {

	/**
	 * moduleType is combination of supported technology and types in mining
	 * (Required)
	 * 
	 */
	@JsonPropertyDescription("moduleType is technology and type")
	@JsonProperty("moduleType")
	private String moduleType;

	/**
	 * Supported technology/language in mining
	 * (Required)
	 * 
	 */
	@JsonPropertyDescription("technology is what kind of launguage")
	@JsonProperty("technology")
	private String technology;

	/**
	 * Supported technology's type in mining
	 * (Required)
	 * 
	 */
	@JsonPropertyDescription("type is type of technology")
	@JsonProperty("type")
	private String type;

	/**
	* Set of the features supported for the technology-type
	* (Required)
	* 
	*/
	@JsonProperty("fullySupportedFeatures")
	@JsonPropertyDescription("Lists the features supported for the technology-type")
	private Set<String> fullySupportedFeatures;

	 /**
     * Set of all In relationships module types
     * (Required)
     * 
     */
	@JsonPropertyDescription("Set of all In relationships module types")
	@JsonProperty("contains")
	private Set<String> contains;

	 /**
     * Set of physical or virtual representations
     * (Required)
     * 
     */
	@JsonProperty("representation")
	@JsonPropertyDescription("Set of physical or virtual representations")
	private Set<String> representation;

	/**
     * Set of out bound technology-types
     * (Required)
     * 
     */
	
	private Set<RelationshipTo> relationships;

	public FeatureMatrix() {

	}

	@JsonProperty("dependencies")
	@JsonPropertyDescription("Set of outbound technology-types")
	public Set<Dependency> getDependencies() {
		final Map<TechnologyAndType, List<RelationshipTo>> groups = relationships.stream().collect(Collectors.groupingBy(rel -> rel.getTechnologyAndType()));
		
		return groups.entrySet().stream()
				.map(entry -> {
					RelationshipTo merged = new RelationshipTo(entry.getKey(), Relationship.NONE, Collections.emptyMap());
					final Set<Relationship> relationships = new HashSet<>();
					for (final RelationshipTo rel : entry.getValue()) {
						merged = merged.withMergedAttributes(rel.getPossibleAttributes());
						relationships.add(rel.getRelationship());
					}
					return new Dependency(entry.getKey().toString(), relationships, merged.getPossibleAttributes());
				})
				.collect(Collectors.toSet());
	}

	public void setDependencies(Set<RelationshipTo> relationships) {
		this.relationships = relationships;
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

}
