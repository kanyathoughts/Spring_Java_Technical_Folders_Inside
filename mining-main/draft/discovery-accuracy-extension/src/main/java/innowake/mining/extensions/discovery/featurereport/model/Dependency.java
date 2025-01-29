/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.extensions.discovery.featurereport.model;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyDescription;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonUnwrapped;

import innowake.mining.shared.model.Relationship;

/**
 * Added the new Dependency class to get the details of dependent
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
		"moduleType", "relationship"
})
public class Dependency {

	/**
	 * moduleType is combination of supported technology and types in mining
	 * (Required)
	 * 
	 */
	@JsonUnwrapped
	@JsonProperty("moduleType")
	@JsonPropertyDescription("moduleType is technology and type")
	private final String moduleType;
	/**
	* List of possible out bound relationships
	* 
	*/
	@JsonProperty("relationship")
	@JsonPropertyDescription("List of possible outbound relationships")
	private final Set<Relationship> relationship;
	
	private final Map<String, Set<String>> attributes;

	public Dependency(String moduleType, Set<Relationship> relationship, final Map<String, Set<String>> attributes) {
		this.moduleType = moduleType;
		this.relationship = relationship;
		this.attributes = attributes;
	}

	public String getModuleType() {
		return moduleType;
	}

	public Set<Relationship> getRelationship() {
		return relationship;
	}
	
	public Map<String, Set<String>> getAttributes() {
		return attributes;
	}

	@Override
	public int hashCode() {
		return Objects.hash(relationship, moduleType);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		Dependency other = (Dependency) obj;
		return relationship.containsAll(other.relationship) && Objects.equals(moduleType, other.moduleType);
	}

	@Override
	public String toString() {
		return super.toString();
	}

}
