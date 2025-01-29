/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.feature.matrix;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import innowake.mining.shared.model.RelationshipType;

/**
 * Added the new Dependency class to get the details of dependent
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
		"moduleType", "relationship", "attributes"
})
public class Dependency {

	public String moduleType = StringUtils.EMPTY;
	private Set<RelationshipType> relationship = new HashSet<>();
	private Map<String, Set<String>> attributes = new HashMap<>();

	public String getModuleType() {
		return moduleType;
	}

	public void setModuleType(String moduleType) {
		this.moduleType = moduleType;
	}

	public Set<RelationshipType> getRelationship() {
		return relationship;
	}

	public void setRelationship(Set<RelationshipType> relationship) {
		this.relationship = relationship;
	}

	public Map<String, Set<String>> getAttributes() {
		return attributes;
	}

	public void setAttributes(Map<String, Set<String>> attributes) {
		this.attributes = attributes;
	}

}
