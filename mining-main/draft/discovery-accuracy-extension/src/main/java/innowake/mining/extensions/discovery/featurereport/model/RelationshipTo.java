/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.discovery.featurereport.model;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonUnwrapped;

import innowake.mining.shared.model.Relationship;

public class RelationshipTo {
	@JsonUnwrapped
	private final TechnologyAndType technologyAndType;
	private final Relationship relationship;
	private final Map<String, Set<String>> possibleAttributes;

	public RelationshipTo(TechnologyAndType technologyAndType, Relationship relationship, Map<String, Set<String>> possibleAttributes) {
		this.technologyAndType = technologyAndType;
		this.relationship = relationship;
		this.possibleAttributes = possibleAttributes;
	}

	public TechnologyAndType getTechnologyAndType() {
		return technologyAndType;
	}

	public Relationship getRelationship() {
		return relationship;
	}
	
	public Map<String, Set<String>> getPossibleAttributes() {
		return possibleAttributes;
	}
	
	public RelationshipTo withMergedAttributes(Map<String, Set<String>> otherAttributes) {
		final Map<String, Set<String>> mergedAttributes = new HashMap<>(possibleAttributes);
		otherAttributes.entrySet().forEach(entry -> {
			if (mergedAttributes.containsKey(entry.getKey())) {
				mergedAttributes.get(entry.getKey()).addAll(entry.getValue());
			} else {
				mergedAttributes.put(entry.getKey(), entry.getValue());
			}
		});
		
		return new RelationshipTo(technologyAndType, relationship, mergedAttributes);
	}

	@Override
	public int hashCode() {
		return Objects.hash(relationship, technologyAndType);
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
		RelationshipTo other = (RelationshipTo) obj;
		return relationship == other.relationship && Objects.equals(technologyAndType, other.technologyAndType);
	}
	
	@Override
	public String toString() {
		return String.format("%s(%s)%s", relationship, technologyAndType, possibleAttributes);
	}
	
}