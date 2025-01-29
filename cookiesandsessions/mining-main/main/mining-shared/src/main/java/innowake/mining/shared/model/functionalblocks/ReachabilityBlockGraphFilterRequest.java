/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.functionalblocks;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.RelationshipType;

import java.util.Set;

/**
 * Pojo used for filtering the dependency graph.
 */
public class ReachabilityBlockGraphFilterRequest {

	private final Set<EntityId> taxonomyIds;
	private final Set<EntityId> functionalBlockIds;
	private final Set<TechnologyType> technologyTypes;
	private final Set<RelationshipType> relationshipTypes;

	/**
	 * Constructor for DependencyGraphFilterRequest.
	 *
	 * @param taxonomyIds Set of taxonomy ids
	 * @param functionalBlockIds Set of functional block ids
	 * @param technologyTypes Set of technology types
	 * @param relationshipTypes Set of relationship types
	 */
	@JsonCreator
	public ReachabilityBlockGraphFilterRequest(@JsonProperty("taxonomyIds") final Set<EntityId> taxonomyIds,
			@JsonProperty("functionalBlockIds") final Set<EntityId> functionalBlockIds,
			@JsonProperty("technologyTypes") final Set<TechnologyType> technologyTypes,
			@JsonProperty("relationshipTypes") final Set<RelationshipType> relationshipTypes) {
		this.taxonomyIds = taxonomyIds;
		this.functionalBlockIds = functionalBlockIds;
		this.relationshipTypes = relationshipTypes;
		this.technologyTypes = technologyTypes;
	}

	/**
	 * Gets the taxonomy ids.
	 *
	 * @return the taxonomy ids
	 */
	public Set<EntityId> getTaxonomyIds() {
		return taxonomyIds;
	}

	/**
	 * Gets the functional block ids.
	 *
	 * @return the functional block ids
	 */
	public Set<EntityId> getFunctionalBlockIds() {
		return functionalBlockIds;
	}

	/**
	 * Gets the technology types.
	 *
	 * @return the technology types
	 */
	public Set<TechnologyType> getTechnologyTypes() {
		return technologyTypes;
	}

	/**
	 * Gets the relationship types.
	 *
	 * @return the relationship types
	 */
	public Set<RelationshipType> getRelationshipTypes() {
		return relationshipTypes;
	}

}
