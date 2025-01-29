/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.functionalblocks;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkType;

import java.util.Map;
import java.util.Set;

/**
 * Pojo used for filtering the reachability network graph.
 */
public class ReachabilityNetworkGraphFilterRequest {

	private final Map<String, Object> filterObject;
	private final Set<TechnologyType> technologyTypes;
	private final FunctionalBlockLinkType functionalBlockLinkType;

	/**
	 * Constructor for ReachabilityNetworkGraphFilterRequest.
	 *
	 * @param filterObject Filter object
	 * @param technologyTypes Set of technology types
	 * @param functionalBlockLinkType Functional block link type
	 */
	@JsonCreator
	public ReachabilityNetworkGraphFilterRequest(@JsonProperty("filterObject") final Map<String, Object> filterObject,
			@JsonProperty("technologyTypes") final Set<TechnologyType> technologyTypes,
			@JsonProperty("functionalBlockLinkType") final FunctionalBlockLinkType functionalBlockLinkType) {
		this.filterObject = filterObject;
		this.technologyTypes = technologyTypes;
		this.functionalBlockLinkType = functionalBlockLinkType;
	}

	/**
	 * Gets the filter object.
	 *
	 * @return the filter object
	 */
	public Map<String, Object> getFilterObject() {
		return filterObject;
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
	 * Gets the functional block link type.
	 *
	 * @return the functional block link type
	 */
	public FunctionalBlockLinkType getFunctionalBlockLinkType() {
		return functionalBlockLinkType;
	}
}
