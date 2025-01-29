/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.functionalblocks;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.shared.access.EntityId;

import java.util.Optional;
import java.util.Set;

/**
 * Pojo used for reachability block computation request.
 */
public class ReachabilityAnalysisRequest {

	public enum AnalysisType {
		BOTTOM_UP,
		TOP_DOWN
	}

	private final AnalysisType analysisType;
	private final Set<EntityId> moduleTaxonomies;
	private final Set<EntityId> functionalBlockUids;
	private final boolean recalculateAll;

	/**
	 * Constructor for ReachabilityBlockComputationRequest.
	 *
	 * @param analysisType Reachability analysis type
	 * @param moduleTaxonomies Module taxonomies for the reachability analysis
	 * @param functionalBlockUids Functional block UIDs for the reachability analysis
	 * @param recalculateAll Recalculate all Modules
	 */
	@JsonCreator
	public ReachabilityAnalysisRequest(
			@JsonProperty("analysisType") final AnalysisType analysisType,
			@JsonProperty("moduleTaxonomies") final Set<EntityId> moduleTaxonomies,
			@JsonProperty("functionalBlockUids") final Set<EntityId> functionalBlockUids,
			@JsonProperty("recalculateAll") final boolean recalculateAll) {
		this.analysisType = analysisType;
		this.moduleTaxonomies = Optional.ofNullable(moduleTaxonomies).orElse(Set.of());
		this.functionalBlockUids = Optional.ofNullable(functionalBlockUids).orElse(Set.of());
		this.recalculateAll = recalculateAll;
	}

	/**
	 * @return Reachability analysis type
	 */
	public AnalysisType getAnalysisType() {
		return analysisType;
	}

	/**
	 * @return Module taxonomies for the reachability analysis
	 */
	public Set<EntityId> getModuleTaxonomies() {
		return moduleTaxonomies;
	}

	/**
	 * @return Functional block UIDs for the reachability analysis
	 */
	public Set<EntityId> getFunctionalBlockUids() {
		return functionalBlockUids;
	}

	/**
	 * @return Recalculate all Modules
	 */
	public boolean getRecalculateAll() {
		return recalculateAll;
	}
}
