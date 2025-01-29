/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.propagation.taxonomy;

import java.io.Serializable;
import java.util.List;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.RelationshipType;

/**
 * Request model to assign TaxonomyPropagationRequest.
 */
public class TaxonomyPropagationRequest implements Serializable {

	private List<EntityId> moduleIds;

	private List<EntityId> taxonomyIds;
	
	private List<RelationshipType> incomingReferences;
	
	private List<RelationshipType> outgoingReferences;
	
	private List<DatabaseAccessType> readsWritesAccesses;
	
	/**
	 * Creates a new instance of {@linkplain TaxonomyPropagationRequest}
	 * 
	 * @param moduleIds {@linkplain List} of module ids which are to be propagated.
	 * @param taxonomyIds {@linkplain List} of taxonomy ids which are to be propagated.
	 * @param incomingReferences {@linkplain List} of incoming {@link RelationshipType Relationships}.
	 * @param outgoingReferences {@linkplain List} of outgoing {@link RelationshipType Relationships}.
	 * @param readsWritesAccesses {@linkplain List} of {@link DatabaseAccessType} references Access.
	 */
	public TaxonomyPropagationRequest(final List<EntityId> moduleIds, final List<EntityId> taxonomyIds, final List<RelationshipType> incomingReferences, final List<RelationshipType> outgoingReferences,
			final List<DatabaseAccessType> readsWritesAccesses) {
		this.moduleIds = moduleIds;
		this.taxonomyIds = taxonomyIds;
		this.incomingReferences = incomingReferences;
		this.outgoingReferences = outgoingReferences;
		this.readsWritesAccesses = readsWritesAccesses;
	}

	/**
	 * Get the {@linkplain List} of Module ids.
	 *
	 * @return {@linkplain List} of Module ids.
	 */
	public List<EntityId> getModuleIds() {
		return moduleIds;
	}

	/**
	 * Get the {@linkplain List} of Taxonomy ids.
	 *
	 * @return {@linkplain List} of Taxonomy ids.
	 */
	public List<EntityId> getTaxonomyIds() {
		return taxonomyIds;
	}

	/**
	 * Get the {@linkplain List}} of Incoming References.
	 *
	 * @return {@linkplain List} of Incoming References.
	 */
	public List<RelationshipType> getIncomingReferences() {
		return incomingReferences;
	}

	/**
	 * Get the {@linkplain List}} of Outgoing References.
	 *
	 * @return {@linkplain List} of Outgoing References.
	 */
	public List<RelationshipType> getOutgoingReferences() {
		return outgoingReferences;
	}

	/**
	 * Get the {@linkplain List}} of readWrite References Access.
	 *
	 * @return {@linkplain List} of readWrite references Access.
	 */
	public List<DatabaseAccessType> getReadsWritesAccesses() {
		return readsWritesAccesses;
	}
}
