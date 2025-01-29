/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.api;

import java.util.Map;

import innowake.mining.shared.model.RelationshipType;

/**
 * An edge representation in the context of dependency information.
 */
public interface DependencyEdge {
	
	/**
	 * @return the incoming Module
	 */
	public DependencyModule getIncomingModule();

	/**
	 * @return the outgoing Module
	 */
	public DependencyModule getOutgoingModule();
	
	/**
	 * @return the relationship of the edge
	 */
	public RelationshipType getRelationship();
	
	/**
	 * @return properties associated with this edge
	 */
	public Map<String, Object> getProperties();

}
