/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.api;

import java.util.List;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * A Module representation used in the context of dependency information.
 * <p>
 * The information provided is tailored for broad dependency analysis.
 */
public interface DependencyModule {
	
	/**
	 * @return the technology of the Module
	 */
	Technology getTechnology();
	
	/**
	 * @return the type of the Module
	 */
	Type getType();
	
	/**
	 * @return the name of the Module
	 */
	String getName();

	/**
	 * @return the project ID of the Module
	 */
	EntityId getProjectId();

	/**
	 * @return all incoming dependency edges
	 */
	List<DependencyEdge> getIncomings();
	
	/**
	 * Returns all incoming dependency edges with the given relationship.
	 * 
	 * @param relationship the relationship of the incoming dependency edges
	 * @return all incoming dependency edges with the given relationship
	 */
	List<DependencyEdge> getIncomings(RelationshipType relationship);

	/**
	 * @return all outgoing dependency edges
	 */
	List<DependencyEdge> getOutgoings();

	/**
	 * Returns all outgoing dependency edges with the given relationship.
	 * 
	 * @param relationship the relationship of the outgoing dependency edges
	 * @return all outgoing dependency edges with the given relationship
	 */
	List<DependencyEdge> getOutgoings(RelationshipType relationship);

}
