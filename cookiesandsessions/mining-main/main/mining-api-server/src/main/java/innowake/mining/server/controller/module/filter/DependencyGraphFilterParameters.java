/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.module.filter;

import java.util.ArrayList;
import java.util.List;

import innowake.lib.core.util.collection.CollectionUtil;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.dependency.graph.NodeType;

/**
 * {@link IDependencyGraphFilterParameter} implementation that provides the module and relationship filter parameters
 * for Dependency Graph filtering.
 */
public class DependencyGraphFilterParameters implements IDependencyGraphFilterParameter {
	
	private List<NodeType> moduleNodeTypeFilter;
	private List<RelationshipType> relationshipTypeFilter;

	/**
	 * Constructor to create Dependency Graph filtering parameters.
	 */
	public DependencyGraphFilterParameters() {
		this.moduleNodeTypeFilter = new ArrayList<>();
		this.relationshipTypeFilter = new ArrayList<>();
	}

	/**
	 * Method to add filter parameters for the Module's Node Type.
	 *
	 * @param moduleNodeTypeFilter - {@link NodeType} to be filtered
	 */
	public void addModuleNodeTypeFilter(final List<NodeType> moduleNodeTypeFilter) {
		this.moduleNodeTypeFilter.addAll(moduleNodeTypeFilter);
	}

	/**
	 * Method to add filter parameters for the Edge's Relationship Type.
	 *
	 * @param relationshipTypeFilter - {@link RelationshipType} to be filtered
	 */
	public void addRelationshipFilter(final List<RelationshipType> relationshipTypeFilter) {
		this.relationshipTypeFilter.addAll(relationshipTypeFilter);
	}

	/**
	 * Getter method to fetch all {@link NodeType} filter parameters.
	 * 
	 * @return {@link NodeType} filter parameters
	 */
	public List<NodeType> getModuleNodeTypeFilter() {
		return CollectionUtil.unmodifiableArrayList(this.moduleNodeTypeFilter);
	}

	/**
	 * Getter method to fetch all {@link RelationshipType} filter parameters.
	 * 
	 * @return {@link RelationshipType} filter parameters
	 */
	public List<RelationshipType> getRelationshipFilter() {
		return CollectionUtil.unmodifiableArrayList(this.relationshipTypeFilter);
	}
}
