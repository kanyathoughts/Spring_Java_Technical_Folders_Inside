/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.datalineage.graph;

import java.beans.ConstructorProperties;
import java.util.HashSet;
import java.util.Set;

import innowake.mining.shared.model.ModuleLocation;

/**
 * Node in a {@link DataFlowGraph} representing a Module. A module contains fields and statements and may have a number of data interfaces.
 */
public class ModuleNode extends DataFlowGraphNode {

	private final Long moduleId;
	private Set<String> dataInterfaces = new HashSet<>();
	
	/**
	 * Creates a new module node with given id and display name.
	 * @param id id of the node - must be unique within the graph
	 * @param moduleId id of the mining module that this graph node represents
	 * @param name display name for the node
	 * @param location The location of the traced field
	 */
	@ConstructorProperties({"id", "moduleId", "name", "location"})
	public ModuleNode(final String id, final Long moduleId, final String name, final ModuleLocation location) {
		super(Type.MODULE, id, name, location);
		this.moduleId = moduleId;
	}

	
	/**
	 * Returns the id of the mining module that this graph node represents.
	 *
	 * @return module id
	 */
	public Long getModuleId() {
		return moduleId;
	}

	
	/**
	 * Returns the set of ids of data interface nodes of this module.
	 *
	 * @return set of ids of data interfaces
	 */
	public Set<String> getDataInterfaces() {
		return dataInterfaces;
	}

	/**
	 * Sets the set of ids of data interface nodes of this module. This overrides the current data interfaces.
	 * @param dataInterfaces set of ids of data interfaces
	 */
	public void setDataInterfaces(final Set<String> dataInterfaces) {
		this.dataInterfaces = dataInterfaces;
	}

	/**
	 * Adds a new data interface to this module.
	 * 
	 * @param node id of the data interface node
	 */
	public void addDataInterface(final String node) {
		dataInterfaces.add(node);
	}
}
