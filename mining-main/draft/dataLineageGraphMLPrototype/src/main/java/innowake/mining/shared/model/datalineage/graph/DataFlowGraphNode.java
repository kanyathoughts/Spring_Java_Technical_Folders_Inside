/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.datalineage.graph;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import innowake.lib.core.api.lang.Nullable;

/**
 * A node in a data flow graph.
 */
public class DataFlowGraphNode implements Serializable {

	/**
	 * The type of node.
	 */
	public enum Type {
		/**
		 * Node represents a module. A module contains fields and statements and may have a number of data interfaces.
		 */
		MODULE,
		/**
		 * Node represents a field inside a module.
		 */
		FIELD,
		/**
		 * Node represents a statement inside a module.
		 */
		STATEMENT,
		/**
		 * Node represents the data interface of a module. A data interface is a point through which data flows in or out of a module.
		 */
		DATA_INTERFACE;
	}
	
	@Nullable
	private Type type;
	@Nullable
	private String id;
	@Nullable
	private String name;
	
	private Set<String> incomings = new HashSet<>();
	private Set<String> outgoings = new HashSet<>();
	
	@Nullable
	private String parentModule;
	private Set<String> children = new HashSet<>();
	@Nullable
	private String moduleId;
	private Set<String> dataInterfaces = new HashSet<>();

	/**
	 * Returns the id of the node that represents the parent of this node.
	 * Usually the parent is a module and contains fields and statements.
	 *
	 * @return id of parent node or {@code null} if the node has no parent
	 */
	@Nullable
	public String getParentModule() {
		return parentModule;
	}
	
	/**
	 * Sets the id of the node the represents the parent of this node.
	 *
	 * @param parentModule id of parent node
	 */
	public void setParentModule(@Nullable final String parentModule) {
		this.parentModule = parentModule;
	}
	
	/**
	 * Returns the type of this node.
	 *
	 * @return the node type
	 */
	public @Nullable Type getType() {
		return type;
	}
	
	/**
	 * Returns the id of this node.
	 *
	 * @return the node id
	 */
	public @Nullable String getId() {
		return id;
	}
	
	/**
	 * Returns the display name of this node.
	 *
	 * @return the display name
	 */
	public @Nullable String getName() {
		return name;
	}
	
	/**
	 * Returns the list of incoming edges from other nodes. This is a list of the ids of the adjacent nodes.
	 *
	 * @return list of ids of nodes that have an edge TO this node
	 */
	public Set<String> getIncomings() {
		return incomings;
	}
	
	/**
	 * Returns the list of outgoing edges from this node to other nodes. This is a list of the ids of the adjacent nodes.
	 *
	 * @return list of ids of nodes that have an edge FROM this node
	 */
	public Set<String> getOutgoings() {
		return outgoings;
	}

	/**
	 * Adds a new incoming edge from another node.
	 * 
	 * @param id id of the node from where to add the edge
	 */
	public void addIncoming(final String id) {
		incomings.add(id);
	}
	
	/**
	 * Adds a new outgoing edge to another node.
	 * 
	 * @param id id of the node to which to create the edge
	 */
	public void addOutgoing(final String id) {
		outgoings.add(id);
	}
	
	/**
	 * Returns the ids of the children of this node.
	 * When a node has children, it usually means that this node is a module and the children are fields or statements.
	 *
	 * @return list of child ids
	 */
	public Set<String> getChildren() {
		return children;
	}
	
	/**
	 * Adds a new child to this node.
	 * 
	 * @param id the id of the child
	 */
	public void addChild(final String id) {
		children.add(id);
	}
	
	public void setType(final Type type) {
		this.type = type;
	}

	public void setId(final String id) {
		this.id = id;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public void setIncomings(final Set<String> incomings) {
		this.incomings = incomings;
	}

	public void setOutgoings(final Set<String> outgoings) {
		this.outgoings = outgoings;
	}

	public void setChildren(final Set<String> children) {
		this.children = children;
	}

	public void setModuleId(final String moduleId) {
		this.moduleId = moduleId;
	}

	public void setDataInterfaces(final Set<String> dataInterfaces) {
		this.dataInterfaces = dataInterfaces;
	}

	@Override
	public int hashCode() {
		return Objects.hash(id, name, type);
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if ( ! (obj instanceof DataFlowGraphNode)) {
			return false;
		}
		final DataFlowGraphNode other = (DataFlowGraphNode) obj;
		return Objects.equals(id, other.id) && Objects.equals(name, other.name) && type == other.type;
	}
}
