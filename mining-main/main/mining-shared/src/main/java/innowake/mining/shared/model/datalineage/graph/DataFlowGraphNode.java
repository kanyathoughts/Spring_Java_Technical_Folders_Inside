/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.datalineage.graph;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.datalineage.DataFlowError;
import innowake.mining.shared.model.datalineage.OrderedDataFlowErrorSetConverter;
import innowake.mining.shared.model.datalineage.SourceLocation;

/**
 * A node in a data flow graph.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({
	@JsonSubTypes.Type(value = ModuleNode.class, name = "MODULE"),
	@JsonSubTypes.Type(value = FieldNode.class, name = "FIELD"),
	@JsonSubTypes.Type(value = StatementNode.class, name = "STATEMENT"),
	@JsonSubTypes.Type(value = DataInterfaceNode.class, name = "DATA_INTERFACE")
})
public abstract class DataFlowGraphNode implements Serializable, Comparable<DataFlowGraphNode> {

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

	public enum NodeDirection {
		BOTH,
		INCOMING,
		OUTGOING;
	}

	private final Type type;
	private final String id;
	private final String name;
	@Nullable
	private final ModuleLocation location;
	
	private Set<String> incomings = new HashSet<>();
	private Set<String> outgoings = new HashSet<>();

	@Nullable
	private NodeDirection direction;

	@Nullable
	private String parentModule;
	private Set<String> children = new HashSet<>();

	private String statementLabel = "";

	@Nullable
	private SourceLocation sourceLocation;
	
	@JsonInclude(JsonInclude.Include.NON_EMPTY)
	@JsonSerialize(converter = OrderedDataFlowErrorSetConverter.class) /* The set is sorted to avoid random test failures */
	private Set<DataFlowError> errors = new TreeSet<>();
	
	protected DataFlowGraphNode(final Type type, final String id, final String name, @Nullable final ModuleLocation location) {
		this.type = type;
		this.id = id;
		this.name = name;
		this.location = location;
	}

	
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
	public Type getType() {
		return type;
	}

	
	/**
	 * Returns the id of this node.
	 *
	 * @return the node id
	 */
	public String getId() {
		return id;
	}

	
	/**
	 * Returns the display name of this node.
	 *
	 * @return the display name
	 */
	public String getName() {
		return name;
	}
	

	
	/**
	 * Returns the set of incoming edges from other nodes. This is a set of the ids of the adjacent nodes.
	 *
	 * @return set of ids of nodes that have an edge TO this node
	 */
	public Set<String> getIncomings() {
		return incomings;
	}

	/**
	 * Sets the list of incoming edges from other nodes. This overrides the current set of incomings.
	 * @param incomings set of ids of nodes that have an edge TO this node
	 */
	public void setIncomings(final Set<String> incomings) {
		this.incomings = incomings;
	}

	/**
	 * Returns the set of outgoing edges from this node to other nodes. This is a set of the ids of the adjacent nodes.
	 *
	 * @return set of ids of nodes that have an edge FROM this node
	 */
	public Set<String> getOutgoings() {
		return outgoings;
	}

	/**
	 * Sets the list of outgoing edges from this node to other nodes. This overrides the current set of outgoings.
	 * @param outgoings set of ids of nodes that have an edge FROM this node
	 */
	public void setOutgoings(final Set<String> outgoings) {
		this.outgoings = outgoings;
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
	 * @return set of child ids
	 */
	public Set<String> getChildren() {
		return children;
	}

	/**
	 * Sets the ids of the children of this node. This overrides the current children.
	 * @param children set of child ids
	 */
	public void setChildren(final Set<String> children) {
		this.children = children;
	}

	/**
	 * Adds a new child to this node.
	 * 
	 * @param id the id of the child
	 */
	public void addChild(final String id) {
		children.add(id);
	}
	
	/**
	 * Returns the sourceLocation. Can be null
	 *
	 * @return Return the sourceLocation if set
	 */
	@Nullable
	public SourceLocation getSourceLocation() {
		return sourceLocation;
	}
	
	/**
	 * Set the sourceLocation
	 *
	 * @param sourceLocation The sourcelocation
	 */
	public void setSourceLocation(@Nullable final SourceLocation sourceLocation) {
		this.sourceLocation = sourceLocation;
	}
	
	/**
	 * Gets the errors that occurred during DataFlowGraph creation
	 *
	 * @return Set of {@link DataFlowError}
	 */
	public Set<DataFlowError> getErrors() {
		return errors;
	}
	
	/**
	 * Sets the {@link DataFlowError}
	 *
	 * @param errors Set of {@link DataFlowError}
	 */
	public void setErrors(final Set<DataFlowError> errors) {
		this.errors = errors;
	}

	/**
	 * Gets the label of the statement associated with the node.
	 *
	 * @return the label of the statement associated with the node
	 */
	public String getStatementLabel() {
		return statementLabel;
	}

	/**
	 * Sets the label of the statement associated with the node.
	 * @param statementLabel the statement label
	 */
	public void setStatementLabel(final String statementLabel) {
		this.statementLabel = statementLabel;
	}

	/**
	 * Gets the direction of the node with respected to the traced field.
	 *
	 * @return the direction of the node with respected to the traced field
	 */
	@Nullable
	public NodeDirection getDirection() {
		return direction;
	}

	/**
	 * Sets the direction of the node with respected to the traced field.
	 *
	 * @param direction the direction with respected to the traced field
	 */
	public void setDirection(final NodeDirection direction) {
		this.direction = direction;
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
	
	
	@Override
	public int compareTo(final @Nullable DataFlowGraphNode o) {
		final DataFlowGraphNode thisNode = this;
		
		if (o == null) {
			return 1;
		}
		
		final DataFlowGraphNode thatNode = o;
		
		final int typeComp = thisNode.type.compareTo(thatNode.type);
		if (typeComp != 0) {
			return typeComp;
		}
		
		final int idComp = thisNode.id.compareTo(thatNode.id);
		if (idComp != 0) {
			return idComp;
		}
		
		final int nameComp = thisNode.name.compareTo(thatNode.name);
		if (nameComp != 0) {
			return nameComp;
		}
		
		return 0;
	}

	@Override
	public String toString() {
		return "DataFlowGraphNode{" +
				"type=" + type +
				", id='" + id + '\'' +
				", name='" + name + '\'' +
				", location=" + location +
				", incomings=" + incomings +
				", outgoings=" + outgoings +
				", parentModule='" + parentModule + '\'' +
				", children=" + children +
				", errors=" + errors +
				", sourceLocation=" + sourceLocation +
				'}';
	}

	/**
	 * Returns the location the node is located at
	 *
	 * @return the location
	 */
	@Nullable
	public ModuleLocation getLocation() {
		return location;
	}
}
