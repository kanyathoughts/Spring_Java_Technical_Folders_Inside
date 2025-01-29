package innowake.mining.server.discovery.dna.community.louvain;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;

/**
 * A node represents one graph node. 
 */
public class Node {
	private final UUID label;
	private final Map<UUID, Node> adjacentNodes = new HashMap<>();
	private final Map<Node, Double> weights = new HashMap<>();
	private int clusterId;
	private final Set<Node> children = new HashSet<>();
	
	/**
	 * Create a node with random ID/label.
	 */
	public Node() {
		this(UUID.randomUUID());
	}
	
	/**
	 * Create a named node.
	 * @param label The name/label of the node.
	 */
	public Node(final UUID label) {
		this.label = label;
		resetClusterId();
	}

	/**
	 * Create a named node with a custom clusterId.
	 * @param clusterId The cluster id.
	 */
	public Node(final int clusterId) {
		this(UUID.randomUUID(), clusterId);
	}
	
	/**
	 * Create a named node with a custom clusterId.
	 * @param label The name/label of the node.
	 * @param clusterId The cluster id.
	 */
	public Node(final UUID label, final int clusterId) {
		this(label);
		this.clusterId = clusterId;
	}

	/**
	 * Return all adjacent nodes as a map.
	 * @return A map containing the nodes as values and labels as keys.
	 */
	public Map<UUID, Node> getAdjacentNodes() {
		return adjacentNodes;
	}
	
	/**
	 * Return all children of this node.
	 * @return A set of children.
	 */
	public Set<Node> getChildren() {
		return this.children;
	}

	/**
	 * Return the cluster id of this node.
	 * @return The cluster id of this node.
	 */
	public int getClusterId() {
		return clusterId;
	}

	/**
	 * Return the label of this node.
	 * @return The label of this node.
	 */
	public UUID getLabel() {
		return label;
	}

	/**
	 * Get all edge weights of this node.
	 * @return all weights of this node.
	 */
	public Map<Node, Double> getWeights() {
		return weights;
	}
	
	/**
	 * Add another node as adjacent node to this node.
	 * @param node The new node to add.
	 * @param weight The edge weight of new edge.
	 */
	public void addAdjacentNode(final Node node, final double weight) {
		/* If node already exists in adjacent nodes then there's no need to continue. */
		if(adjacentNodes.put(node.getLabel(), node) != null) {
			return;
		}
		weights.put(node, Double.valueOf(weight));
		node.addAdjacentNode(this, weight);
	}
	
	/**
	 * Add a new child to this node ( necessary of backtracking the meta nodes later ).
	 * @param child A new child node.
	 */
	public void addChild(final Node child) {
		this.children.add(child);
	}

	/**
	 * Add multiple children.  
	 * @param children A list of child nodes.
	 */
	public void addChildren(final Set<Node> children) {
		this.children.addAll(children);
		
	}

	/**
	 * Resets the cluster id to the hash code of the label.
	 */
	public void resetClusterId() {
		clusterId = label.hashCode();
	}
	
	/**
	 * Set the cluster id of this node.
	 * @param clusterId A cluster id.
	 */
	public void setClusterId(final int clusterId) {
		this.clusterId = clusterId;
	}
	/**
	 * Two nodes are equals if their label name is equal.
	 */
	@Override
	public boolean equals(final @Nullable Object o) {
		if (o instanceof Node) {
			return ((Node) o).getLabel().equals(this.label);
		}
		return super.equals(o);
	}

	/**
	 * Returns the String-hashcode of the label of this node.
	 */
	@Override
	public int hashCode() {
		return label.hashCode();
	}

	/**
	 * A string representation of this node.
	 */
	@Override
	public String toString() {
		return "Node: " + label + " ClusterId " + clusterId + " Neighbors count: " + adjacentNodes.size();
	}
}
