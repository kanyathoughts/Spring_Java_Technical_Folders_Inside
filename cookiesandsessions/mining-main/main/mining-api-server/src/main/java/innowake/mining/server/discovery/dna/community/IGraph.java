package innowake.mining.server.discovery.dna.community;

import java.util.function.IntPredicate;


/**
 * Any algorithms using graphs should implement this interface, this ensures
 * good decoupling.
 */
public interface IGraph {
	/**
	 * Returns the edge weight between two nodes.
	 * @param sourceNodeId The id of the source node.
	 * @param targetNodeId The id of the target node.
	 * @return A number between 0 and 1.
	 */
	public double weightOf(int sourceNodeId, int targetNodeId);
	
	/**
	 * For each relationship(adjacency) the RelationshipConsumer c is called.
	 * @param node The node which is part of an adjacency.
	 * @param consumer A relationship consumer.
	 */
	public void forEachRelationship(int node, IntBiConsumer consumer);
	
	/**
	 * The number of nodes contained in the graph.
	 * @return The number of nodes contained in the graph.
	 */
	public int nodeCount();
	
	/**
	 * The number of edges incident(degree) to the node is returned.
	 * @param node The node for which the degrees shall be returned.
	 * @return The degree of a node.
	 */
	public int degree(int node);
	
	/**
	 * For each outgoing adjacency the RelationShipConumer c is called.
	 * @param node The node which is part of the outgoing adjacency.
	 * @param consumer The relationship consumer.
	 */
	public void forEachOutgoing(int node, final IntBiConsumer consumer);
	
	/**
	 * The consumer is called for each node in the graph.
	 * @param consumer The consumer to be called.
	 */
	public void forEachNode(final IntPredicate consumer);

}
