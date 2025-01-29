package innowake.mining.server.discovery.dna.community.louvain;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * A graph implementation which can hold nodes and their adjacent nodes.
 */
public class UnitTestGraph {
	private final List<Node> allNodes;
	private final double m;

	/**
	 * Create a new graph based on a list of nodes.
	 * @param nodes A list of nodes.
	 */
	public UnitTestGraph(final List<Node> nodes) {
		Collections.shuffle(nodes);
		this.allNodes = nodes;
		m = computeM();
	}

	/**
	 * Compute the sum of all edges.
	 * @return The sum all all weights.
	 */
	protected double computeM() {
		double overallWeights = 0;
		for (final Node currentNode : allNodes) {
			overallWeights += currentNode.getWeights().values().stream().reduce(Double.valueOf(0.0), Double::sum).doubleValue();
			final Double selfNode = currentNode.getWeights().get(currentNode);
			if (selfNode != null) {
				overallWeights += selfNode.doubleValue();
			}
		}

		return overallWeights;
	}

	/**
	 * Get the sum of all edge weights
	 * @return The sum of all edge weights.
	 **/
	public double getM() {
		return m;
	}

	/**
	 * Return a list of all nodes in the graph.
	 * @return A list of all nodes contained in the graph.
	 */
	public List<Node> getAllNodes() {
		return allNodes;
	}

	
	/**
	 * Return all nodes by which are in a specified cluster.
	 * @param clusterId The cluster for which the nodes should be returned.
	 * @return The nodes of clusterId.
	 */
	public List<Node> getNodesByCluster(final int clusterId) {
		final List<Node> list = new ArrayList<Node>();
		for (final Node node : allNodes) {
			if (node.getClusterId() == clusterId) {
				list.add(node);
			}
		}
		return list;
	}
	/**
	 * Returns a list of  clusters in this graph.
	 * @return An integer list of all clusters in this graph.
	 */
	public Integer[] getAllClusters() {
		final Set<Integer> clusters = new HashSet<Integer>();
		for (final Node node : allNodes) {
			clusters.add(Integer.valueOf(node.getClusterId()));
		}

		return clusters.toArray(new Integer[clusters.size()]);
	}

	/**
	 * Print all nodes in this graph.
	 */
	public void printNodes() {
		for (final Node n : allNodes) {
			System.out.println("Node Label: " + n.getLabel() + " Cluster: " + n.getClusterId()
					+ " Weights of adjacent nodes: " + n.getWeights().values().stream().reduce(Double.valueOf(0.0), Double::sum) + " Number of children:  " + n.getChildren().size());
		}
	}
}
