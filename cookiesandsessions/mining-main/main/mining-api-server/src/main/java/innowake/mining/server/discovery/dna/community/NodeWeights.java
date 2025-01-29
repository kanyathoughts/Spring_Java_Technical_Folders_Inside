package innowake.mining.server.discovery.dna.community;

/**
 * Helper interface.
 */
@FunctionalInterface
public interface NodeWeights {
	/**
	 * Return the weight of a node.
	 * 
	 * @param nodeId The input node id.
	 * @return The weight of a node.
	 */
	double weightOf(int nodeId);
}
