package innowake.mining.server.discovery.dna.community;

import java.util.function.IntPredicate;

import com.carrotsearch.hppc.IntContainer;
import com.carrotsearch.hppc.IntObjectMap;
import com.carrotsearch.hppc.LongDoubleMap;
import com.carrotsearch.hppc.procedures.IntProcedure;

import innowake.mining.server.discovery.dna.community.louvain.IntOperations;
import innowake.lib.core.api.lang.Nullable;

/**
 * A Simple implementation of a graph ( this implementation can later be replaced with a memory efficient version, like in HugeGraphImpl:
 * https://github.com/neo4j-contrib/neo4j-graph-algorithms/blob/3.5/core/src/main/java/org/neo4j/graphalgo/core/huge/HugeGraphImpl.java )
 */
public class GraphImpl implements IGraph {
	/**
	 * A map containing the weights.
	 */
	private final LongDoubleMap weights;
    
	/**
	 * A map containing all adjacencies.
	 */
	private final IntObjectMap<? extends IntContainer> adjacencies;
	
	/**
	 * The number of nodes in this graph.
	 */
	private final int nodeCount;
	
	/**
	 * Create a new graph instance.
	 * This graph do not have label nodes. For performance reasons the nodes are all positive numbers from 0 till {@code nodeCount}
	 * @param nodeCount The number of nodes represented in this graph.
	 * @param adjacencies The edges between nodes.
	 * @param weights The weights on the edges.
	 */
	public GraphImpl(final int nodeCount, final IntObjectMap<? extends IntContainer> adjacencies, final LongDoubleMap weights) {
		this.nodeCount = nodeCount;
		this.adjacencies = adjacencies;
		this.weights = weights;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public double weightOf(final int sourceNodeId, final int targetNodeId) {
		double res;
		if (sourceNodeId < targetNodeId) {
			final long combined = IntOperations.combine(sourceNodeId, targetNodeId);
			res = weights.get(combined);
		} else {
			final long combined = IntOperations.combine(targetNodeId, sourceNodeId);
			res = weights.get(combined);
		}
		
		return res;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void forEachRelationship(final int node, final IntBiConsumer consumer) {
		final @Nullable IntContainer intCursors = adjacencies.get(node);
		if (intCursors == null) {
			return;
		}
		intCursors.forEach((IntProcedure) t -> consumer.accept(node, t));
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void forEachOutgoing(final int node, final IntBiConsumer consumer) {
		forEachRelationship(node, consumer);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public int nodeCount() {
		return this.nodeCount;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public int degree(final int node) {
		return adjacencies.get(node).size();
	}

	@Override
	public void forEachNode(final IntPredicate consumer) {
        IntOperations.consume(nodeCount(), consumer);
	}
}
