package innowake.mining.server.discovery.dna.community.louvain;

import java.util.function.IntPredicate;

/**
 * Interface for node iterators.
 */
public interface NodeIterator {
	
    /**
     * Iterates over each node .
     * @param predicate Predicate for the int node id.
     */
    void forEachNode(final IntPredicate predicate);

}
