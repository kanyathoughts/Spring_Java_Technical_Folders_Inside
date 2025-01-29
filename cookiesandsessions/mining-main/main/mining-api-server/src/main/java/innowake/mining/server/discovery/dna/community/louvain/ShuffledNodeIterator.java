package innowake.mining.server.discovery.dna.community.louvain;

import java.util.Arrays;
import java.util.Random;
import java.util.function.IntPredicate;

/**
 * Creates an iterator with randomized order of nodes.
 */
public class ShuffledNodeIterator implements NodeIterator {
	
	/**
	 * Number of total nodes in the underlying graph.
	 */
	private final int nodeCount;

	/**
	 * Create the iterator for nodeCount nodes.
	 * @param nodeCount The total node count.
	 */
	public ShuffledNodeIterator(final int nodeCount) {
		this.nodeCount = nodeCount;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void forEachNode(final IntPredicate consumer) {
        final int[] nodes = new int[nodeCount];
        Arrays.setAll(nodes, i -> i);
        shuffle(nodes, nodeCount, new Random());
		for (final int node: nodes) {
			if ( ! consumer.test(node)) {
				break;
			}
		}
	}
	
	/**
	 * Shuffles an int[] array.
	 * @param data The int-array to shuffle.
	 * @param length The length of the array.
	 * @param rnd The random.
	 */
    private static void shuffle(final int[] data, final int length, final Random rnd) {
        int t, r;
        for (int i = 0; i < length; i++) {
            r = i + rnd.nextInt(length - i);
            t = data[i];
            data[i] = data[r];
            data[r] = t;
        }
    }

}
