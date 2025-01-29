package innowake.mining.server.discovery.dna.community.louvain;

import java.util.function.IntPredicate;

/**
 * Class for primitive integer operations.
 */
public class IntOperations {
	
	private IntOperations() {}
	
	/**
	 * Shifts head into the most significant 4 bytes of the long
	 * and places the tail in the least significant bytes
	 *
	 * @param head an arbitrary int value
	 * @param tail an arbitrary int value
	 * @return combination of head and tail
	 */
	public static long combine(final int head, final int tail) {
		return ((long) head << 32) | tail & 0xFFFFFFFFL;
	}

	/**
	 * Consume all integers from 0 till given {@code nodeCount}
	 *
	 * @param nodeCount The number of nodes to consume
	 * @param consumer The predicate to test with the node index
	 */
	public static void consume(final int nodeCount, final IntPredicate consumer) {
		final int[] nodes = new int[nodeCount];
		for (int node = 0; node < nodeCount; node++) {
			nodes[node] = node;
			if ( ! consumer.test(node)) {
				break;
			}
		}
	}
}
