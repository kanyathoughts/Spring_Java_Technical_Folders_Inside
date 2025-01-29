package innowake.mining.server.discovery.dna.community.louvain;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import com.carrotsearch.hppc.IntObjectMap;
import com.carrotsearch.hppc.IntObjectScatterMap;
import com.carrotsearch.hppc.IntScatterSet;
import com.carrotsearch.hppc.LongDoubleScatterMap;

import innowake.lib.job.api.NullProgressMonitor;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.discovery.dna.community.GraphImpl;
import innowake.mining.server.discovery.dna.community.IGraph;

/**
 * Implementation of the Louvain algorithm
 * <p>The algorithm runs several levels of the first+second phase which consists of the
 * following two tasks:
 * <ol>
 * <li> Phase 1: Modularity optimization and
 * <li> Phase 2: Rebuilding of the graph.
 * </ol>
 * Even better results should be obtained by utilizing HugeGraphImpl from the neo4j implementation with its sophisticated memory management using vlongs and TLAB allocation.
 * @see <a href="https://developers.google.com/protocol-buffers/docs/encoding#varints">more abount vlong</a>
 * @see <a href="https://shipilev.net/jvm-anatomy-park/4-tlab-allocation/">more abount TLAB allocation</a>
 */
public class Louvain {
	/**
	 * The number of nodes in the root graph.
	 */
	private final int rootNodeCount;
	
	/**
	 * The current level of the algorithm.
	 */
	private int level;
	
	/**
	 * The current communities for each node.
	 */
	private int[] communities = new int[0];
	
	/**
	 * The modularity for each level.
	 */
	private double[] modularities = new double[0];

	/**
	 * The node weights for each node.
	 */
	private final double[] nodeWeights;

	/**
	 * The root graph.
	 */
	private final IGraph root;
	
	/**
	 * Communities for each level.
	 */
	private int[][] dendrogram = new int[0][0];
	
	/**
	 * The current number of communities(clusters).
	 */
	private int communityCount;

	/**
	 * Modularity Optimization will quit if a modularity gain between two iterations is less than this value.
	 */
	private double tolerance = 0.0001;

	/**
	 * Default progress monitor which does nothing.
	 */
	private ProgressMonitor progressMonitor = new NullProgressMonitor();
	
	/**
	 * Create an instance of the louvain algorithm with a initial graph containing
	 * all adjacencies and weights.
	 * 
	 * @param graph The input graph.
	 */
	public Louvain(final IGraph graph) {
		this.root = graph;
		this.rootNodeCount = graph.nodeCount();
		this.communities = new int[rootNodeCount];
		this.nodeWeights = new double[rootNodeCount];
		this.communityCount = rootNodeCount;
		Arrays.setAll(communities, i -> i);
	}

	/**
	 * Run all levels of the algorithm until maxLevel is reached and maxInterations
	 * is reached.
	 * 
	 * @param maxLevel      Determines how many levels the algorithm should run.
	 * @param maxIterations Determines how many iterations for the first phase
	 *                      should be maximally used.
	 * @return The current instance of this class.
	 */
	public Louvain compute(final int maxLevel, final int maxIterations) {
		/* temporary graph */
		IGraph graph = this.root;
		/* result arrays */
		dendrogram = new int[maxLevel][];
		modularities = new double[maxLevel];
		int nodeCount = rootNodeCount;
		for (level = 0; level < maxLevel; level++) {
			progressMonitor
					.setStepDescription(String.format("Louvain modularity optimization (Level %d/%d)", Integer.valueOf(level), Integer.valueOf(maxLevel)));
			/* Start modularity optimization ( Phase 1 ) */
			final ModularityOptimization modularityOptimization = new ModularityOptimization(graph, 
					nodeId -> nodeWeights[nodeId])
					.withTolerance(tolerance)
					.withProgressMonitor(progressMonitor)
					.compute(maxIterations);
			/* Rebuild the graph based on the community structure */
			final int[] communityIds = modularityOptimization.getCommunityIds();
			communityCount = normalize(communityIds);
			/* Release the old algo instance. */
			if (communityCount >= nodeCount) {
				break;
			}
			nodeCount = communityCount;
			dendrogram[level] = rebuildCommunityStructure(communityIds);
			modularities[level] = modularityOptimization.getModularity();
			/* Rebuild the graph ( Phase 2 ). */
			graph = rebuildGraph(graph, communityIds, communityCount);
		}
		dendrogram = Arrays.copyOf(dendrogram, level);
		return this;
	}

	/**
	 * NodeId to community mapping array.
	 *
	 * @return The community ids for each node.
	 */
	public int[] getCommunityIds() {
		return communities;
	}

	/**
	 * Get the modularities for each level.
	 * 
	 * @return the modularity for all levels.
	 */
	public double[] getModularities() {
		return Arrays.copyOfRange(modularities, 0, level);
	}

	/**
	 * Get the final(best) modularity.
	 * 
	 * @return The final modularity.
	 */
	public double getFinalModularity() {
		return modularities[level - 1];
	}

	/**
	 * Number of distinct communities
	 *
	 * @return The number of communities
	 */
	public long getCommunityCount() {
		return communityCount;
	}
	
	/**
	 * Return communities for a specified level.
	 * @param level The level.
	 * @return  The communities.
	 */
	public int[] getCommunities(final int level) {
		return dendrogram[level];
	}
	
    /**
     * Returns the latest community structure.
     * @return the community structure.
     */
	public int[] getFinalCommunities() {
		return dendrogram[level-1];
	}

	/**
	 * Adds a custom iteration tolerance.
	 * @param tolerance A small number, e.g. 0.0001 is the default value.
	 * @return this.
	 */
	public Louvain withIterationTolerance(final double tolerance) {
		this.tolerance = tolerance;
		return this;
	}
	
	/**
	 * Adds a custom progress monitor.
	 * @param progressMonitor A progress monitor.
	 * @return this.
	 */
	public Louvain withProgressMonitor(final ProgressMonitor progressMonitor) {
		this.progressMonitor = progressMonitor;
		return this;
	}
	
	/**
	 * Put the community into relationships if its not already in.
	 * 
	 * @param relationships The map containing the relationships.
	 * @param community     The community to insert.
	 * @return The existing / newly created set which is now contained in the
	 *         relationships.
	 */
    private static IntScatterSet putIfAbsent(final IntObjectMap<IntScatterSet> relationships, final int community) {
        final IntScatterSet intCursors = relationships.get(community);
        if (null == intCursors) {
            final IntScatterSet newSet = new IntScatterSet();
            relationships.put(community, newSet);
            return newSet;
        }
        return intCursors;
    }
	
    /**
     * Rebuild the communities.
     * @param communityIds The node->community/cluster mapping.
     * @return A new array of communities.
     */
    private int[] rebuildCommunityStructure(final int[] communityIds) {
        assert rootNodeCount == communities.length;
        final int[] ints = new int[rootNodeCount];
        Arrays.setAll(ints, i -> communityIds[communities[i]]);
        communities = ints;
        return communities;
    }
    
	/**
	 * Count the communities.
	 * @param communities The array containing node->community mapping.
	 * @return The number of communities.
	 */
    private static int normalize(final int[] communities) {
        final Map<Integer, Integer> map = new HashMap<>(communities.length);
        int c = 0;
        for (int i = 0; i < communities.length; i++) {
            int mapped;
			final int community = communities[i];
            if ((mapped = map.getOrDefault(Integer.valueOf(community), Integer.valueOf(-1)).intValue()) != -1) {
                communities[i] = mapped;
            } else {
                map.put(Integer.valueOf(community), Integer.valueOf(c));
                communities[i] = c++;
            }
        }
        return c;
    }
    
	/**
	 * Create a virtual graph based on the community structure of the previous
	 * louvain round.
	 *
	 * @param graph        Previous graph
	 * @param communityIds Community structure
	 * @return A new graph built from a community structure
	 */
	private IGraph rebuildGraph(final IGraph graph, final int[] communityIds, final int communityCount) {
		/* count and normalize community structure */
		final int nodeCount = communityIds.length;
        final IntObjectMap<IntScatterSet> relationships = new IntObjectScatterMap<>(nodeCount);
        final LongDoubleScatterMap relationshipWeights = new LongDoubleScatterMap(nodeCount);
		for (int i = 0; i < nodeCount; i++) {
			/* map node nodeId to community nodeId */
			final int sourceCommunity = communityIds[i];
			/* get transitions from current node */
			graph.forEachOutgoing(i, (s, t) -> {
				final int targetCommunity = communityIds[t];
				final double value = graph.weightOf(s, t);
				if (sourceCommunity == targetCommunity) {
					nodeWeights[sourceCommunity] += value;
				}
                putIfAbsent(relationships, targetCommunity).add(sourceCommunity);
                putIfAbsent(relationships, sourceCommunity).add(targetCommunity);
                relationshipWeights.addTo(IntOperations.combine(sourceCommunity, targetCommunity), value / 2);
                relationshipWeights.addTo(IntOperations.combine(targetCommunity, sourceCommunity), value / 2);
			});
		}
		return new GraphImpl(communityCount, relationships, relationshipWeights);
	}

}
