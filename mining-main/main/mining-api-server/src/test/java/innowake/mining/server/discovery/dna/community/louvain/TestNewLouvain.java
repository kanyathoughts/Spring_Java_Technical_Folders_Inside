package innowake.mining.server.discovery.dna.community.louvain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;

import org.junit.Test;

import com.carrotsearch.hppc.IntObjectHashMap;
import com.carrotsearch.hppc.IntObjectMap;
import com.carrotsearch.hppc.IntScatterSet;
import com.carrotsearch.hppc.LongDoubleHashMap;
import com.carrotsearch.hppc.LongDoubleMap;

import innowake.mining.server.discovery.dna.community.GraphImpl;
import innowake.mining.server.discovery.dna.community.IGraph;
import innowake.mining.server.discovery.dna.community.louvain.LouvainRunner.IDMapper;
import innowake.mining.server.discovery.dna.model.Cluster;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSimilarityPojo;

/**
 * Test-cases for the ported neo4j-louvain algorithm.
 */
public class TestNewLouvain {

	private final IDMapper idMapper = new LouvainRunner.IDMapper();

	private static DnaSimilarityPojo dnaSimilarity(final UUID fromModule, final UUID toModule, final double similarity) {
		return new DnaSimilarityPojo(EntityId.of(fromModule), EntityId.of(toModule), DnaSequencer.COBOL_SKELETON_RULE,
				DnaSimilarityAlgorithm.WEIGHTED_LEVENSHTEIN, similarity);
	}
	
	/**
	 * 
	 * 
	 * Graph:
	 *  (n1) ----1----- (n2)
	 *   |  \         /  |
	 *   |   .5     .5   |
	 *   .5              .5
	 *   |   .5     .5   |
	 *   |  /         \  |
	 *  (n3) ----1----- (n4)
	 *  
	 *  Expected Result
	 *  - 2 Cluster based on the weight 1
	 *  - Cluster 1 with n1 & n2  
	 *  - Cluster 2 with n3 & n4  
	 * 
	 */
	@Test
	public void test4Nodes2Communities() {
		final UUID n1 = UUID.randomUUID();
		final UUID n2 = UUID.randomUUID();
		final UUID n3 = UUID.randomUUID();
		final UUID n4 = UUID.randomUUID();

		final List<DnaSimilarityPojo> similarities = new ArrayList<>(6);
		similarities.add(dnaSimilarity(n1, n2, 1));
		similarities.add(dnaSimilarity(n1, n3, .5));
		similarities.add(dnaSimilarity(n1, n4, .5));
		similarities.add(dnaSimilarity(n2, n4, .5));
		similarities.add(dnaSimilarity(n2, n3, .5));
		similarities.add(dnaSimilarity(n3, n4, 1));
		final List<Cluster> clusters = new LouvainRunner().execute(similarities);
		assertEquals("Assert two clusters.", 2, clusters.size());
		
		int validClusters = 0;
		for (final Cluster c : clusters) {
			final List<UUID> nodes = c.getIdentifiers();
			if ( (nodes.size() == 2 && nodes.contains(n1) && nodes.contains(n2)) 
					|| (nodes.size() == 2 && nodes.contains(n3) && nodes.contains(n4))) {
				validClusters++;
			}
		}
		assertEquals("The cluster structure is not correct.",  2, validClusters);
	}
	
	/**
	 * Test the 30-cliques test-case as many times as the optimal result has not been achived yet.
	 */
	@Test
	public void test30Cliques() {
		final UnitTestGraph cliques30 = TestGraphFactory.create30CliquesGraph();
		long counter = 0, communityCount = 150;
		double finalModularity = -1;
		/* Run until 15 communities were found or the counter gets too big. */
		final IntObjectMap<IntScatterSet> ads = getAdjacencies(cliques30);
		while (communityCount > 15 && counter <= 200) {
			final IGraph graph = new GraphImpl(ads.size(), ads,
					getWeights(cliques30));
			final Louvain louvainRes = new Louvain(graph).compute(5, 10);
			communityCount = louvainRes.getCommunityCount();
			finalModularity = louvainRes.getFinalModularity();
			counter++;
		}
		assertTrue(finalModularity > 0.88);
		assertEquals(15, communityCount);
	}
	/**
	 * Tests the whole algorithm including CSV generation.
	 */
	@Test
	public void testLouvainWrapper() {
		final List<DnaSimilarityPojo> sims = convertGraphToSimilarities(TestGraphFactory.create30CliquesGraph());
		final List<Cluster> clusters = new LouvainRunner().execute(sims);
		assertTrue(clusters.size() < 20 && clusters.size() >= 15);
	}
	
	/**
	 * Tests the performance of the adapted neo4j-louvain algorithm. Currently the
	 * performance bottleneck is the use of the standard collections ( map lookups
	 * tend to be slow for large maps with java.util.* ).
	 */
//	@Test
	public void testLouvainPerformance() {
		final int numberOfNodes = 20000;
		final IntObjectMap<IntScatterSet> adjacencies = new IntObjectHashMap<IntScatterSet>();
		final LongDoubleMap weights = new LongDoubleHashMap();
		final long startMemory = System.nanoTime();
		for (int i = 0; i < numberOfNodes; i++) {
			final IntScatterSet ads = new IntScatterSet();
			for (int j = 0; j < numberOfNodes; j++) {
				if (i != j) {
					ads.add(j);
					if(i<j) {
						weights.put(IntOperations.combine(i, j), Math.random());
					} else {
						weights.put(IntOperations.combine(j, i), Math.random());
					}
				}
			}
			adjacencies.put(i, ads);
		}
		final IGraph graph = new GraphImpl(numberOfNodes, adjacencies, weights);
		System.out.println("Structure creation time: " + ((System.nanoTime()-startMemory)/1000000000) + " seconds.");
		final long start = System.nanoTime();
		final Louvain louvain = new Louvain(graph);
		louvain.compute(5, 10);
		System.out.println("Louvain computation time for " + numberOfNodes + " nodes is: " + ((System.nanoTime()-start)/1000000000) + " seconds.");
		
		System.out.println("New community count: " + louvain.getCommunityCount());
		System.out.println("Modularity: " + louvain.getFinalModularity());
	}

	/**
	 * Assert correct Cliques value.
	 */
	@Test
	public void test2Cliques() {
		final UnitTestGraph cliques2 = TestGraphFactory.createSimple2CliquesGraph();
		final IGraph graph = new GraphImpl(getAdjacencies(cliques2).size(), getAdjacencies(cliques2), getWeights(cliques2));
		final Louvain louvainRes = new Louvain(graph).compute(10, 10);
		assertEquals(0.421875, louvainRes.getFinalModularity(), 0.0001);
	}

	/**
	 * Helper method for converting a graph to a list of similarities.
	 * @param g The input graph containing all adjacencies and weights.
	 * @return A list of similarities.
	 */
	private List<DnaSimilarityPojo> convertGraphToSimilarities(final UnitTestGraph g) {
		final List<DnaSimilarityPojo> list = new ArrayList<>();
		for(final Node node : g.getAllNodes()) {
			for(final Entry<UUID, Node> entry : node.getAdjacentNodes().entrySet()) {
				list.add(dnaSimilarity(node.getLabel(), entry.getKey(), 1));
			}
		}
		
		return list;
	}
	
	/**
	 * Return the weights of a graph as a map.
	 * @param g The input graph.
	 * @return A map where the key consists of combined sourceNodeId and targetNodeId as Long-value and the weight as double value.
	 */
	private LongDoubleMap getWeights(final UnitTestGraph g) {
		final LongDoubleMap weights = new LongDoubleHashMap();
		final List<Node> nodes = g.getAllNodes();
		for(final Node node : nodes ) {
			final int sourceNodeId = idMapper.getIdForLabel(node.getLabel());
			final Map<Node, Double> adjacentWeights = node.getWeights();
			for(final Map.Entry<Node, Double> weight : adjacentWeights.entrySet()) {
				final int targetNodeId = idMapper.getIdForLabel(weight.getKey().getLabel());
				if(sourceNodeId < targetNodeId) 
					weights.put(IntOperations.combine(sourceNodeId, targetNodeId), weight.getValue().doubleValue());
				else
					weights.put(IntOperations.combine(targetNodeId, sourceNodeId), weight.getValue().doubleValue());
			}
		}
		return weights;
	}

	
	
	/**
	 * Returns adjacencies for a graph.
	 * @param g The input graph.
	 * @return A list of adjacencies of the input graph.
	 */
	private IntObjectMap<IntScatterSet> getAdjacencies(final UnitTestGraph g) {
		final IntObjectMap<IntScatterSet> newAdjacencies = new IntObjectHashMap<IntScatterSet>();
		final List<Node> nodes = g.getAllNodes();
		for (final Node node : nodes) {
			final Map<UUID, Node> adjacents = node.getAdjacentNodes();
			final IntScatterSet integers = new IntScatterSet();
			for (final Map.Entry<UUID, Node> entry : adjacents.entrySet()) {
				integers.add(idMapper.getIdForLabel(entry.getValue().getLabel()));
			}
			newAdjacencies.put(idMapper.getIdForLabel(node.getLabel()), integers);
		}
		return newAdjacencies;
	}
}
