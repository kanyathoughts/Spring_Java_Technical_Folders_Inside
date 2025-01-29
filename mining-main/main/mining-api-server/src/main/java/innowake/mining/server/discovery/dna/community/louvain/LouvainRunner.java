package innowake.mining.server.discovery.dna.community.louvain;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.UnaryOperator;

import com.carrotsearch.hppc.IntObjectHashMap;
import com.carrotsearch.hppc.IntObjectMap;
import com.carrotsearch.hppc.IntScatterSet;
import com.carrotsearch.hppc.LongDoubleHashMap;
import com.carrotsearch.hppc.LongDoubleMap;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.data.io.discovery.config.Configurable;
import innowake.mining.data.io.discovery.config.Configuration;
import innowake.mining.server.discovery.dna.community.GraphImpl;
import innowake.mining.server.discovery.dna.community.IGraph;
import innowake.mining.server.discovery.dna.model.Cluster;
import innowake.mining.shared.discovery.config.ConfigResources;
import innowake.mining.shared.entities.dna.DnaSimilarityPojo;

/**
 * A wrapper for using the adapted neo4j clustering algorithm with the ClusteringAlgorithm interface.
 */
public class LouvainRunner extends Configurable {

	private int maxLevels = 5;
	private int maxIterations = 10;
	private double defaultTolerance = 0.0001;

	private final IDMapper idMapper = new IDMapper();
	@Nullable private Louvain louvain;
	@Nullable private IGraph graph;

	/**
	 * Create a new instance of the louvain runner.
	 * This class adapts the data structure of the dna to the louvain algorithm interface.
	 * @param configProvider Function providing configuration data by configuration name.
	 */
	public LouvainRunner(final UnaryOperator<String> configProvider) {
		super(() -> configProvider.apply(ConfigResources.DNA_LOUVAIN_RUNNER_CONFIG.getResourceName()));
		loadAndApplyConfiguration();
	}

	/**
	 * Create a new instance of the louvain runner.
	 * <b>Important:</b> This instance does not load the configuration.
	 * This class adapts the data structure of the dna to the louvain algorithm interface.
	 */
	public LouvainRunner() {}
	
	/**
	 * Sets the number of graph versions.
	 * Each version optimize modularity by the number of iterations and verify the improvement
	 * @param maxLevels the maximum number of graph versions.
	 */
	@Configuration(name = "maxLevels", defaultValue = "5", comment = "Define the number of graph versions. "
			+ "Each version optimize modularity by the number of iterations and verify the improvement.", title = "Maximum Levels")
	public void setMaxLevels(final int maxLevels) {
		this.maxLevels = maxLevels;
	}

	/**
	 * Defines the number to modularity optimization iterations. https://en.wikipedia.org/wiki/Louvain_modularity
	 * @param maxIterations the maximum number to modularity optimization iterations
	 */
	@Configuration(name = "maxIterations", defaultValue = "10", 
			comment = "Define the number to modularity optimization iterations. See: https://en.wikipedia.org/wiki/Louvain_modularity",
			title = "Maximum Iterations")
	public void setMaxIterations(final int maxIterations) {
		this.maxIterations = maxIterations;
	}

	/**
	 * Sets the minimum of Q-improvement required to take the current graph as improvement of the level.
	 * Q is the graph modularity. See https://en.wikipedia.org/wiki/Modularity_(networks)
	 * @param defaultTolerance the default tolerance
	 */
	@Configuration(name = "defaultTolerance", defaultValue = "0.0001", comment = "The minimum of Q-improvement required to take the current graph as improvement of the level. "
			+ "Q is the graph modularity. See https://en.wikipedia.org/wiki/Modularity_(networks)", title = "Default Tolerance")
	public void setDefaultTolerance(final double defaultTolerance) {
		this.defaultTolerance = defaultTolerance;
	}

	/**
	 * Run the louvain algorithm on the given similarity entries.
	 * Return a set of identified clusters
	 * @param similarities The {@link DnaSimilarityPojo} entries. A data structure with left identifier and right identifier and a score.
	 * This describes the graph to run the cluster identification on.
	 * @return A list of identified cluster and the connected nodes to each cluster.
	 */
	public List<Cluster> execute(final List<DnaSimilarityPojo> similarities) {
		return execute(similarities, new NullProgressMonitor());
	}

	/**
	 * Run the louvain algorithm on the given similarity entries.
	 * Return a set of identified clusters
	 * @param similarities The {@link DnaSimilarityPojo} entries. A data structure with left identifier and right identifier and a score.
	 * This describes the graph to run the cluster identification on.
	 * @param monitor The progress monitor to report progress and check on cancelled.
	 * @return A list of identified cluster and the connected nodes to each cluster.
	 */
	public List<Cluster> execute(final List<DnaSimilarityPojo> similarities, final ProgressMonitor monitor) {
		final IntObjectMap<IntScatterSet> adjacencies = new IntObjectHashMap<>();
		final LongDoubleMap weights = new LongDoubleHashMap();
		for (final DnaSimilarityPojo sim : similarities) {
			final int leftId = idMapper.getIdForLabel(sim.getAModule().getUid());
			final int rightId = idMapper.getIdForLabel(sim.getBModule().getUid());
			final double score = sim.getSimilarity();
			IntScatterSet adsSet = adjacencies.get(leftId);
			if (adsSet == null) {
				final IntScatterSet newSet = new IntScatterSet();
				newSet.add(rightId);
				adjacencies.put(leftId, newSet);
			} else {
				adsSet.add(rightId);
			}
			adsSet = adjacencies.get(rightId);
			if (adsSet == null) {
				final IntScatterSet newSet = new IntScatterSet();
				newSet.add(leftId);
				adjacencies.put(rightId, newSet);
			} else {
				adsSet.add(leftId);
			}
			weights.put(IntOperations.combine(leftId, rightId), score);
			weights.put(IntOperations.combine(rightId, leftId), score);
		}

		graph = new GraphImpl(adjacencies.size(), adjacencies, weights);

		louvain = new Louvain(graph).withIterationTolerance(defaultTolerance).withProgressMonitor(monitor);
		louvain.compute(maxLevels, maxIterations);

		/**
		 * Retrieve results and create clustering results.
		 */
		final int[] communityIds = assertNotNull(louvain).getFinalCommunities();
		final Map<Integer, Cluster> clusters = new HashMap<>();
		for (int currentNode = 0; currentNode < communityIds.length; currentNode++) {
			final int clusterIndex = communityIds[currentNode];
			Cluster cluster = clusters.get(Integer.valueOf(clusterIndex));
			if (cluster == null) {
				cluster = new Cluster(clusterIndex);

			}
			final UUID nodeLabel = idMapper.getLabelForId(currentNode);
			if (nodeLabel != null) {
				cluster.add(nodeLabel);
				clusters.put(Integer.valueOf(clusterIndex), cluster);
			}
		}

		return new ArrayList<>(clusters.values());
	}
	
	/**
	 * Mapper which maps the concrete nodes to ascending numbers, so the graph can be represented
	 * by node from 0..N-1, where N denotes the number of nodes.
	 *
	 */
	public static class IDMapper {

		/**
		 * It is important to the algorithm that the ids begin with 0 and increment by 1.
		 */
		private final AtomicInteger currentId = new AtomicInteger(0);
		private final Map<UUID, Integer> idMap = new HashMap<>();
		private final Map<Integer, UUID> labelMap = new HashMap<>();

		/**
		 * Map a given node label to the internally used node id.
		 * The node label must identify the node unique.
		 *
		 * @param label The given node label.
		 * @return The node id generated for the label.
		 */
		public int getIdForLabel(final UUID label) {
			if (idMap.containsKey(label)) {
				return idMap.get(label).intValue();
			}
			final Integer newId = Integer.valueOf(currentId.getAndIncrement());
			idMap.put(label, newId);
			labelMap.put(newId, label);
			return newId.intValue();
		}

		/**
		 * Translate a given node id back to the corresponding label.
		 * The result could be {@code null} if the id could not be found.
		 *
		 * @param id The node id.
		 * @return The label for the id or {@code null} if the label for the id could not be found.
		 */
		public @Nullable UUID getLabelForId(final int id) {
			return labelMap.get(Integer.valueOf(id));
		}
	}
}
