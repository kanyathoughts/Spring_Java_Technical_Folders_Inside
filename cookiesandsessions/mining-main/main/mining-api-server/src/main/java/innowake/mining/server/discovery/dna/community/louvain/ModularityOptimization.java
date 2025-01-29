package innowake.mining.server.discovery.dna.community.louvain;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import com.carrotsearch.hppc.IntDoubleHashMap;
import com.carrotsearch.hppc.IntDoubleMap;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.server.discovery.dna.community.IGraph;
import innowake.mining.server.discovery.dna.community.NodeWeights;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;

/**
 * Implementation of the first phase of the louvain algorithm.
 * This should only be used by the {@link Louvain} implementation, therefore the methods are package private.
 */
class ModularityOptimization {
	/**
	 * Modularity is a value between -1 and +1. The best possible modularity is +1.
	 */
	private static final double MINIMUM_MODULARITY = -1.0;

	/**
	 * only outgoing directions are visited since the graph itself must be loaded
	 * using {@code .asUndirected(true) } !
	 */
	private static final int NONE = -1;

	/**
	 * Number of nodes in the graph.
	 */
	private final int nodeCount;

	/**
	 * The node weights for the "meta"-nodes.
	 */
	private final NodeWeights nodeWeights;

	/**
	 * The graph containing adjacencies and weights.
	 */
	private final IGraph graph;

	/**
	 * The sum of all edge weights m2 and its squared value m22.
	 */
	private double m2, m22;

	/**
	 * A mapping of node->corresponding community.
	 */
	private final int[] communities;

	/**
	 * A mapping of node->Sum of the weights of the links incident to node.
	 */
	private final double[] ki;

	/**
	 * Number of iterations of the optimization.
	 */
	private int iterations;

	/**
	 * The currently best q.
	 */
	private double q = MINIMUM_MODULARITY;

	/**
	 * Iterator for iterating over nodes.
	 */
	private final NodeIterator nodeIterator;
	
	/**
	 * Tolerance between iterations.
	 */
	private double tolerance = 0.0001;

	
	/**
	 * Keeps track of iteration progress.
	 */
	private ProgressMonitor progressMonitor = new NullProgressMonitor();

	/**
	 * Create a new ModularityOptimization instance.
	 * 
	 * @param graph       A graph containing the adjacencies and weights.
	 * @param nodeWeights The nodeWeights containing the weights of the
	 *                    pseudo-nodes.
	 */
	ModularityOptimization(final IGraph graph, final NodeWeights nodeWeights) {
		this.graph = graph;
		this.nodeWeights = nodeWeights;
		this.nodeCount = Math.toIntExact(graph.nodeCount());
		this.ki = new double[nodeCount];
		this.communities = new int[nodeCount];
		this.nodeIterator = createNodeIterator();
	}

	/**
	 * Compute the first phase of the louvain algorithm.
	 *
	 * @param maxIterations The maximum number of iterations this algorithm shall
	 *                      run.
	 * @return This instance.
	 */
	ModularityOptimization compute(final int maxIterations) {
		final ExecutorService pool = Executors.newCachedThreadPool();
		init();
		final ArrayList<Task> tasks = new ArrayList<>();
		tasks.add(new Task());
		for (int i = 0; i < Runtime.getRuntime().availableProcessors(); i++) {
			tasks.add(new Task());
		}
		/* as long as maxIterations is not reached */
		for (iterations = 0; iterations < maxIterations; iterations++) {
			/* Run all tasks, either serial or parallel. */
			ParallelUtil.runWithConcurrency(pool, tasks);
			progressMonitor.checkCanceled();
			/* take the best candidate */
			final Task candidate = best(tasks);
			if (null == candidate || candidate.taskQ <= this.q + tolerance) {
				progressMonitor.checkCanceled();
				/* best candidate's modularity did not improve, so we break. */
				break;
			}
			/* save current modularity */
			this.q = candidate.taskQ;
			/* sync all tasks with the best candidate for the next round */
			sync(candidate, tasks);
		}
		pool.shutdown();
		return this;
	}

	/**
	 * Get communities.
	 *
	 * @return Node-nodeId to localCommunities nodeId mapping.
	 */
	int[] getCommunityIds() {
		return communities;
	}

	/**
	 * Number of iterations
	 *
	 * @return The number of iterations.
	 */
	int getIterations() {
		return iterations;
	}

	/**
	 * The currently best modularity.
	 * 
	 * @return the best modularity.
	 */
	double getModularity() {
		return q;
	}

	/**
	 * Restart-able task to perform modularity optimization.
	 */
	class Task implements Runnable {
		final double[] sTot, sIn;
		final int[] localCommunities;
		double bestGain, bestWeight, taskQ = MINIMUM_MODULARITY;
		int bestCommunity;
		boolean improvement = false;
		double v = 0;

		/**
		 * At creation the task copies the community-structure and initializes its
		 * helper arrays.
		 */
		public Task() {
			sTot = new double[nodeCount];
			System.arraycopy(ki, 0, sTot, 0, nodeCount);
			localCommunities = new int[nodeCount];
			System.arraycopy(communities, 0, localCommunities, 0, nodeCount);
			sIn = new double[nodeCount];
			Arrays.fill(sIn, 0.);
		}

		/**
		 * Copy community structure and helper arrays from parent task into this task.
		 * This is necessary for potential parallelization of the algorithm.
		 */
		void sync(final Task parent) {
			System.arraycopy(parent.localCommunities, 0, localCommunities, 0, nodeCount);
			System.arraycopy(parent.sTot, 0, sTot, 0, nodeCount);
			System.arraycopy(parent.sIn, 0, sIn, 0, nodeCount);
			this.taskQ = parent.taskQ;
		}

		/**
		 * Run the algorithm.
		 */
		@Override
		public void run() {
			improvement = false;
			nodeIterator.forEachNode(node -> {
				final boolean move = move(node);
				improvement |= move;
				return true;
			});
			this.taskQ = calcModularity();
		}

		/**
		 * Get the graph modularity of the calculated community structure
		 */
		double getModularity() {
			return taskQ;
		}

		/**
		 * Calculate modularity-gain for a node and move it into the best community.
		 *
		 * @param node The node to move.
		 * @return true If the node has been moved.
		 */
		private boolean move(final int node) {
			final int currentCommunity = bestCommunity = localCommunities[node];

			final int degree = graph.degree(node);
			final int[] communitiesInOrder = new int[degree];
			final IntDoubleMap communityWeights = new IntDoubleHashMap(degree);

			final int[] communityCount = { 0 };
			graph.forEachRelationship(node, (s, t) -> {
				final double weight = graph.weightOf(s, t);
				final int localCommunity = localCommunities[t];
				if (communityWeights.containsKey(localCommunity)) {
					communityWeights.addTo(localCommunity, weight);
				} else {
					communityWeights.put(localCommunity, weight);
					communitiesInOrder[communityCount[0]++] = localCommunity;
				}
			});
			final double w = communityWeights.get(currentCommunity);
			/* Remove node from the current community. */
			sTot[currentCommunity] -= ki[node];
			sIn[currentCommunity] -= 2 * (w + nodeWeights.weightOf(node));

			removeWeightForSelfRelationships(node, communityWeights);

			localCommunities[node] = NONE;
			bestGain = .0;
			bestWeight = w;
			if (degree > 0) {
				/* Calculate the modularity gain for this node for all communities. */
				for (int i = 0; i < communityCount[0]; i++) {
					final int community = communitiesInOrder[i];
					final double wic = communityWeights.get(community);
					/*
					 * Computes whether the movement would result in a modularity gain. That is true if this
					 * value is greater than zero(in almost all cases).
					 */
					final double g = wic / m2 - sTot[community] * ki[node] / m22;
					if (g > bestGain) {
						bestGain = g;
						bestCommunity = community;
						bestWeight = wic;
					}
				}
			}

			/* Move the node to the best possible community. */
			sTot[bestCommunity] += ki[node];
			sIn[bestCommunity] += 2 * (bestWeight + nodeWeights.weightOf(node));
			localCommunities[node] = bestCommunity;
			return bestCommunity != currentCommunity;
		}

		/**
		 * Remove the weight for the self relationship.
		 * 
		 * @param node             The node.
		 * @param communityWeights The map containing the community weights.
		 */
        private void removeWeightForSelfRelationships(final int node, final IntDoubleMap communityWeights) {
            graph.forEachRelationship(node, (s, t) -> {
                if(s == t) {
                    final double currentWeight = communityWeights.get(localCommunities[s]);
                    communityWeights.put(localCommunities[s], currentWeight - graph.weightOf(s, t));
                }
            });
        }

		/**
		 * Calculate the modularity.
		 * 
		 * @return A number between -1 and 1.
		 */
		private double calcModularity() {
			v = 0;
			for (int node = 0; node < nodeCount; node++) {
				graph.forEachOutgoing(node, (s, t) -> {
					if (localCommunities[s] != localCommunities[t]) {
						return;
					}
					v += graph.weightOf(s, t) - (ki[s] * ki[t] / m2);
					return;
				});
			}
			return v / m2;
		}
	}

	/**
	 * Adjust the iteration tolerance.
	 * @param tolerance Usually a small number.
	 * @return This.
	 */
	ModularityOptimization withTolerance(final double tolerance) {
		this.tolerance = tolerance;
		return this;
	}

	/**
	 * Add a progress monitor.
	 * @param progressMonitor The progress monitor to add.
	 * @return this.
	 */
	ModularityOptimization withProgressMonitor(final ProgressMonitor progressMonitor) {
		this.progressMonitor = progressMonitor;
		return this;
	}

	/**
	 * Sync parent Task with all other task except itself and copy community
	 * structure to global community structure.
	 */
	private void sync(final Task parent, final Collection<Task> tasks) {
		for (final Task task : tasks) {
			task.improvement = false;
			if (task == parent) {
				continue;
			}
			task.sync(parent);
		}
		System.arraycopy(parent.localCommunities, 0, communities, 0, nodeCount);
	}

	private NodeIterator createNodeIterator() {
		return new ShuffledNodeIterator(nodeCount);
	}

	/**
	 * Get the task with the best community distribution (highest modularity value)
	 * of an array of tasks
	 *
	 * @return The best task.
	 */
	private static @Nullable Task best(final Collection<Task> tasks) {
		/* May stay null if no task improves the current q */
		Task best = null;
		double q = MINIMUM_MODULARITY;
		for (final Task task : tasks) {
			if (!task.improvement) {
				continue;
			}
			final double modularity = task.getModularity();
			if (modularity > q) {
				q = modularity;
				best = task;
			}
		}
		return best;
	}

	/**
	 * Initialize m2, m22 and ki and the communities. Each node is assigned a
	 * different community at the beginning.
	 */
	private void init() {
		m2 = .0;
		for (int node = 0; node < nodeCount; node++) {
			graph.forEachRelationship(node, (s, t) -> {
				final double w = graph.weightOf(s, t);
				m2 += w;
				ki[s] += w / 2;
				ki[t] += w / 2;
			});
		}
		m22 = Math.pow(m2, 2.0);
		Arrays.setAll(communities, i -> i);
	}

}
