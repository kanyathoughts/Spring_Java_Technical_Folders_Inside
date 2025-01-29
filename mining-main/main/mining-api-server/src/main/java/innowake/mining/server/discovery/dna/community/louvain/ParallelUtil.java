package innowake.mining.server.discovery.dna.community.louvain;

import java.util.List;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;

import innowake.mining.server.discovery.dna.community.louvain.ModularityOptimization.Task;

/**
 * Control class for parallelization of the louvain algorithm.
 */
class ParallelUtil {
	
	private ParallelUtil() {}

	/**
	 * Run pending louvain tasks:
	 * 
	 * @param pool An executor service.
	 * @param tasks A list of runnable tasks.
	 */
	static void runWithConcurrency(final ExecutorService pool, final List<Task> tasks) {
		/* Fast-path if only one task is pending - no thread scheduling is required */
		if (tasks.size() <= 1) {
			for (int i = 0; i < tasks.size(); i++) {
				tasks.get(i).run();
			}
			return;
		}

		final ExecutorCompletionService<Double> ecs = new ExecutorCompletionService<>(pool);
		final Double result = Double.valueOf(100D);
		for (int i = 0; i < tasks.size(); i++) {
			ecs.submit(tasks.get(i), result);
		}

		for (int i = 0; i < tasks.size(); i++) {
			try {
				ecs.take();
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
				throw new RuntimeException(e.getMessage(), e);
			}
		}
	}

}
