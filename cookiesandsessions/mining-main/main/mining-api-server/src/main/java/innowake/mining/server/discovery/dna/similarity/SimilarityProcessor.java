/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dna.similarity;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.task.ReportMessageExceptionHandler;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.Task;
import innowake.lib.job.api.task.TaskSource;
import innowake.mining.data.model.discovery.dna.DnaConfig;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.metrics.TaskHandler;
import innowake.mining.server.util.ProgressMonitorThrottle;
import innowake.mining.shared.configuration.GenericConfiguration;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSimilarityPojo;

/**
 * The processor responsible for computing {@link DnaSimilarityPojo} for all available {@link DnaSimilarityAlgorithm}.
 */
public class SimilarityProcessor {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.DNA);
	private final AtomicInteger currentPosition = new AtomicInteger(0);
	private final TaskHandler taskHandler;
	private final ProgressMonitor progressMonitor;
	private final GenericConfiguration configProperties;
	private final DnaConfig dnaConfig;
	private static final String STEP_THROTTLE_DESCRIPTION = "Performing similarity computation: %s (%d /%d)";

	/**
	 * Constructor.
	 *
	 * @param configProperties the application configuration properties
	 * @param taskHandler the task handler to submit the tasks
	 * @param progressMonitor the {@link ProgressMonitor} that should be used for the task
	 * @param dnaConfig the DNA config
	 */
	public SimilarityProcessor(final GenericConfiguration configProperties, final TaskHandler taskHandler, final ProgressMonitor progressMonitor, final DnaConfig dnaConfig) {
		this.configProperties = configProperties;
		this.taskHandler = taskHandler;
		this.progressMonitor = progressMonitor;
		this.dnaConfig = dnaConfig;
	}

	/**
	 * Process the similarity computation for the given sequencer and dna strings.
	 *
	 * @param sequencerId the sequencer id to process
	 * @param dnaStringUuids the list of dna string {@link UUID UUIDs} to process
	 */
	public void process(final DnaSequencer sequencerId, List<UUID> dnaStringUuids) {
		final int size = dnaStringUuids.size();
		LOG.info(() -> String.format("Performing similarity computation: %s for %d Dna strings", sequencerId.getId(), size));
		progressMonitor.setStepDescription("Performing similarity computation: " + sequencerId.getId());

		if ( ! (dnaStringUuids instanceof Serializable)) {
			dnaStringUuids = new ArrayList<>(dnaStringUuids);
		}

		final int weightedLevenshteinTotalCount = size * (size - 1) / 2;
		progressMonitor.begin(weightedLevenshteinTotalCount);
		final AtomicInteger weightedLevenshteinProcessedCount = new AtomicInteger(0);
		currentPosition.set(0);
		ProgressMonitorThrottle.throttleStepDescription(
				String.format(STEP_THROTTLE_DESCRIPTION, sequencerId.getId(), weightedLevenshteinProcessedCount.get(), weightedLevenshteinTotalCount),
				progressMonitor);
		final int partitionSize = configProperties.getDiscoveryDnaSimilarityPartitionSize() == 0 ? dnaStringUuids.size()
				: configProperties.getDiscoveryDnaSimilarityPartitionSize();
		final double similarityThreshold = dnaConfig.getSimilarityThreshold();
		taskHandler.forkJobTasks(createTaskSourceForWeightedLevenshtein(sequencerId, dnaStringUuids, partitionSize, similarityThreshold),
				new ResultConsumer<Integer>(new ReportMessageExceptionHandler<>(taskHandler.getJobMonitor())) {

					@Override
					protected void handleResult(final String taskId, final Result<Integer> result) {
						final String message = String.format(STEP_THROTTLE_DESCRIPTION, sequencerId.getId(),
								weightedLevenshteinProcessedCount.addAndGet(assertNotNull(result.value)), weightedLevenshteinTotalCount);
						LOG.info(message);
						ProgressMonitorThrottle.throttleStepDescription(message, progressMonitor);
					}
				});

	}

	private TaskSource<Integer> createTaskSourceForWeightedLevenshtein(final DnaSequencer sequencerId, final List<UUID> dnaStringUuids, final int partitionSize,
			final double similarityThreshold) {
		return new TaskSource<Integer>() {

			@Override
			public boolean hasNextTask() {
				return currentPosition.get() < dnaStringUuids.size();
			}

			@Override
			public Task<Integer> nextTask() {
				return new WeightedLevenshteinComparison(progressMonitor, taskHandler.getJobId(), sequencerId, dnaStringUuids,
						currentPosition.getAndAdd(partitionSize), partitionSize, similarityThreshold);
			}
		};
	}

}
