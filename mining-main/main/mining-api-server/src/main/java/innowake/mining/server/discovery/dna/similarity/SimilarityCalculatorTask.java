/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dna.similarity;

import java.io.Serializable;

import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.task.Task;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;


/**
 * The {@link Task} to compute the DNA similarity. The computed value must be persisted.
 *
 * @param <T> the result of a similarity computation
 */
public abstract class SimilarityCalculatorTask<T extends Serializable> extends Task<T> {

	/**
	 * Constructor.
	 *
	 * @param progressMonitor the {@link ProgressMonitor} that should be used for this task created by {@link ProgressMonitor#subMonitor(int)}
	 * @param jobId the Id of the job this task belongs to
	 */
	protected SimilarityCalculatorTask(final ProgressMonitor progressMonitor, final String jobId) {
		super(progressMonitor, jobId);
	}

	/**
	 * The {@code SimilarityId} this task should process.
	 *
	 * @return the similarity id
	 */
	protected abstract DnaSimilarityAlgorithm getId();

}
