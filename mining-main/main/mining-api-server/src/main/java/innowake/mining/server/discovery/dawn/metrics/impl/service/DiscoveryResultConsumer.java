/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.api.task.ResultConsumer;
import innowake.lib.job.api.task.TaskExceptionHandler;

import java.io.Serializable;

/**
 * Default result consumer for Discovery tasks.
 * <p>
 * The Discovery tasks typically do not produce a result, so this result consumer is only responsible
 * for reporting errors and tracking the progress.
 * <p>
 * The result consumer counts the number of tasks executed and reports the progress in the status message.
 * @param <T> the concrete return type of the task
 */
public class DiscoveryResultConsumer<T extends Serializable> extends ResultConsumer<T> {

	private static class CountingExceptionHandler<T extends Serializable> implements TaskExceptionHandler<T> {
		private int errorCount = 0;

		@Override
		public void handle(final String taskId, final Result<T> result) {
			errorCount++;
		}
	}

	private static final Logger LOG = LoggerFactory.getLogger(DiscoveryResultConsumer.class);

	private final JobMonitor jobMonitor;

	private int resultCount = 0;
	protected int totalTaskCount = -1;
	protected String statusMessage;

	/**
	 * Creates a new DiscoveryResultConsumer.
	 * 
	 * @param jobMonitor the job monitor used for reporting errors and setting the status message
	 * @param statusMessage the initial status message
	 */
	public DiscoveryResultConsumer(final JobMonitor jobMonitor, final String statusMessage) {
		super(new CountingExceptionHandler<>());
		this.jobMonitor = jobMonitor;
		this.statusMessage = statusMessage;
	}

	@Override
	protected void handleResult(final String s, final Result<T> result) {
		resultCount++;
		final String message;
		if (totalTaskCount > 0) {
			message = String.format("%s (%d/%d) (%d%%)", statusMessage, resultCount, totalTaskCount, resultCount * 100 / totalTaskCount);
		} else {
			message = statusMessage;
		}
		LOG.info(() -> message);
		jobMonitor.setStepDescription(message);
	}

	/**
	 * The total task count. When set to a value greater than 0 it enables progress reporting in the status message.
	 * @param totalTaskCount the total number of tasks
	 */
	public void setTotalTaskCount(final int totalTaskCount) {
		this.totalTaskCount = totalTaskCount;
	}

	/**
	 * The text to use when updating the step description of the job.
	 * @param statusMessage the status message
	 */
	public void setStatusMessage(final String statusMessage) {
		this.statusMessage = statusMessage;
	}

	public int getFailedTaskCount() {
		return ((CountingExceptionHandler<T>) exceptionHandler).errorCount;
	}
}
