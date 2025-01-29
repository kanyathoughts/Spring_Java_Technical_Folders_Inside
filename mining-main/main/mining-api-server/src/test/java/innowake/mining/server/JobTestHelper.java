/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server;

import java.time.Duration;
import java.time.Instant;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Helper class for dealing with {@link Job jobs} in tests.
 */
public class JobTestHelper {
	
	private JobTestHelper() {}
	
	/**
	 * Wait for the completion of a job.
	 *
	 * @param jobId the ID of the job
	 * @param jobManager the job manager associated with the job
	 * @param timeout the timeout duration
	 * @param timeoutTimeUnit the timeout unit
	 */
	public static void waitForJobCompletion(final String jobId, final JobManager jobManager, final long timeout, final TimeUnit timeoutTimeUnit) {
		final JobMonitor monitor = jobManager.getJobMonitor(jobId);
		
		if (monitor == null) {
			final Optional<JobInformation> jobInfo = jobManager
					.getJobs(q -> q.byId(UUID.fromString(jobId))).stream().findAny();
			if (jobInfo.isPresent() && ! JobStatus.isActive(jobInfo.get().getStatus())) {
				/* Job exists but is already finished */
				return;
			}
			throw new IllegalArgumentException("No job monitor for provided job ID available (" + jobId + ")");
		}
		
		final long start = System.currentTimeMillis();
		long timeoutMs = TimeUnit.MILLISECONDS.convert(timeout, timeoutTimeUnit);
		while (monitor.getStatus() == JobStatus.SCHEDULED || monitor.getStatus() == JobStatus.RUNNING) {
			if (System.currentTimeMillis() - start > timeoutMs) {
				throw new IllegalStateException("Waiting for job " + jobId + " completion timed out after " + timeout + " " + timeoutTimeUnit);
			}
			try {
				TimeUnit.SECONDS.sleep(1);
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
				throw new IllegalStateException(e);
			}
		}
	}
	
	/**
	 * Finds the first job submitted after a given point in time.
	 * @param jobManager The {@link JobManager} to query.
	 * @param jobDescription Optional description to filter jobs by. Matches any job if {@code null}.
	 * @param start The point in time from which onward to find job submissions.
	 * @param timeout Number of seconds after {@code start} during which to keep polling.
	 * @return A {@link JobInformation}, if found.
	 */
	public static Optional<JobInformation> findJobByLastSubmitTime(final JobManager jobManager,
			@Nullable final String jobDescription, final Instant start, final long timeout) {
		boolean poll = true;
		do {
			final Optional<JobInformation> jobInfo = jobManager
					.getJobs(q -> q.withSubmitTime(Comperator.GREATER_OR_EQUAL, start))
					.stream().filter(job -> jobDescription == null || jobDescription.equals(job.getJobDescription()))
					.findFirst();
			
			if (jobInfo.isPresent()) {
				return jobInfo;
			}
			
			poll = Duration.between(start, Instant.now()).getSeconds() < timeout;
			
			if (poll) {
				try {
					TimeUnit.SECONDS.sleep(1);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
					throw new IllegalStateException(e);
				}
			}
		} while (poll);
		
		return Optional.empty();
	}
	
}
