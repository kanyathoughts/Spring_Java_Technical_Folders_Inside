/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.deletion;

import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Callable;
import org.springframework.security.core.context.SecurityContextHolder;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.server.service.SystemUserSecurityContext;
import innowake.mining.server.util.TracingHelper;
import innowake.mining.shared.model.job.JobStatus;

/**
 * The {@link JobExecutionCallback} for {@link BackgroundDeletionJob BackgroundDeletionJobs}.
 * <p>
 * It makes sure to delete clients and projects which are marked for deletion while the {@link BackgroundDeletionJob deletion job} was already running.
 */
public final class BackgroundDeletionExecutionCallback implements JobExecutionCallback {

	private static final Logger LOG = LoggerFactory.getLogger(BackgroundDeletionExecutionCallback.class);

	private final BackgroundDeletionJob job;
	private final JobManager jobManager;
	private final TracingHelper tracingHelper;

	/**
	 * Finds a currently running deletion job.
	 * @param jobManager The {@link JobManager} to query.
	 * @return A {@link JobInformation}, if found.
	 */
	public static Optional<JobInformation> findPendingDeletionJob(final JobManager jobManager) {
		final var jobs = jobManager.getJobs(q -> q.withStatus(Set.of(JobStatus.RUNNING, JobStatus.SCHEDULED))
													.withDescription(BackgroundDeletionJob.DESCRIPTION));

		return jobs.isEmpty() ? Optional.empty() : Optional.of(jobs.get(0));
	}
	
	/**
	 * Tests, if a {@link BackgroundDeletionJob} is already running and, if not, submits a new one.
	 *
	 * @param jobManager The {@link JobManager}
	 * @param tracingHelper The {@link TracingHelper}
	 * @return id of the already running or scheduled {@link BackgroundDeletionJob}
	 */
	public static String submit(final JobManager jobManager, final TracingHelper tracingHelper) {
		final Optional<JobInformation> deletionJob = findPendingDeletionJob(jobManager);

		if (deletionJob.isPresent()) {
			LOG.debug(() -> "Deletion job already running. Waiting for next schedule.");
			return deletionJob.get().getJobId();
		}

		final String jobId;
		try {
			final Callable<JobMonitor> callable = () -> {
				final BackgroundDeletionJob job = new BackgroundDeletionJob();
				return jobManager.submit(job, new BackgroundDeletionExecutionCallback(job, jobManager, tracingHelper));
			};

			final JobMonitor monitor = tracingHelper.runInParentScope(callable).call();
			jobId = monitor.getJobId();
		} catch (final Exception exception) {
			LOG.error(() -> "Failed to submit BackgroundDeletionJob", exception);
			throw new IllegalStateException(exception);
		}

		LOG.debug(() -> String.format("Job sumitted (ID: %s)", jobId));
		return jobId;
	}

	/**
	 * Constructor.
	 * 
	 * @param job The {@link BackgroundDeletionJob}
	 * @param tracingHelper The {@link TracingHelper}
	 */
	private BackgroundDeletionExecutionCallback(final BackgroundDeletionJob job, final JobManager jobManager, final TracingHelper tracingHelper) {
		this.job = job;
		this.jobManager = jobManager;
		this.tracingHelper = tracingHelper;
	}

	@Override
	public void onFailure(@Nullable final Throwable throwable) {
		LOG.error(() -> "Failure in background deletion job", throwable);
	}

	/**
	 * Tests if there are projects or clients which need to be deleted and schedules another {@link BackgroundDeletionJob} if so.
	 */
	@Override
	public void onCompletion() {
		try {
			/* Test if there are projects which need to be deleted */
			final long projectsToBeDeleted = job.getToBeDeletedProjects().size();
			LOG.info(() -> String.format("Found %d additional project(s) to be deleted.", projectsToBeDeleted));
			final long clientsToBeDeleted;
			if (projectsToBeDeleted == 0) {
				/* Test if there are clients which need to be deleted */
				clientsToBeDeleted = job.countToBeDeletedClients();
				LOG.info(() -> String.format("Found %d additional clients(s) to be deleted.", clientsToBeDeleted));
			} else {
				clientsToBeDeleted = 0;
			}

			if (projectsToBeDeleted > 0 || clientsToBeDeleted > 0) {
				/* we also need to create a new tracing scope, as this code is not run in the context of an active REST request */
				tracingHelper.runInNewScope(() -> {
					/* We need to fake the authentication because this runs in a separate thread and therefore has a different context. */
					SecurityContextHolder.setContext(SystemUserSecurityContext.get());
					final String jobId = BackgroundDeletionExecutionCallback.submit(jobManager, tracingHelper);
					LOG.debug(() -> String.format("Job sumitted (ID: %s)", jobId));
				}).run();
			}
		} catch (final Exception exception) {
			/* All exceptions occurring in a Callback are somehow swallowed by Hazelcast, therefore adding explicit logging here. */
			LOG.error(() -> "Error while executing completion tests or while submitting new BackgroundDeletionJob", exception);
			throw exception;
		}
	}
}
