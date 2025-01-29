/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.status;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.Serializable;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Import;

import innowake.mining.job.AbstractJobTest;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobInfoService;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.internal.JobInfoUtil;
import innowake.mining.job.status.TimeoutJob.Config;
import innowake.mining.shared.entities.JobInfoPojo;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Tests for job status handling.
 */
public abstract class AbstractJobStatusTest extends AbstractJobTest {

	/**
	 * Tests canceling of a running job.
	 */
	@Test
	public void testCancelRequest() {
		final CancelJob job = new CancelJob(innowake.mining.job.status.CancelJob.ExecutionMode.RUN_SUCCESSFUL);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), monitor -> {
			JobInformation runningJobInfo;
			while (true) {
				runningJobInfo = monitor.getJobInformation();
				assertNotNull(runningJobInfo);
				if (runningJobInfo.getProcessedWorkUnits() >= 2) {
					monitor.cancel();
					break;
				}
				try {
					TimeUnit.MILLISECONDS.sleep(500);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
		}, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertJobInfo(jobInfo, "Cancel job", JobStatus.CANCELED, 10);
		assertTrue(jobInfo.getProcessedWorkUnits() >= 2);
		final Serializable result = jobInfo.getResult();
		assertNull(result);
	}

	/**
	 * Tests canceling of a running job that ignores the cancel request. It will continue to run.
	 */
	@Test
	public void testIgnoredCancelRequest() {
		final CancelJob job = new CancelJob(innowake.mining.job.status.CancelJob.ExecutionMode.IGNORE_CANCEL_REQUEST);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), monitor -> {
			JobInformation runningJobInfo;
			final Instant start = Instant.now();
			JobStatus status = null;
			while (Duration.between(start, Instant.now()).toMinutes() < 2) { /* Break if the status didn't change after 2 minutes */
				runningJobInfo = monitor.getJobInformation();
				assertNotNull(runningJobInfo);
				status = runningJobInfo.getStatus();
				if (runningJobInfo.getProcessedWorkUnits() >= 2 && status != JobStatus.CANCEL_REQUESTED) {
					monitor.cancel();
				} else if (status == JobStatus.CANCEL_REQUESTED) {
					break;
				}
				try {
					TimeUnit.MILLISECONDS.sleep(500);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
			assertEquals(JobStatus.CANCEL_REQUESTED, status);
		}, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertJobInfo(jobInfo, "Cancel job", JobStatus.SUCCESS, 10);
		assertEquals(5, jobInfo.getProcessedWorkUnits(), 0);
		final Result<Serializable> result = jobInfo.getResult();
		assertNotNull(result);
		assertEquals("Ran even if canceled", result.value);
		assertEquals(Severity.OK, result.status.getSeverity());
	}

	/**
	 * Test a job that sets itself into the cancel status.
	 */
	@Test
	public void testJobCancelingItself() {
		final CancelJob job = new CancelJob(innowake.mining.job.status.CancelJob.ExecutionMode.RETURN_CANCEL_RESULT);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertJobInfo(jobInfo, "Cancel job", JobStatus.CANCELED, 10);
		assertTrue(jobInfo.getProcessedWorkUnits() >= 2);
		final Result<Serializable> result = jobInfo.getResult();
		assertNotNull(result);
		assertEquals(Severity.CANCELED, result.status.getSeverity());
	}

	/**
	 * Test a job that sets the warning status.
	 */
	@Test
	public void testJobWarning() {
		final FailureJob job = new FailureJob(innowake.mining.job.status.FailureJob.ExecutionMode.RESULT_WITH_WARNING_SEVERITY);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertJobInfo(jobInfo, "Failure job", JobStatus.SUCCESS, 10);
		assertEquals(2, jobInfo.getProcessedWorkUnits(), 0);
		final Result<Serializable> result = jobInfo.getResult();
		assertNotNull(result);
		assertNull(result.value);
		assertNull(result.status.getMessage());
		assertNull(result.status.getStackTrace());
		assertEquals(Severity.WARNING, result.status.getSeverity());
	}

	/**
	 * Test a job that sets the error status.
	 */
	@Test
	public void testJobError() {
		final FailureJob job = new FailureJob(innowake.mining.job.status.FailureJob.ExecutionMode.RESULT_WITH_ERROR_SEVERITY);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertJobInfo(jobInfo, "Failure job", JobStatus.FAILURE, 10);
		assertEquals(2, jobInfo.getProcessedWorkUnits(), 0);
		final Result<Serializable> result = jobInfo.getResult();
		assertNotNull(result);
		assertNull(result.value);
		assertNull(result.status.getMessage());
		assertNull(result.status.getStackTrace());
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}

	/**
	 * Tests a job that sets a status with an exception.
	 */
	@Test
	public void testJobErrorWithException() {
		final FailureJob job = new FailureJob(innowake.mining.job.status.FailureJob.ExecutionMode.RESULT_WITH_STATUS_EXCEPTION);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertJobInfo(jobInfo, "Failure job", JobStatus.FAILURE, 10);
		assertEquals(2, jobInfo.getProcessedWorkUnits(), 0);
		final Result<Serializable> result = jobInfo.getResult();
		assertNotNull(result);
		assertNull(result.value);
		assertEquals(Severity.ERROR, result.status.getSeverity());
		assertEquals("my special exception", result.status.getMessage());
	}

	/**
	 * Tests that we gracefully recover when a job throws an unhandled exception.
	 */
	@Test
	public void testJobErrorWithUnexpectedException() {
		final FailureJob job = new FailureJob(innowake.mining.job.status.FailureJob.ExecutionMode.THROW_UNHANDLED_EXCEPTION);
		startJobAndAwaitCompletion(job, t -> {
			assertNotNull(t);
			assertEquals("java.lang.IllegalStateException: random unhandled exception", t.getMessage());
		}, null, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertJobInfo(jobInfo, "Failure job", JobStatus.FAILURE, 10);
		assertEquals(2, jobInfo.getProcessedWorkUnits(), 0);
		final Serializable result = jobInfo.getResult();
		assertNull(result);
	}

	/**
	 * Tests that that failure status is set when job aborts with JVM Error.
	 */
	@Test
	public void testJobErrorWithError() {
		final FailureJob job = new FailureJob(innowake.mining.job.status.FailureJob.ExecutionMode.THROW_ERROR);
		startJobAndAwaitCompletion(job, t -> {
			assertNotNull(t);
			assertEquals("java.lang.Error: something has gone seriously wrong", t.getMessage());
		}, null, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertJobInfo(jobInfo, "Failure job", JobStatus.FAILURE, 10);
		assertEquals(2, jobInfo.getProcessedWorkUnits(), 0);
		final Serializable result = jobInfo.getResult();
		assertNull(result);
	}

	/**
	 * Tests a job with multiple tasks that emit different status values.
	 */
	@Test
	public void testJobWithTasks() {
		final JobWithTasks job = new JobWithTasks();
		startJobAndAwaitCompletion(job, t -> { throw new IllegalStateException(t); }, null, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertJobInfo(jobInfo, "Job with tasks", JobStatus.FAILURE, 25);
		assertEquals(3, jobInfo.getProcessedWorkUnits(), 0);
		final Result<Serializable> result = jobInfo.getResult();
		assertNotNull(result);
		assertEquals(Severity.ERROR, result.status.getSeverity());
	}

	/**
	 * Tests a job by canceling the scheduled job
	 */
	@Test
	public void testCancelRequestForScheduled() {
		final CancelJob job = new CancelJob(innowake.mining.job.status.CancelJob.ExecutionMode.CANCEL_SCHEDULED_JOB);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), monitor -> {
			JobInformation runningJobInfo;
			final Instant start = Instant.now();
			while (Duration.between(start, Instant.now()).toMinutes() < 2) { /* Break if the status didn't change after 2 minutes */
				runningJobInfo = monitor.getJobInformation();
				assertNotNull(runningJobInfo);
				if (runningJobInfo.getStatus() == JobStatus.SCHEDULED) {
					monitor.cancel();
					break;
				}
				try {
					TimeUnit.MILLISECONDS.sleep(500);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
		}, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertEquals(JobStatus.CANCELED, jobInfo.getStatus());
		final Serializable result = jobInfo.getResult();
		assertNull(result);
	}

	/**
	 * Tests a job by canceling the cancel requested job
	 */
	@Test
	public void testCancelForCancelRequest() {
		final CancelJob job = new CancelJob(innowake.mining.job.status.CancelJob.ExecutionMode.CANCEL_RUNNING_JOB);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), monitor -> {
			JobInformation runningJobInfo;
			final Instant start = Instant.now();
			while (Duration.between(start, Instant.now()).toMinutes() < 2) { /* Break if the status didn't change after 2 minutes */
				runningJobInfo = monitor.getJobInformation();
				assertNotNull(runningJobInfo);
				if (runningJobInfo.getProcessedWorkUnits() >= 2) {
					monitor.cancel();
				}
				if (runningJobInfo.getStatus() == JobStatus.CANCEL_REQUESTED) {
					monitor.cancel();
					break;
				}
				try {
					TimeUnit.MILLISECONDS.sleep(500);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
				}
			}
		}, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertEquals(JobStatus.CANCELED, jobInfo.getStatus());
		final Serializable result = jobInfo.getResult();
		assertNull(result);
	}

	@Test
	public void testDeletingCancelledJob() {
		final Job<?> job = new CancelJob(innowake.mining.job.status.CancelJob.ExecutionMode.CANCEL_RUNNING_JOB);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);

		testIfDeleted(job);
	}

	@Test
	public void testDeletingFailureJob() {
		final Job<?> job = new FailureJob(innowake.mining.job.status.FailureJob.ExecutionMode.RESULT_WITH_ERROR_SEVERITY);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);

		testIfDeleted(job);
	}

	@Test
	public void testDeletingSuccessJob() {
		final Job<?> job = new FailureJob(innowake.mining.job.status.FailureJob.ExecutionMode.RESULT_WITH_WARNING_SEVERITY);
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);

		testIfDeleted(job);
	}

	@Test
	public void testDeletingRunningJob() {
		final Job<?> job = new RunningJob();
		jobManager.submit(job);
		assertThrows(IllegalStateException.class, () -> jobManager.delete(job.getJobId()));

		final List<JobInformation> jobInfos = jobManager.getJobs(q -> q.byId(UUID.fromString(job.getJobId())));
		assertEquals(1, jobInfos.size());

		final JobMonitor monitor = innowake.lib.core.lang.Assert.assertNotNull(jobManager.getJobMonitor(job.getJobId()));
		monitor.cancel();
	}

	@Test
	public void testDeletingMultipleJobs() {
		final Job<?> cancelledJob = new CancelJob(innowake.mining.job.status.CancelJob.ExecutionMode.CANCEL_RUNNING_JOB);
		final Job<?> failureJob = new FailureJob(innowake.mining.job.status.FailureJob.ExecutionMode.RESULT_WITH_ERROR_SEVERITY);
		final Job<?> runningJob = new RunningJob();

		startJobAndAwaitCompletion(cancelledJob, t -> fail("Job execution failed: " + t.getMessage()), null, null);
		startJobAndAwaitCompletion(failureJob, t -> fail("Job execution failed: " + t.getMessage()), null, null);
		jobManager.submit(runningJob);

		jobManager.delete(q -> {});

		final List<JobInformation> jobInfos = jobManager.getJobs(q -> {});
		assertEquals(1, jobInfos.size());
		assertEquals(jobInfos.get(0).getJobId(), runningJob.getJobId());

		final JobMonitor monitor = innowake.lib.core.lang.Assert.assertNotNull(jobManager.getJobMonitor(runningJob.getJobId()));
		monitor.cancel();
	}

	private void testIfDeleted(final Job<?> job) {
		jobManager.delete(job.getJobId());

		final List<JobInformation> jobInfos = jobManager.getJobs(q -> q.byId(UUID.fromString(job.getJobId())));
		assertEquals(0, jobInfos.size());
	}

	private void assertJobInfo(final JobInformation jobInfo, final String description, final JobStatus status, final int totalWorkUnits) {
		assertNotNull(jobInfo.getDuration());
		assertNotNull(jobInfo.getFinishTime());
		assertNotNull(jobInfo.getScheduledStartTime());
		assertNotNull(jobInfo.getStartTime());
		assertNull(jobInfo.getEta());
		assertEquals(description, jobInfo.getJobDescription());
		assertEquals("myUser", jobInfo.getUserName());
		assertEquals(0, jobInfo.getPendingTasks());
		assertEquals(status, jobInfo.getStatus());
		assertEquals(totalWorkUnits, jobInfo.getTotalWorkUnits());
	}

	/**
	 * This class needs to be nested because it should use custom config. This custom config is required to simulate job timeout.
	 */
	@Nested
	@Import(Config.class)
	class TimeoutJobTest extends AbstractJobTest {

		@Autowired
		private JobInfoService jobInfoService;

		/**
		 * Tests timeout of a running job.
		 */
		@Test
		void testTimeout() {
			final Job<?> job = new TimeoutJob();
			startJobAndAwaitCompletion(job, t -> {}, null, null);

			final JobInformation jobInfo = getJobInfo(job.getJobId());
			assertNotNull(jobInfo);
			assertJobInfo(jobInfo, "Timeout job", JobStatus.TIMEOUT, 10);
			final Serializable result = jobInfo.getResult();
			assertNull(result);

			final JobInfoPojo jobInfoDb = jobInfoService.get(jobInfo.getId());
			assertJobInfo(JobInfoUtil.toJobInformation(jobInfoDb), "Timeout job", JobStatus.TIMEOUT, 10);
		}

		@Test
		void testDeletingTimeoutJob() {
			final Job<?> job = new TimeoutJob();
			startJobAndAwaitCompletion(job, t -> {}, null, null);

			testIfDeleted(job);
		}
	}

}
