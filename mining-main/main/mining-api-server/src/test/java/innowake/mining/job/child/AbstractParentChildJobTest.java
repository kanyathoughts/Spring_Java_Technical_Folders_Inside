/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.child;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import innowake.lib.core.lang.Assert.AssertionException;
import innowake.lib.core.lang.Nullable;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.job.AbstractJobTest;

/**
 * Abstract base class to test submit child job running inside the parent Job.
 */
abstract class AbstractParentChildJobTest extends AbstractJobTest {

	@Mock
	@Nullable
	JobMonitor JobMonitor;

	/**
	 * Runs {@link ParentJobRunningChildJob } and tests whether child job runs without any issues.
	 */
	@Test
	void testJobSubmitFromParentJob() {
		final ParentJobRunningChildJob job = new ParentJobRunningChildJob();
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);
	}

	/**
	 * Test submitFromJobAndWait with {@link ChildJob } without the parent Job running and assert on exception error message.
	 */
	@Test
	void testSubmitFromJobWithoutParentJob() {
		final ChildJob childJob = new ChildJob();
		final Throwable exception = assertThrows(AssertionException.class, () -> jobManager.submitFromJobAndWait(childJob, assertNotNull(JobMonitor)));
		assertEquals("Parent job must be running", exception.getMessage());
	}
}
