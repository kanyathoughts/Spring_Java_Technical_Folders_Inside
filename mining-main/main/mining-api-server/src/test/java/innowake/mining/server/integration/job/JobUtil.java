/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.job;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.management.JobManager;

/**
 * Utility class providing a method for submitting a job.
 */
public class JobUtil {
	
	private JobUtil() {
		/* Hide implicit constructor */
	}
	
	/**
	 * Submits the provided {@code job} for execution.
	 *
	 * @param jobManager the {@link JobManager}
	 * @param tracer the {@link Tracer}
	 * @param job the {@link Job} to submit
	 *
	 * @return the jobId
	 */
	public static String submitJob(final JobManager jobManager, final Tracer tracer, final Job<?> job) {
		final Span rootSpan = tracer.newTrace();
		String jobId = "";
		try (final Tracer.SpanInScope scope = tracer.withSpanInScope(rootSpan)) {
			final CountDownLatch latch = new CountDownLatch(1);
			final Throwable[] error = new Throwable[1];
			jobId = jobManager.submit(job, new JobExecutionCallback() {

				@Override
				public void onCompletion() {
					latch.countDown();
				}

				@Override
				public void onFailure(@Nullable final Throwable throwable) {
					error[0] = throwable;
					latch.countDown();
				}
			}).getJobId();

			try {
				final boolean countReachedZero = latch.await(10, TimeUnit.MINUTES);
				if ( ! countReachedZero) {
					throw new IllegalStateException("CountDownLatch timed out in JobUtil.submitJob(), possible deadlock! (" + latch.toString() + ")");
				}
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}

			if (error[0] != null) {
				throw new IllegalStateException(error[0]);
			}
		} finally {
			rootSpan.finish();
		}
		return jobId;
	}
}
