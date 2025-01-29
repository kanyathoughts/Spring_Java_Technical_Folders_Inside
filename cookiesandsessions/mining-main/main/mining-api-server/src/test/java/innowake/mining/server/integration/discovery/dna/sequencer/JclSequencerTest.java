/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery.dna.sequencer;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.tags.DiscoveryTest;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * Tests the DNA strings of {@link Technology#JCL}.
 */
@DiscoveryTest
public class JclSequencerTest extends AbstractSequencerTest {

	@Override
	protected Tuple2<Technology, Type> getTechnologyType() {
		return Tuple2.of(Technology.JCL, Type.JOB);
	}

	@Override
	protected String getFolder() {
		return "JCL";
	}

	@Override
	protected void preProcess() {
		super.preProcess();
		submitJob(jobManager, tracer, new DiscoverCodeJob(PROJECT_ID));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(PROJECT_ID, false));
	}

	private static String submitJob(final JobManager jobManager, final Tracer tracer, final Job<?> job) {
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
					throw new IllegalStateException(
							"CountDownLatch timed out in JobController.submitJob(), possible deadlock! (" + latch.toString() + ")");
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
