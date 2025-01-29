/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.lang.ProcessBuilder.Redirect;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.annotation.DirtiesContext;

import brave.Tracer;
import brave.Tracer.SpanInScope;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.junit.extend.TestLevelCondition;
import innowake.mining.server.MiningApiApplication;
import innowake.mining.server.integration.DatabaseRelatedTest;

/**
 * Abstract base class for job tests.
 */
@DirtiesContext
@EnableConfigurationProperties
@WithMockUser(username = "myUser")
@ExtendWith(TestLevelCondition.class)
public abstract class AbstractJobTest extends DatabaseRelatedTest {

	@Autowired
	@Qualifier(JobConfiguration.JOB_MANAGER_ID)
	protected JobManager jobManager;

	@Autowired
	protected Tracer tracer;
	@Nullable
	protected SpanInScope rootScope;

	@BeforeEach
	protected void onBefore() {
		rootScope = tracer.withSpanInScope(tracer.newTrace());
	}

	@AfterEach
	protected void onAfter() {
		if (rootScope != null) {
			rootScope.close();
		}
	}

	/**
	 * @return {@code true} if the test should be executed in a cluster or {@code false} for only on a single node
	 */
	protected boolean runInCluster() {
		return false;
	}

	/**
	 * @return the spring profile name to use for the additional cluster process
	 */
	protected String getClusterProfileName() {
		return "cluster_mode";
	}

	/**
	 * @param jobId the job Id
	 * @return the {@link JobInformation} for the provided job Id
	 */
	protected JobInformation getJobInfo(final String jobId) {
		final List<JobInformation> jobInfos = jobManager.getJobs(q -> q.byId(UUID.fromString(jobId)));
		assertEquals(1, jobInfos.size());
		return jobInfos.get(0);
	}

	/**
	 * Starts the provided job and waits until the execution is finished. Depending on {@link #runInCluster()} it will either run on a single node or
	 * on two nodes in a cluster.
	 *
	 * @param job the {@link Job} to execute
	 * @param onError this {@link Consumer} will be triggered if there was any error during the job execution
	 * @param operationBeforeWait this {@link Consumer} will be triggered before the current thread is blocked to wait for the job execution to finish
	 * @param operationAfterWait this {@link Consumer} will be triggered right after the current thread continues to execute after receiving the signal that the
	 * job execution has finished
	 * @return the {@link JobMonitor} of the started job
	 */
	protected JobMonitor startJobAndAwaitCompletion(final Job<?> job, final Consumer<Throwable> onError,
			@Nullable final Consumer<JobMonitor> operationBeforeWait, @Nullable final Consumer<JobMonitor> operationAfterWait) {
		if (runInCluster()) {
			return startJobInClusterAndAwaitCompletion(getClusterProfileName(), job, onError, operationBeforeWait, operationAfterWait);
		} else {
			return startJobWithoutClusterAndAwaitCompletion(job, onError, operationBeforeWait, operationAfterWait);
		}
	}

	/**
	 * Starts the provided job on a single node and waits until the execution is finished.
	 *
	 * @param job the {@link Job} to execute
	 * @param onError this {@link Consumer} will be triggered if there was any error during the job execution
	 * @param operationBeforeWait this {@link Consumer} will be triggered before the current thread is blocked to wait for the job execution to finish
	 * @param operationAfterWait this {@link Consumer} will be triggered right after the current thread continues to execute after receiving the signal that the
	 * job execution has finished
	 * @return the {@link JobMonitor} of the started job
	 */
	protected JobMonitor startJobWithoutClusterAndAwaitCompletion(final Job<?> job, final Consumer<Throwable> onError,
			@Nullable final Consumer<JobMonitor> operationBeforeWait, @Nullable final Consumer<JobMonitor> operationAfterWait) {
		final CountDownLatch latch = new CountDownLatch(1);
		final Throwable[] error = new Throwable[1];
		final JobMonitor jobMonitor = jobManager.submit(job, new JobExecutionCallback() {

			@Override
			public void onCompletion() {
				latch.countDown();
			}

			@Override
			public void onFailure(@Nullable final Throwable throwable) {
				error[0] = throwable;
				latch.countDown();
			}
		});

		if (operationBeforeWait != null) {
			operationBeforeWait.accept(jobMonitor);
		}

		try {
			latch.await(5, TimeUnit.MINUTES);
		} catch (final InterruptedException e) {
			throw new IllegalStateException(e);
		}

		if (error[0] != null) {
			onError.accept(error[0]);
		}

		if (operationAfterWait != null) {
			operationAfterWait.accept(jobMonitor);
		}

		return jobMonitor;
	}

	/**
	 * Starts the provided job on a cluster of two nodes and waits until the execution is finished.
	 *
	 * @param profileName the name of the spring profile to apply to the process of the second cluster node
	 * @param job the {@link Job} to execute
	 * @param onError this {@link Consumer} will be triggered if there was any error during the job execution
	 * @param operationBeforeWait this {@link Consumer} will be triggered before the current thread is blocked to wait for the job execution to finish
	 * @param operationAfterWait this {@link Consumer} will be triggered right after the current thread continues to execute after receiving the signal that the
	 * job execution has finished
	 * @return the {@link JobMonitor} of the started job
	 */
	protected JobMonitor startJobInClusterAndAwaitCompletion(final String profileName, final Job<?> job, final Consumer<Throwable> onError,
			@Nullable final Consumer<JobMonitor> operationBeforeWait, @Nullable final Consumer<JobMonitor> operationAfterWait) {
		final List<String> command = new ArrayList<>();
		command.add(System.getProperty("java.home") + File.separator + "bin" + File.separator + "java");
		command.add("-cp");
		command.add(System.getProperty("java.class.path"));
		command.add(MiningApiApplication.class.getName());
		command.add("--spring.profiles.active=" + profileName);

		JobMonitor jobMonitor = null;
		try {
			final ProcessBuilder pb = new ProcessBuilder(command);
			final Process process = pb.redirectError(Redirect.INHERIT).start();

			try (final BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream(), Charset.defaultCharset()));
					final Writer writer = new OutputStreamWriter(process.getOutputStream(), Charset.defaultCharset())) {
				final CountDownLatch startLatch = new CountDownLatch(2);
				new Thread(new InputRunnable(reader, startLatch)).start();
				try {
					startLatch.await(2, TimeUnit.MINUTES);
				} catch (final InterruptedException e) {
					Thread.currentThread().interrupt();
					fail("Interrupted while waiting for second cluster node to start up");
				}

				jobMonitor = startJobWithoutClusterAndAwaitCompletion(job, onError, operationBeforeWait, operationAfterWait);

				process.destroy();
			} catch (final IOException e) {
				throw new IllegalStateException("Unable to start second cluster node.", e);
			} finally {
				if (process != null) {
					process.destroyForcibly();
				}
			}
		} catch (final IOException e) {
			throw new IllegalStateException("Unable to start second cluster node.", e);
		}

		return jobMonitor;
	}

	/**
	 * This runnable is being used to read the InputStream from the second cluster node process. This is mainly
	 * required to check if the cluster actually started correct and to grab the output of the second process.
	 */
	public static class InputRunnable implements Runnable {

		private final BufferedReader reader;
		private final CountDownLatch startLatch;

		public InputRunnable(final BufferedReader reader, final CountDownLatch startLatch) {
			this.reader = reader;
			this.startLatch = startLatch;
		}

		@Override
		public void run() {
			try {
				String line;
				while ((line = reader.readLine()) != null) {
					if (line.startsWith("Members {size:2")) {
						startLatch.countDown();
					} else if (line.endsWith("is STARTED")) {
						startLatch.countDown();
					}
					/* All other output of the process is just printed. */
					System.out.println("SECOND NODE: " + line);
				}
			} catch (final IOException e) {
				throw new IllegalStateException(e);
			}
		}
	}
}
