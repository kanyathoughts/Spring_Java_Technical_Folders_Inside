/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.job.file;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.FileJob;
import innowake.lib.job.api.JobExecutionCallback;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Severity;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.mining.job.AbstractJobTest;

/**
 * Test for local {@link FileJob FileJobs}
 */
@TestInstance(Lifecycle.PER_CLASS) /* for the cleanUp() method */
class LocalFileTest extends AbstractJobTest {

	private static final String CONTENT = "This is the content for the FileJob: %s";

	@Autowired
	protected transient JobConfigurationProperties jobConfigurationProperties;

	@Override
	protected boolean runInCluster() {
		return false;
	}

	@AfterAll
	void cleanUp() throws IOException {
		FileUtils.deleteDirectory(new File("./" + jobConfigurationProperties.getJobResultFolder()));
	}

	@Test
	void testCreateReadDeleteFile() throws IOException {
		final LocalFileJob job = new LocalFileJob(jobConfigurationProperties);
		job.createTestFile(String.format(CONTENT, job.getJobId()));

		/* test that the test file was created with the right content */
		assertFileExists(job);

		job.deleteTestFile();

		/* test that the test file was deleted */
		assertFileMissing(job);
	}

	@Test
	void testSingleJob() throws IOException {
		final CountDownLatch latch = new CountDownLatch(1);
		final LocalFileJob job = createAndSubmit(createExecutionCallback(latch));

		try {
			latch.await(5, TimeUnit.MINUTES);
		} catch (final InterruptedException e) {
			throw new IllegalStateException(e);
		}

		assertJobDone(job);
	}

	/**
	 * Runs multiple jobs after each other and tests that every one gets its own log file with correct content
	 * @throws IOException
	 */
	@Test
	void testMultipleJobs() throws IOException {
		final LocalFileJob[] jobs = new LocalFileJob[4];
		final CountDownLatch latch = new CountDownLatch(jobs.length);

		for (int i = 0; i < jobs.length; i++) {
			jobs[i] = createAndSubmit(createExecutionCallback(latch));
		}

		try {
			latch.await(5, TimeUnit.MINUTES);
		} catch (final InterruptedException e) {
			throw new IllegalStateException(e);
		}

		for (int i = 0; i < jobs.length; i++) {
			assertJobDone(jobs[i]);
		}
	}

	private static JobExecutionCallback createExecutionCallback(final CountDownLatch latch) {
		return new JobExecutionCallback() {

			@Override
			public void onFailure(@Nullable final Throwable throwable) {
				fail(throwable);
			}

			@Override
			public void onCompletion() {
				latch.countDown();
			}
		};
	}

	private LocalFileJob createAndSubmit(final JobExecutionCallback callback) throws IOException {
		final LocalFileJob job = new LocalFileJob(jobConfigurationProperties);
		job.createTestFile(String.format(CONTENT, job.getJobId()));

		/* test that file is created immediately */
		assertFileExists(job);

		jobManager.submit(job, callback);

		return job;
	}

	private void assertFileExists(final LocalFileJob job) throws IOException {
		final String content = String.format(CONTENT, job.getJobId());
		assertEquals(content, job.getTestFile());
	}

	private void assertFileMissing(final LocalFileJob job) throws IOException {
		try {
			job.getTestFile();
			fail("File must not exist anymore");
		} catch (final java.nio.file.NoSuchFileException exception) {
			/* expected */
		}
	}

	private void assertJobDone(final LocalFileJob job) throws IOException {
		/* test that the job finished successfully */
		@SuppressWarnings("unchecked")
		final Result<Serializable> result = (Result<Serializable>) jobManager.getJobResult(job.getJobId());
		assertNotNull("Job result must not be null", result);
		assertEquals(Severity.OK, result.status.getSeverity(), "Job must be in status SUCCESS");

		/* test that the test file of the job was deleted */
		assertFileMissing(job);

		/* test that the job was able to read the test file and to return its content with the job result */
		final String content = String.format(CONTENT, job.getJobId());
		assertEquals(content, result.value);
	}
}
