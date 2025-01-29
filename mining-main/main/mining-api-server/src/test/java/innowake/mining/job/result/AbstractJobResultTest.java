/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.result;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;

import innowake.mining.job.AbstractJobTest;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Abstract base class for job result tests.
 */
abstract class AbstractJobResultTest extends AbstractJobTest {
	
	protected static final String DEFAULT_RESULT_FOLDER = "jobResults";
	
	private final JobStatus expectedJobStatus;
	
	public AbstractJobResultTest(final JobStatus expectedJobStatus) {
		this.expectedJobStatus = expectedJobStatus;
	}

	@AfterAll
	public static void cleanupJobResults() throws IOException {
		FileUtils.deleteDirectory(Paths.get(DEFAULT_RESULT_FOLDER).toFile());
	}
	
	/**
	 * Runs a single job and tests the result file for correct content.
	 * @throws IOException
	 */
	@Test
	void testSingleJob() throws IOException {
		runJobAndAssertResult();
	}
	
	/**
	 * Runs multiple jobs after each other and tests that every one gets its own result file with correct content.
	 * @throws IOException
	 */
	@Test
	void testMultipleJobs() throws IOException {
		runJobAndAssertResult();
		runJobAndAssertResult();
		runJobAndAssertResult();
	}

	/**
	 * Runs {@link JobResultTestJob} and asserts that the result file exists and contains the expected content.
	 * 
	 * @throws IOException if the result file cannot be read
	 */
	protected void runJobAndAssertResult() throws IOException {
		final JobResultTestJob job = new JobResultTestJob();
		final AtomicReference<String> jobId = new AtomicReference<>("");
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, jobMonitor -> {
			jobId.set(jobMonitor.getJobId());
		});
		
		JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertEquals(expectedJobStatus, jobInfo.getStatus());
		
		if (expectedJobStatus == JobStatus.SUCCESS) {
			final Result<?> jobResult = (Result<?>) jobManager.getJobResult(jobId.get());
			assertNotNull("Expected job to return a result", jobResult);
			assertTrue("Expected job to return FileSystemResult", jobResult.value instanceof FileSystemResult);
			
			final FileSystemResult fileSystemResult = (FileSystemResult) jobResult.value;
			assertNotNull("Expected result to contain a value", fileSystemResult);
			assertEquals("text/plain", fileSystemResult.getContentType());
			assertEquals("hello.txt", fileSystemResult.getFileName());
		}
		
		final File resultFile = Paths.get(DEFAULT_RESULT_FOLDER, jobId.get()).toFile();
		assertTrue("Expected result file to exist", resultFile.exists());
		
		final String result = FileUtils.readFileToString(resultFile, StandardCharsets.UTF_8);
		assertEquals("Hello, World!", result);
	}
}
