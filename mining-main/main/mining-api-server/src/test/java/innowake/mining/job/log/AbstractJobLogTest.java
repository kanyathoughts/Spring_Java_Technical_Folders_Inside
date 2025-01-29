/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.log;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.junit.jupiter.api.Test;

import brave.propagation.TraceContext;
import innowake.mining.job.AbstractJobTest;

/**
 * Abstract base class for job log tests.
 */
abstract class AbstractJobLogTest extends AbstractJobTest {
	
	/**
	 * Runs a single job and tests the log file for correct content.
	 * @throws IOException
	 */
	@Test
	void testSingleJob() throws IOException {
		runJobAndAssertLog();
	}
	
	/**
	 * Runs multiple jobs after each other and tests that every one gets its own log file with correct content
	 * @throws IOException
	 */
	@Test
	void testMultipleJobs() throws IOException {
		runJobAndAssertLog();
		runJobAndAssertLog();
		runJobAndAssertLog();
	}
	
	/**
	 * Tests that the internal log specific system properties are being set.
	 */
	@Test
	void testSystemProperties() {
		assertEquals(getExpectedLogPath(), System.getProperty("job-api.log.logFolder"));
		assertEquals("job-", System.getProperty("job-api.log.logFilePrefix"));
	}
	
	protected String getExpectedLogPath() {
		return "logs";
	}

	/**
	 * Runs {@link JobLogTestJob} and asserts that the log file exists and that every line contains a trace and span Id.
	 * 
	 * @throws IOException if the log file cannot be read
	 */
	protected void runJobAndAssertLog() throws IOException {
		final JobLogTestJob job = new JobLogTestJob();
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);
		
		final TraceContext context = tracer.currentSpan().context();
		final Optional<Path> logFile = Files.list(Paths.get(getExpectedLogPath()))
				.filter(file -> FilenameUtils.getBaseName(file.getFileName().toString()).endsWith(job.getJobId()))
				.findFirst();
		assertTrue(logFile.isPresent());
		final List<String> lines = FileUtils.readLines(logFile.get().toFile(), StandardCharsets.UTF_8);
		lines.forEach(line -> {
			assertTrue(line.contains(context.traceIdString()));
			assertTrue(line.contains(context.spanIdString()));
		});
	}
}
