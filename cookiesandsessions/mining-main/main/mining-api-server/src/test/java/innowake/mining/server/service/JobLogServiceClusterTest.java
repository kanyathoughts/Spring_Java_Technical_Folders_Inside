/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ActiveProfiles;

import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.config.properties.LogProperties;
import innowake.lib.job.internal.JobConfiguration;
import innowake.mining.server.integration.DatabaseRelatedTest;

/**
 * Tests for {@link MiningJobService} when running in a cluster. This test actually doesn't start a cluster,
 * as only a single instance is being used but the configuration is for a cluster environment that will show
 * a different behavior.
 */
@ActiveProfiles("cluster_mode")
class JobLogServiceClusterTest extends DatabaseRelatedTest {

	@Autowired
	private JobConfigurationProperties jobConfigProperties;
	@Autowired
	private MiningJobService miningJobService;
	@Autowired
	@Qualifier(JobConfiguration.NODE_NAME)
	private String nodeName;

	/**
	 * Tests that all logs of the current node are properly cleaned up.
	 * 
	 * @throws IOException if the files cannot be deleted
	 */
	@Test
	void testCleanupLogDirectory() throws IOException {
		final LogProperties properties = jobConfigProperties.getLog();
		final String logFolder = properties.getLogFolder() + "/" + nodeName;
		final String logFilePrefix = properties.getLogFilePrefix();
		FileUtils.writeStringToFile(Paths.get(logFolder, logFilePrefix + "job1.log").toFile(), "Log output for job 1", Charset.forName("Cp1252"));
		FileUtils.writeStringToFile(Paths.get(logFolder, logFilePrefix + "job2.log").toFile(), "Log output for job 2", Charset.forName("Cp1252"));
		FileUtils.writeStringToFile(Paths.get(logFolder, logFilePrefix + "job3.log").toFile(), "Log output for job 3", Charset.forName("Cp1252"));
		FileUtils.writeStringToFile(Paths.get(logFolder, "not-a-job.log").toFile(), "Not a job log file", Charset.forName("Cp1252"));

		miningJobService.cleanupLogDirectory();

		assertEquals(0, Files.walk(Paths.get(logFolder)).filter(p -> p.startsWith(logFilePrefix)).count());
		assertTrue(Paths.get(logFolder, "not-a-job.log").toFile().exists());
		Paths.get(logFolder, "not-a-job.log").toFile().delete();
	}

	/**
	 * Tests that the {@link MiningJobService} properly resolves the job log from two different node directories.
	 * 
	 * @throws IOException if the logs cannot be accessed
	 */
	@Test
	void testGetJobLogs() throws IOException {
		final LogProperties properties = jobConfigProperties.getLog();
		final File logFile = Paths.get(properties.getLogFolder() + "/" + nodeName, properties.getLogFilePrefix() + "job1.log").toFile();
		FileUtils.writeStringToFile(logFile, "Log output for job 1 on " + nodeName, Charset.forName("Cp1252"));
		
		final File logFile1 = Paths.get(properties.getLogFolder() + "/someNode", properties.getLogFilePrefix() + "job1.log").toFile();
		FileUtils.writeStringToFile(logFile1, "Log output for job 1 on someNode", Charset.forName("Cp1252"));

		final Map<String, String> logs = miningJobService.getJobLog("job1");

		assertEquals(2, logs.size());
		final String logContent = logs.get(nodeName);
		assertNotNull(logContent);
		assertEquals("Log output for job 1 on " + nodeName, logContent);
		
		final String logContent1 = logs.get("someNode");
		assertNotNull(logContent1);
		assertEquals("Log output for job 1 on someNode", logContent1);

		logFile.delete();
		logFile1.delete();
	}
}
