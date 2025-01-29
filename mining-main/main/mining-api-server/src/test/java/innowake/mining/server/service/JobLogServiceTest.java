/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
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

import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.lib.job.api.config.properties.LogProperties;
import innowake.lib.job.internal.JobConfiguration;
import innowake.mining.server.integration.DatabaseRelatedTest;

/**
 * Tests for {@link MiningJobService}.
 */
class JobLogServiceTest extends DatabaseRelatedTest {

	@Autowired
	private JobConfigurationProperties jobConfigProperties;
	@Autowired
	private MiningJobService miningJobService;
	@Autowired
	@Qualifier(JobConfiguration.NODE_NAME)
	private String nodeName;

	@Test
	void testCleanupLogDirectory() throws IOException {
		final LogProperties properties = jobConfigProperties.getLog();
		final String logFolder = properties.getLogFolder();
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

	@Test
	void testGetJobLog() throws IOException {
		final LogProperties properties = jobConfigProperties.getLog();
		final File logFile = Paths.get(properties.getLogFolder(), properties.getLogFilePrefix() + "job1.log").toFile();
		FileUtils.writeStringToFile(logFile, "Log output for job 1", Charset.forName("Cp1252"));

		final Map<String, String> log = miningJobService.getJobLog("job1");

		assertFalse(log.isEmpty());
		assertTrue(log.containsKey(nodeName));
		assertEquals("Log output for job 1", log.get(nodeName));

		logFile.delete();
	}

	@Test
	void testGetJobNotPresent() throws IOException {
		final Map<String, String> log = miningJobService.getJobLog("doesnotexist");
		assertTrue(log.isEmpty());
	}
}
