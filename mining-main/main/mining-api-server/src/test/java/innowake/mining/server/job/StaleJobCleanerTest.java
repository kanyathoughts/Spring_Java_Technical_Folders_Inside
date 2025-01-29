/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.JobInfoService;
import innowake.lib.job.internal.JobInfo;
import innowake.lib.job.internal.JobInfoUtil;
import innowake.lib.job.internal.StaleJobCleaner;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * Test class for StaleJobCleaner component.
 */
class StaleJobCleanerTest extends DatabaseRelatedTest {

	@Autowired
	private JobInfoService jobInfoService;

	@Autowired
	private StaleJobCleaner staleJobCleaner;

	@Test
	void testStaleJobCleaner() {
		final var jobInfo = new JobInfo(UUID.randomUUID().toString(), "Stale Cleaner", "admin");
		jobInfo.setStartTime(Instant.now());
		jobInfo.setJobDescription("Test Stale Job Cleaner");
		jobInfo.setScheduledStartTime(Instant.now());
		jobInfo.setFinishTime(Instant.now());
		jobInfo.setStatus(JobStatus.RUNNING);
		jobInfoService.upsert(JobInfoUtil.toPrototype(jobInfo));

		staleJobCleaner.cleanStaleJobs();

		final var jobInformation = jobInfoService.get(jobInfo.getId());
		assertEquals(JobStatus.FAILURE, jobInformation.getStatus().orElse(null));
		final List<Message> messages = jobInformation.getMessages();
		assertEquals(1, messages.size());
		assertEquals(Message.Severity.ERROR, messages.get(0).getSeverity());
	}
}
