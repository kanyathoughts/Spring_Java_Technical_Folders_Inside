/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.job.fields;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.JobInfoService;
import innowake.lib.job.api.management.JobInformation;
import innowake.mining.job.AbstractJobTest;

/**
 * Tests the name of the job
 */
class JobNameTest extends AbstractJobTest {

	@Autowired
	private JobInfoService jobInfoService;

	@Test
	void customJobName() {
		executeAndTest(new CustomNameJob(), "Example");
	}

	@Test
	void defaultJobName() {
		executeAndTest(new NoNameJob(), "No Name");
	}

	void executeAndTest(final Job<?> job, final String expectedJobName) {
		startJobAndAwaitCompletion(job, t -> fail("Job execution failed: " + t.getMessage()), null, null);

		final JobInformation jobInfo = getJobInfo(job.getJobId());
		assertNotNull(jobInfo);
		assertEquals(expectedJobName, jobInfo.getJobName());

		final var jobInfoDb = jobInfoService.get(jobInfo.getId());
		assertEquals(expectedJobName, jobInfoDb.getName());
	}

}
