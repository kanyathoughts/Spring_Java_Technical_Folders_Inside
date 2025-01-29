/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.result;

import org.springframework.test.context.ActiveProfiles;

import innowake.mining.shared.model.job.JobStatus;

/**
 * Test for job {@link JobResultTestJob} testing that results can be stored and retrieved from the file system.
 */
@ActiveProfiles("local_mode")
class JobResultTest extends AbstractJobResultTest {

	/* Actual tests are in the base class */
	
	public JobResultTest() {
		super(JobStatus.SUCCESS);
	}

}
