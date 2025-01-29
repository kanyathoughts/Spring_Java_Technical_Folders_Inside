/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.result;

import org.springframework.test.context.ActiveProfiles;

import innowake.lib.junit.Config;
import innowake.lib.junit.Level;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Test for job {@link JobResultTestJob} testing that results can be stored and retrieved from the file system.
 */
@ActiveProfiles("cluster_mode")
@Config(level = Level.INTEGRATION)
class JobResultClusterTest extends AbstractJobResultTest {
	
	public JobResultClusterTest() {
		super(JobStatus.SUCCESS);
	}

	@Override
	protected boolean runInCluster() {
		return true;
	}
	
	@Override
	protected String getClusterProfileName() {
		return "cluster_mode";
	}
}
