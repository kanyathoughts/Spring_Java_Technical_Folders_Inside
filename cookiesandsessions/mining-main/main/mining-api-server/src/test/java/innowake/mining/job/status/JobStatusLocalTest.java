/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.status;

import org.springframework.test.context.ActiveProfiles;

import innowake.lib.junit.Config;
import innowake.lib.junit.Level;

/**
 * Tests for job status handling on a single node.
 */
@ActiveProfiles("local_mode")
@Config(level = Level.INTEGRATION)
public class JobStatusLocalTest extends AbstractJobStatusTest {
	
	@Override
	protected boolean runInCluster() {
		return false;
	}

}
