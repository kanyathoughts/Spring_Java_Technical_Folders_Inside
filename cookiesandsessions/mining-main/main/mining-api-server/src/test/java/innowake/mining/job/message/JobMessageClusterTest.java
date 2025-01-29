/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.message;

import org.springframework.test.context.ActiveProfiles;

import innowake.lib.junit.Config;
import innowake.lib.junit.Level;

/**
 * Tests for job and task message handling in a cluster.
 */
@ActiveProfiles("cluster_mode")
@Config(level = Level.INTEGRATION)
public class JobMessageClusterTest extends AbstractJobMessageTest {
	
	@Override
	protected boolean runInCluster() {
		return true;
	}
	
	@Override
	protected String getClusterProfileName() {
		return "cluster_mode";
	}

}
