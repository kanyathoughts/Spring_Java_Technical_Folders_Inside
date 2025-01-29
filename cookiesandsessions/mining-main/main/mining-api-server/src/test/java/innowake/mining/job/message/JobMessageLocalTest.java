/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.message;

import org.springframework.test.context.ActiveProfiles;

/**
 * Tests for job and task message handling on a single node.
 */
@ActiveProfiles("local_mode")
public class JobMessageLocalTest extends AbstractJobMessageTest {
	
	@Override
	protected boolean runInCluster() {
		return false;
	}
	
}
