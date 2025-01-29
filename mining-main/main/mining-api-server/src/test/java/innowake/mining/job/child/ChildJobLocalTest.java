/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.child;

import org.springframework.test.context.ActiveProfiles;

/**
 * Test for jobs {@link ParentJobRunningChildJob } and {@link ChildJob } testing that the child job runs without any issues inside the parent job.
 * 
 */
@ActiveProfiles("local_mode")
class ChildJobLocalTest extends AbstractParentChildJobTest {

	/* Actual tests are in the base class */
}
