/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.log;

import org.springframework.test.context.ActiveProfiles;

/**
 * Test for job {@link JobLogTestJob} testing that log paths are correctly set and that every log output contains
 * a trace and span Id.
 */
@ActiveProfiles("local_mode")
class JobLogTest extends AbstractJobLogTest {

	/* Actual tests are in the base class */
}
