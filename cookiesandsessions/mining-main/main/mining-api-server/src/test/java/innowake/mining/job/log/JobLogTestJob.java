/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.log;

import java.io.Serializable;

import org.springframework.test.context.ActiveProfiles;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;

/**
 * The test job for writing logs into file and Job Log.
 */
@ActiveProfiles("local_mode")
public class JobLogTestJob extends Job<Serializable> {

	private static final Logger LOG = LoggerFactory.getLogger("test.siftingFile");

	@Override
	protected Result<Serializable> run(final ProgressMonitor progressMonitor) {
		LOG.info("Start logging");
		progressMonitor.begin(1);
		progressMonitor.setJobDescription("Custom Job for sifting file appender");
		for (int i = 0; i < 3; i++) {
			LOG.info("Doing someting " + i);
		}

		LOG.info("End logging");
		return new Result<>(Status.OK);
	}
}
