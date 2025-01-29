/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.job.status;

import java.util.concurrent.TimeUnit;

import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Primary;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;


/**
 * This job alone does not really cause timeout but with combination of {@link Config} it will cause timeout.
 */
public class TimeoutJob extends Job<String> {

	@Override
	protected Result<String> run(final ProgressMonitor progressMonitor) {
		progressMonitor.begin(10);
		progressMonitor.setJobDescription("Timeout job");

		try {
			TimeUnit.SECONDS.sleep(3);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		return new Result<String>(Status.OK, "Ran even if timeout");
	}

	/**
	 * Overrides the default job config to simulate job timeout.
	 */
	@Primary
	@TestConfiguration
	public static class Config extends JobConfigurationProperties {

		@Override
		public int getMaximumHeartbeatAge() {
			return 0;
		}

		@Override
		public int getJobHeartbeatInterval() {
			return 1;
		}
	}
}
