/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.health;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.stereotype.Component;

import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Health indicator that keeps track of jobs.
 */
@Component("job information")
public class JobInformationHealthIndicator implements HealthIndicator {

	@Autowired
	private JobManager jobManager;

	@SuppressWarnings("boxing")
	@Override
	public Health health() {
		List<JobInformation> allJobs = jobManager.getJobs(q -> {});
		return Health.up().withDetail("total_jobs", allJobs.size())
				.withDetail("scheduled_jobs", allJobs.stream().filter(j -> j.getStatus() == JobStatus.SCHEDULED).count())
				.withDetail("running_jobs", allJobs.stream().filter(j -> j.getStatus() == JobStatus.RUNNING).count())
				.withDetail("successful_jobs", allJobs.stream().filter(j -> j.getStatus() == JobStatus.SUCCESS).count())
				.withDetail("failed_jobs", allJobs.stream().filter(j -> j.getStatus() == JobStatus.FAILURE).count())
				.withDetail("timed_out_jobs", allJobs.stream().filter(j -> j.getStatus() == JobStatus.TIMEOUT).count())
				.withDetail("cancel_requested_jobs", allJobs.stream().filter(j -> j.getStatus() == JobStatus.CANCEL_REQUESTED).count())
				.withDetail("canceled_jobs", allJobs.stream().filter(j -> j.getStatus() == JobStatus.CANCELED).count())
				.withDetail("unknown_status_jobs", allJobs.stream().filter(j -> j.getStatus() == JobStatus.UNKNOWN).count())
				.withDetail("pending_tasks", allJobs.stream().mapToInt(JobInformation::getPendingTasks).sum())
				.build();
	}
}
