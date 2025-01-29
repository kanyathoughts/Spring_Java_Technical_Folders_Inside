/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.lib.job.internal;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.JobInfoService;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.shared.entities.JobInfoPojoPrototype;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * Updates any left over {@linkplain JobStatus#RUNNING running jobs} from a previous server run to failed.
 */
@Component
public class StaleJobCleaner {
	
	private static final Logger LOG = LoggerFactory.getLogger(StaleJobCleaner.class);

	private final JobManager jobManager;
	private final JobInfoService jobInfoService;

	/**
	 * The Constructor.
	 * 
	 * @param jobManager JobManager instance to be injected
	 * @param jobInfoService JobInfoDao instance to be injected
	 */
	@Autowired
	public StaleJobCleaner(final JobManager jobManager, final JobInfoService jobInfoService) {
		this.jobManager = jobManager;
		this.jobInfoService = jobInfoService;
	}

	/**
	 * Updates any existing job with {@linkplain JobStatus#RUNNING} to {@linkplain JobStatus#FAILURE}.
	 */
	public void cleanStaleJobs() {
		for (final JobInformation job : jobManager.getJobs(q -> q.withStatus(JobStatus.RUNNING))) {
			if (jobManager.getJobMonitor(job.getJobId()) == null) {
				/* if we can not get the job monitor for this job, then it means the job is no longer actively executing
				   => update the job status and set it to FAILED */
				LOG.info("Failing job with ID [{}]", job.getJobId());

				jobInfoService.update(new JobInfoPojoPrototype()
											.setId(job.getId())
											.setStatus(JobStatus.FAILURE));
				jobInfoService.addJobMessages(job.getId(), List.of(new Message(Message.Severity.ERROR, "This job was terminated by a server shutdown.")));
			}
		}
	}
}