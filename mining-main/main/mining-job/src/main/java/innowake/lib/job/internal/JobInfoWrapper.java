/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import java.io.Serializable;
import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.management.JobInformation;
import innowake.mining.shared.entities.JobInfoPojo;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * Non modifiable {@link JobInformation} wrapper for {@link JobInfoPojo}.
 */
class JobInfoWrapper implements JobInformation {

	private final JobInfoPojo jobInfo;
	private final Instant submitTime;

	JobInfoWrapper(final JobInfoPojo jobInfo) {
		this.jobInfo = jobInfo;
		this.submitTime = jobInfo.getSubmitTime().orElse(Instant.now());
	}

	@Override
	public UUID getId() {
		return jobInfo.getId();
	}

	@Override
	public String getJobName() {
		return jobInfo.getName();
	}

	@Override
	public String getUserName() {
		return jobInfo.getCreatedByUserId();
	}

	@Nullable
	@Override
	public String getJobDescription() {
		return jobInfo.getDescription().orElse(null);
	}

	@Nullable
	@Override
	public String getStepDescription() {
		return jobInfo.getStepDescription().orElse(null);
	}

	@Override
	public JobStatus getStatus() {
		return jobInfo.getStatus().orElse(JobStatus.UNKNOWN);
	}

	@Override
	public Instant getSubmitTime() {
		return submitTime;
	}

	@Nullable
	@Override
	public Instant getScheduledStartTime() {
		return jobInfo.getScheduledStartTime().orElse(null);
	}

	@Nullable
	@Override
	public Instant getStartTime() {
		return jobInfo.getStartTime().orElse(null);
	}

	@Nullable
	@Override
	public Instant getFinishTime() {
		return jobInfo.getFinishTime().orElse(null);
	}

	@Nullable
	@Override
	public Result<Serializable> getResult() {
		return JobInfoUtil.deserializeObject(jobInfo.getResult().orElse(null));
	}

	@Override
	public int getPendingTasks() {
		return jobInfo.getPendingTasks();
	}

	@Override
	public int getTotalWorkUnits() {
		return jobInfo.getTotalWorkUnits();
	}

	@Override
	public double getProcessedWorkUnits() {
		return jobInfo.getProcessedWorkUnits();
	}

	@Override
	public List<Message> getMessages() {
		return Collections.unmodifiableList(jobInfo.getMessages());
	}
}
