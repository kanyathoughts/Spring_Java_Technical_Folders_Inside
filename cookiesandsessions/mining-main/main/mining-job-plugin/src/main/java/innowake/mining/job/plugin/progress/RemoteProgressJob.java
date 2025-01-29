/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.progress;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobGroup;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.job.plugin.manager.RemoteJobManager;
import innowake.mining.job.plugin.view.labelprovider.TimeColumnLabelProvider;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * This eclipse job represents the progress of an active remote job.
 */
public class RemoteProgressJob extends Job {
	
	/* for example: Mar 25, 2020 10:02:33 AM */
	private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM).withZone(ZoneId.systemDefault());
	
	private final RemoteJobManager remoteJobManager;
	
	private JobInformation jobInfo;
	private String lastJobDescription = "";
	private String lastStepDescription = "";
	private JobStatus lastStatus = JobStatus.UNKNOWN;
	private boolean cancelRequested = false;
	private boolean isOffline = false;
	private boolean forceFinish = false;

	/**
	 * Constructor.
	 * 
	 * @param remoteJobManager the {@link RemoteJobManager} instance
	 * @param jobInfo the initial {@link JobInformation} instance
	 */
	public RemoteProgressJob(final RemoteJobManager remoteJobManager, final JobInformation jobInfo) {
		super("");
		setJobGroup(new JobGroup("remote-job-" + jobInfo.getJobId(), 1, 1));
		this.remoteJobManager = remoteJobManager;
		this.jobInfo = jobInfo;
	}
	
	/**
	 * Sets a new {@link JobInformation} instance to be used to update this jobs state.
	 * 
	 * @param jobInfo the {@link JobInformation} instance
	 */
	public void setJobInfo(final JobInformation jobInfo) {
		this.jobInfo = jobInfo;
	}
	
	/**
	 * Marks the job as offline if set to {@code true}, as the server can't be reached for any updates.
	 * 
	 * @param isOffline {@code true} to mark offline; {@code false} otherwise
	 */
	public void setOffline(final boolean isOffline) {
		this.isOffline = isOffline;
	}
	
	/**
	 * Sets if the job should be forcefully finished by exiting its main loop, independent from the state of the remote job.
	 * <p>
	 * This will not and must not send any cancel requests to the remote job. This is just for cleanup in eclipse.
	 * 
	 * @param forceFinish {@code true} to forcefully finish the execution; {@code false} to continue based on the remote job state
	 */
	public void setForceFinish(final boolean forceFinish) {
		this.forceFinish = forceFinish;
		this.cancelRequested = true; /* Avoid that a cancel request is being send to the remote job. */
	}

	@Override
	protected IStatus run(@Nullable final IProgressMonitor monitor) {
		final IProgressMonitor progressMonitor = Assert.assertNotNull(monitor);
		progressMonitor.beginTask("", IProgressMonitor.UNKNOWN);
		
		while ( ! forceFinish) {
			if (progressMonitor.isCanceled() && ! cancelRequested) {
				/* Job has been explicitly canceled by a user action in the progress view. Eclipse already marks such a job as canceled,
				 * so we don't need to manually set any cancel information in the jobs name. */
				remoteJobManager.requestJobCancel(jobInfo.getJobId());
				cancelRequested = true;
			}
			
			final JobStatus jobStatus = jobInfo.getStatus();
			/* Eclipse job has been canceled, but we wait until the actual job status has also transitioned to the cancel state. */
			final boolean isCanceled = progressMonitor.isCanceled() && jobStatus == JobStatus.CANCELED;
			/* A job is seen as active as long as it's scheduled, running or requested to cancel (but did not cancel yet) */
			final boolean isActive = RemoteJobManager.isJobInActiveState(jobInfo);
			if (isCanceled || ! isActive) {
				remoteJobManager.removeRemoteProgressJob(jobInfo.getJobId());
				break;
			}
			
			handleJobName();
			handleStepName(progressMonitor);
			
			try {
				TimeUnit.SECONDS.sleep(1);
			} catch (final InterruptedException e) {
				Thread.currentThread().interrupt();
			}
		}
		
		return Status.OK_STATUS;
	}
	
	private void handleJobName() {
		final JobStatus jobStatus = jobInfo.getStatus();
		final String jobDescription = jobInfo.getJobDescription();
		if (lastStatus != jobStatus || ! lastJobDescription.equals(jobDescription)) {
			final StringBuilder nameBuilder = new StringBuilder();
			nameBuilder.append("Remote job '").append(jobInfo.getJobId()).append("'");
			if (jobDescription != null) {
				nameBuilder.append(": ").append(jobDescription);
			}
			
			switch (jobStatus) {
				case SCHEDULED:
					final Instant scheduledStartTime = jobInfo.getScheduledStartTime();
					if (scheduledStartTime != null && scheduledStartTime.isAfter(Instant.now())) {
						/* Job is scheduled to start at a future time. */
						nameBuilder.append(" (Starts at ").append(DATE_TIME_FORMATTER.format(scheduledStartTime)).append(")");
					} else {
						/* Job is scheduled to start with the next available slot. */
						nameBuilder.append(" (Scheduled)");
					}
					break;
				case RUNNING:
					final Instant startTime = jobInfo.getStartTime();
					if (startTime != null) {
						/* Job is either in indeterministic state or has not done any work yet. */
						nameBuilder.append(" (Running since ").append(TimeColumnLabelProvider.getElapsedTime(startTime)).append(")");
					}
					break;
				case CANCEL_REQUESTED:
					if ( ! cancelRequested) {
						/* The eclipse job already provides cancel information in case the user clicked on the cancel button.
						 * This case is just if someone remotely canceled the job, but the job did not yet cancel. */
						nameBuilder.append(" (Canceled remotely)");
					}
					break;
				default:
					break;
			}
			
			if (isOffline) {
				nameBuilder.append(" (Offline)");
			}
			
			final String jobName = nameBuilder.toString();
			setName(jobName);
			lastJobDescription = jobName;
		}
	}
	
	private void handleStepName(final IProgressMonitor progressMonitor) {
		final String stepDescription = jobInfo.getStepDescription();
		if ( ! lastStepDescription.equals(stepDescription)) {
			progressMonitor.subTask(stepDescription);
			lastStepDescription = stepDescription != null ? stepDescription : "";
		}
	}
	
}
