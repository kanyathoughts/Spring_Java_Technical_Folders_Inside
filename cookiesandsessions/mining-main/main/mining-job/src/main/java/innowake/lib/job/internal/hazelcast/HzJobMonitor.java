/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.hazelcast;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Boxing.box;

import java.text.DecimalFormat;
import java.time.Instant;
import java.util.Map.Entry;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.hazelcast.map.EntryProcessor;
import com.hazelcast.map.IMap;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobInformation;
import innowake.lib.job.api.management.JobMonitor;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.job.internal.JobInfo;
import innowake.lib.job.internal.JobManagerInternal;
import innowake.lib.job.internal.Logging;
import innowake.lib.job.internal.ProgressMonitorInternal;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;

/**
 * {@link JobMonitor} implementation for jobs that are executed within a Hazelcast cluster.
 */
public class HzJobMonitor implements JobMonitor, ProgressMonitorInternal {
	
	private static final long serialVersionUID = 1L;
	private static final Logger LOG = LoggerFactory.getLogger(Logging.JOB_MONITOR);

	@Autowired
	@Qualifier(JobConfiguration.JOBS_ID)
	@Nullable /* can be null when this class is deserialized on a different cluster node without triggering the injection. */
	private transient IMap<String, JobInfo> jobs; /* transient as we don't want this to be serialized/deserialized between the cluster nodes. */

	@Nullable
	private String jobId;

	@Override
	public final void setJobDescription(final String description) {
		checkCanceled();
		modify(jobInfo -> jobInfo.setJobDescription(description));
	}
	
	@Override
	public void setStepDescription(final String description) {
		checkCanceled();
		modify(jobInfo -> jobInfo.setStepDescription(description));
	}
	
	@Override
	public final void begin(final int ticks) {
		modify(jobInfo -> jobInfo.setTotalWorkUnits(ticks));
	}
	
	@Override
	public final int modifyPendingTasks(final int delta) {
		return modifyAndGet(jobInfo -> jobInfo.modifyPendingTasks(delta)).getPendingTasks();
	}

	@Override
	public final int getPendingTasks() {
		final JobInformation jobInfo = getJobInformation();
		return jobInfo != null ? jobInfo.getPendingTasks() : 0;
	}

	@Override
	public final String getJobId() {
		return assertNotNull(jobId);
	}
	
	@Nullable
	@Override
	public final JobInformation getJobInformation() {
		return getJobs().get(jobId);
	}
	
	@Override
	public final void setStatus(final JobStatus status) {
		modify(jobInfo -> jobInfo.nextStatus(status));
	}
	
	@Override
	public final JobStatus getStatus() {
		final IMap<String, JobInfo> jobs = getJobs();
		if (jobs.containsKey(jobId)) {
			return jobs.get(jobId).getStatus();
		}
		return JobStatus.UNKNOWN;
	}
	
	@Override
	public final void setScheduledStartTime(final Instant scheduledStartTime) {
		modify(jobInfo -> jobInfo.setScheduledStartTime(scheduledStartTime));
		setStatus(JobStatus.SCHEDULED);
	}

	@Override
	public final void setStartTime(final Instant startTime) {
		modify(jobInfo -> jobInfo.setStartTime(startTime));
	}

	@Override
	public final void setFinishTime(final Instant finishTime) {
		modify(jobInfo -> jobInfo.setFinishTime(finishTime));
	}
	
	@Override
	public final void setProcessedWorkUnits(final int workedTicks) {
		modify(jobInfo -> jobInfo.setProcessedWorkUnits(workedTicks));
	}

	@Override
	public final void setTotalWorkUnits(final int totalTicks) {
		modify(jobInfo -> jobInfo.setTotalWorkUnits(totalTicks));
	}
	
	@Override
	public final void addMessage(final Message message) {
		modify(jobInfo -> jobInfo.addMessage(message));
	}
	
	@Override
	public final void updateHeartbeat() {
		modify(JobInfo::updateHeartbeat);
	}

	@Override
	public final int getTotalWorkUnits() {
		final IMap<String, JobInfo> jobs = getJobs();
		if (jobs.containsKey(jobId)) {
			return jobs.get(jobId).getTotalWorkUnits();
		}
		return 0;
	}
	
	@Override
	public final void internalWork(final double delta) {
		if (getTotalWorkUnits() != INDETERMINISTIC) {
			final JobInfo jobInformation = modifyAndGet(jobInfo -> jobInfo.internalWork(delta));
			if (LOG.isDebugEnabled()) {
				final int total = jobInformation.getTotalWorkUnits();
				final double worked = jobInformation.getProcessedWorkUnits();
				LOG.debug("Updated job progress: Id=" + jobId + " total=" + format4(total) + " adding=" + format4(delta)
					+ " worked=" + format4(worked) + " (" + format2(worked * 100 / total) + "%, ETA: " + jobInformation.getEta() + ")");
			}
		}
	}

	@Override
	public final void cancel() {
		setStatus(JobStatus.CANCEL_REQUESTED);
	}

	@Override
	public final boolean isCanceled() {
		return getStatus() == JobStatus.CANCEL_REQUESTED || getStatus() == JobStatus.CANCELED;
	}
	
	@Override
	public final String toString() {
		final JobInfo data = getJobs().get(jobId);
		final ToStringBuilder sb = new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE).append("jobId", jobId);
		if (data != null) {
			sb.append("status", data.getStatus()).append("total", data.getTotalWorkUnits()).append("worked", new DecimalFormat("#.##").format(data.getProcessedWorkUnits()));
		}
		return sb.toString();
	}

	@Override
	public final void destroy() {
		LOG.debug(() -> "Destroying the job monitor " + this);
		getJobs().remove(jobId);
	}
	
	@Override
	public void initialize(final JobManagerInternal jobManager) {
		/* This class is serialized/deserialized between hazelcast nodes, but hazelcast only runs auto injection on the
		 * actual Runnables or Callables being executed. Therefore we have to trigger manual injection here. */
		if (jobs == null) {
			jobManager.prepareBean(this);
		}
	}
	
	/**
	 * Sets the {@link JobInfo} relevant for this monitor. This also populates the internal Hazelcast job map.
	 *
	 * @param jobInfo the {@link JobInfo}
	 */
	public final void setJobInfo(final JobInfo jobInfo) {
		this.jobId = jobInfo.getJobId().toString();
		getJobs().putIfAbsent(jobId, jobInfo);
	}
	
	private void modify(final Modifier modifier) {
		final IMap<String, JobInfo> jobs = getJobs();
		if (jobs.containsKey(jobId)) {
			jobs.executeOnKey(jobId, modifier);
		} else {
			throw new IllegalStateException("Cannot modify properties of job with Id " + jobId + " as it has already been disposed from the cluster.");
		}
	}
	
	private JobInfo modifyAndGet(final Modifier modifier) {
		final IMap<String, JobInfo> jobs = getJobs();
		if (jobs.containsKey(jobId)) {
			return jobs.executeOnKey(jobId, modifier);
		}
		throw new IllegalStateException("Cannot modify properties of job with Id " + jobId + " as it has already been disposed from the cluster.");
	}
	
	private IMap<String, JobInfo> getJobs() {
		return assertNotNull(jobs);
	}
	
	private static String format2(final double value) {
		return String.format("%3.2f", box(value));
	}
	
	private static String format4(final double value) {
		return String.format("%3.4f", box(value));
	}
	
	private static interface Modifier extends EntryProcessor<String, JobInfo, JobInfo> {

		@Override
		default JobInfo process(@Nullable final Entry<String, JobInfo> entryToProcess) {
			final Entry<String, JobInfo> entry = assertNotNull(entryToProcess);
			final JobInfo jobInfo = entry.getValue();
			modify(jobInfo);
			/* The modified value must always explicitly be put back to the map to be populated in the Hazelcast cluster. */
			entry.setValue(jobInfo);
			return jobInfo;
		}
		
		void modify(final JobInfo jobInfo);
		
	}

}
