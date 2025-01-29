/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import static innowake.lib.core.lang.Assert.assertNotNull;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobMonitor;
import innowake.mining.shared.model.job.JobStatus;

/**
 * This updates the job heartbeat as long as the job is in the {@link JobStatus#RUNNING} state.
 * All jobs not sending a heartbeat any more are treated as dead.
 */
public class HeartbeatRunnable implements Runnable {
	
	private static final Logger LOG_HEARTBEAT = LoggerFactory.getLogger(Logging.JOB_HEARTBEAT);

	private final JobMonitor monitor;
	private final Tracer tracer;
	private final Span jobSpan;

	/**
	 * Constructor.
	 * 
	 * @param monitor the {@link JobMonitor} of the job
	 * @param tracer the {@link Tracer}
	 * @param jobSpan the {@link Span} of the job
	 */
	public HeartbeatRunnable(final JobMonitor monitor, final Tracer tracer, final Span jobSpan) {
		this.monitor = monitor;
		this.tracer = tracer;
		this.jobSpan = jobSpan;
	}

	@Override
	public void run() {
		/* Runs in the same scope as the job itself */
		try (final Tracer.SpanInScope parentScope = tracer.withSpanInScope(assertNotNull(jobSpan))) {
			if (monitor.getStatus() == JobStatus.RUNNING) {
				LOG_HEARTBEAT.trace(() -> "Sending heartbeat for job with Id: " + monitor.getJobId());
				monitor.updateHeartbeat();
			}
		}
	}
	
	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE).append("jobId", monitor.getJobId()).toString();
	}
}
