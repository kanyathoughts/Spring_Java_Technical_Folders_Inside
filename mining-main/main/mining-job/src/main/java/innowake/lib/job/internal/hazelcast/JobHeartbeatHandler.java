/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.hazelcast;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.time.Instant;
import java.util.Map.Entry;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import com.hazelcast.map.IMap;
import com.hazelcast.spring.context.SpringAware;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.management.JobManager;
import innowake.lib.job.internal.JobInfoUtil;
import innowake.lib.job.internal.JobConfiguration;
import innowake.lib.job.internal.JobInfo;
import innowake.lib.job.internal.Logging;
import innowake.lib.job.internal.executor.SerializableRunnable;
import innowake.mining.shared.model.job.JobStatus;
import innowake.lib.job.api.JobInfoService;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;

/**
 * This handler ensures that jobs are properly removed from the cluster for which no heartbeat
 * has been received for a specified amount of time, effectively declaring them as dead.
 */
@SpringAware
public class JobHeartbeatHandler implements SerializableRunnable {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.JOB_HEARTBEAT);
	private static final String LOCK_NAME = "job-heartbeat-lock";

	@Autowired
	@Qualifier(JobConfiguration.JOBS_ID)
	private transient IMap<String, JobInfo> jobs;
	@Autowired
	private transient JobInfoService jobInfoService;
	@Autowired
	private transient JobManager jobManager;
	@Autowired
	private transient Tracer tracer;
	@Autowired
	private transient JobConfigurationProperties jobProperties;

	@Nullable
	private transient Span span;

	@PostConstruct
	private void postConstruct() {
		/* We have to do this after deserialization and injection is done. */
		if (span == null) {
			span = tracer.newTrace();
		}
	}

	@Override
	public void run() {
		/* All executions will run in the scope of the same trace, but always with a new span. */
		try (final Tracer.SpanInScope rootScope = tracer.withSpanInScope(assertNotNull(span))) {
			final Span currentExecSpan = tracer.nextSpan().start();
			try (final Tracer.SpanInScope currentExecScope = tracer.withSpanInScope(currentExecSpan)) {
				try {
					jobs.lock(LOCK_NAME); /* As this runs on all cluster nodes, we have to ensure that only one executes this logic at the same time. */
					LOG.trace(() -> "Handling job heartbeats");
					final Instant oldestAccepted = Instant.now().minusSeconds(jobProperties.getMaximumHeartbeatAge());

					for (final Entry<String, JobInfo> entry : jobs.entrySet()) {
						final JobInfo jobInfo = entry.getValue();
						if (jobInfo.getStatus() != JobStatus.SCHEDULED && jobInfo.getLastHeartbeat().isBefore(oldestAccepted)) {
							LOG.warn(() -> "No heartbeat received since " + oldestAccepted + ". Removing job " + jobInfo.getJobId());

							/* Only update and store if it's not already present in the database. */
							if (jobInfoService.findAny(q -> q.byId(jobInfo.getId())).isEmpty()) {
								if (jobInfo.getFinishTime() == null) {
									jobInfo.setFinishTime(Instant.now());
								}
								jobInfo.setStatus(JobStatus.TIMEOUT);

								jobInfoService.upsert(JobInfoUtil.toPrototype(jobInfo));
								jobManager.getClusterInformation().modifyActiveJobCount(-1);
							}

							jobs.remove(entry.getKey());
						}
					}
				} finally {
					jobs.unlock(LOCK_NAME);
					LOG.trace(() -> "Done handling job heartbeats");
				}
			} catch (final Exception e) {
				currentExecSpan.error(e);
				LOG.error(() -> "Unable to handle job heartbeats.", e);
			} finally {
				currentExecSpan.finish();
			}
		} finally {
			assertNotNull(span).flush();
		}
	}
}
