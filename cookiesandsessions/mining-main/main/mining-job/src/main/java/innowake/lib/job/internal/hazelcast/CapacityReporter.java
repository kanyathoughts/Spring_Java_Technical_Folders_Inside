/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.hazelcast;

import static innowake.lib.core.lang.Assert.assertNotNull;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;

import com.hazelcast.spring.context.SpringAware;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.internal.Logging;
import innowake.lib.job.internal.MemberInfoRecord;
import innowake.lib.job.internal.executor.SerializableRunnable;

/**
 * This runnable is executed periodically on all cluster nodes to update the internal cluster status information.
 */
@SpringAware
public class CapacityReporter implements SerializableRunnable {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.CLUSTER_INFO);
	
	@Autowired
	private transient MemberInfoRecord memberInfoRecord;
	@Autowired
	private transient HzClusterInformation clusterInformation;
	@Autowired
	private transient Tracer tracer;
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
				memberInfoRecord.updateTimestamp();
				clusterInformation.update(memberInfoRecord);
				LOG.debug(clusterInformation::toString);
			} catch (final Exception e) {
				currentExecSpan.error(e);
			} finally {
				currentExecSpan.finish();
			}
		} finally {
			assertNotNull(span).flush();
		}
	}

}
