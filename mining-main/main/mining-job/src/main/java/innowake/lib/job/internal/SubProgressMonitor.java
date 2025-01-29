/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;

/**
 * Sub {@link ProgressMonitor} that reports fractional deltas to its parent.
 */
public class SubProgressMonitor implements ProgressMonitorInternal {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.JOB_MONITOR);
	
	private final ProgressMonitor parent;
	private final int workUnitsFromParent;
	private int workUnitsInThis;
	
	/**
	 * Creates a new sub monitor on a parent. This takes an amount of work units from the parent and maps them to the work units in this instance.
	 * For example it could take 1 work unit from the parent and partition this to 10 local work units. Each {@link #worked(int)} with value {@code 1}
	 * would increase the parent by {@code 0.1}.
	 * 
	 * @param parent the parent monitor
	 * @param workUnitsFromParent the work units to take from the parent
	 */
	public SubProgressMonitor(final ProgressMonitor parent, final int workUnitsFromParent) {
		this.parent = parent;
		this.workUnitsFromParent = workUnitsFromParent;
		this.workUnitsInThis = INDETERMINISTIC;
	}
	
	@Override
	public final void setJobDescription(final String description) {
		checkCanceled();
		parent.setJobDescription(description);
	}
	
	@Override
	public void setStepDescription(final String description) {
		checkCanceled();
		parent.setStepDescription(description);
	}
	
	@Override
	public void begin(final int workUnits) {
		workUnitsInThis = workUnits;
	}
	
	@Override
	public final void internalWork(final double work) {
		final double parentWorked = (workUnitsFromParent * work) / workUnitsInThis;
		parent.internalWork(parentWorked);
		LOG.trace(() -> "Updated sub progress monitor: " + this);
	}

	@Override
	public final void cancel() {
		parent.cancel();
	}

	@Override
	public final boolean isCanceled() {
		return parent.isCanceled();
	}
	
	@Override
	public void initialize(final JobManagerInternal jobManager) {
		/* The progress monitors are serialized/deserialized between hazelcast nodes, but hazelcast only runs auto injection on the
		 * actual Runnables or Callables being executed. Therefore we have to trigger manual injection here to ensure that all progress
		 * monitors in the chain are properly initialized. */
		if (parent instanceof ProgressMonitorInternal) {
			((ProgressMonitorInternal) parent).initialize(jobManager);
		}
	}
	
	@Override
	public final String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE).append("parent", parent).append("workUnitsFromParent", workUnitsFromParent)
				.append("workUnitsInThis", workUnitsInThis).toString();
	}
	
}
