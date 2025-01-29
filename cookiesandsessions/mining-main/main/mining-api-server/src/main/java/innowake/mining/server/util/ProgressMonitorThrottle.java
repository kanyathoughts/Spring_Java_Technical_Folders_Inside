/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.util;

import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import com.google.common.util.concurrent.Futures;

import innowake.lib.job.api.ProgressMonitor;

/**
 * Throttles calls to {@link ProgressMonitor progress monitors}
 */
public class ProgressMonitorThrottle {
	
	public static final int DESCRIPTION_THROTTLE_INTERVAL = 2000;
	private static final ProgressMonitorThrottle INSTANCE = new ProgressMonitorThrottle();

	private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();
	private Future<?> descriptionFuture;
	
	private ProgressMonitorThrottle() {
		descriptionFuture = Futures.immediateFuture(null);
	}
	
	/**
	 * Throttles the given step description on the progress monitor.
	 * <p>
	 * The throttling interval is set to {@value #DESCRIPTION_THROTTLE_INTERVAL} ms.
	 *
	 * @param description the step description to throttle
	 * @param monitor the progress monitor to use
	 */
	public static synchronized void throttleStepDescription(final String description, final ProgressMonitor monitor) {
		INSTANCE.throttleDescription(description, monitor);
	}

	private void throttleDescription(final String description, final ProgressMonitor monitor) {
		if (descriptionFuture.isDone()) {
			descriptionFuture = scheduler.schedule(() -> monitor.setStepDescription(description), DESCRIPTION_THROTTLE_INTERVAL, TimeUnit.MILLISECONDS);
		}
	}

	/**
	 * A wrapper class that throttles calls to {@link ProgressMonitor progress monitors}.
	 * <p>Throttle support is limited to {@link ProgressMonitor#setStepDescription(String)} for now.</p>
	 */
	public static class ThrottlingProgressMonitor implements ProgressMonitor {

		private final ProgressMonitor progressMonitor;

		/**
		 * Adds throttling support to the given {@code progressMonitor}.
		 * 
		 * @param progressMonitor The {@link ProgressMonitor} to wrap
		 */
		public ThrottlingProgressMonitor(final ProgressMonitor progressMonitor) {
			this.progressMonitor = progressMonitor;
		}

		@Override
		public void setJobDescription(final String description) {
			progressMonitor.setJobDescription(description);
		}

		@Override
		public void setStepDescription(final String description) {
			ProgressMonitorThrottle.throttleStepDescription(description, progressMonitor);
		}

		@Override
		public void begin(final int workUnits) {
			progressMonitor.begin(workUnits);
		}

		@Override
		public void internalWork(final double work) {
			progressMonitor.internalWork(work);
		}

		@Override
		public void cancel() {
			progressMonitor.cancel();
		}

		@Override
		public boolean isCanceled() {
			return progressMonitor.isCanceled();
		}
		
		@Override
		public void worked(final int workUnits) {
			progressMonitor.worked(workUnits);
		}
		
		@Override
		public void checkCanceled() {
			progressMonitor.checkCanceled();
		}
		
		@Override
		public ProgressMonitor subMonitor(final int workUnitsFromParent) {
			return progressMonitor.subMonitor(workUnitsFromParent);
		}
	}
}
