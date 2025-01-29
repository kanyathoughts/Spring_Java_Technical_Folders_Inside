/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.dna;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import innowake.lib.job.api.ProgressMonitor;

/**
 * Extend an {@link ThreadPoolExecutor} by a method to shutdown an wait for pool termination with respect to a given monitor.
 * The thread pool executor is initialized with a pool size equal to the number of available processors.
 */
public class MonitorExecutorService extends ThreadPoolExecutor {

	/**
	 * Create a new instance of the monitored pool.
	 */
	public MonitorExecutorService() {
		super(Runtime.getRuntime().availableProcessors(), 
			Runtime.getRuntime().availableProcessors(),
            0L, 
            TimeUnit.MILLISECONDS,
            new LinkedBlockingQueue<Runnable>());
	}

	/**
	 * Request a shutdown of the pool and wait for termination.
	 * After the shutdown request, <b>this method wait forever</b> on pool termination or monitor cancel=true.
	 * If you want to interrupt the shutdown, set the {@link ProgressMonitor#isCanceled()} to return {@code true}.
	 * The pool status is checked in second interval.
	 * The method return if all pending pool work is done. 
	 *
	 * @param monitor The monitor instance to listen on cancel state
	 */
	public void shutdownAndWait(final ProgressMonitor monitor) {
        shutdown();
		while (true) {
			try {
				awaitTermination(1, TimeUnit.SECONDS);
				
				if (isTerminated()) {
					break;
				}
				
				if (monitor.isCanceled()) {
					shutdownNow();
					break;
				}
			} catch (final InterruptedException e) {
				/* Wait for the conditions "pool is done with work" or "monitor is cancelled" to become true. */
				Thread.currentThread().interrupt();
			}
		}
	}
}
