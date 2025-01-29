/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.internal.Logging;


/**
 * Replacement class of org.eclipse.core.runtime.NullProgressMonitor.
 */
public class NullProgressMonitor implements ProgressMonitor {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.JOB_MONITOR);	

	/**
	 * Constructs a new progress monitor.
	 */
	public NullProgressMonitor() {
		super();
	}
	
	@Override
	public void setJobDescription(final String description) {
		/* do nothing */
		checkCanceled();
	}

	@Override
	public void setStepDescription(final String stepDescription) {
		/* do nothing */
		checkCanceled();
	}

	@Override
	public void begin(int workUnits) {
		/* do nothing */
	}
	
	/**
	 * This implementation does nothing.
	 * Subclasses may override this method.
	 * 
	 * @see ProgressMonitor#internalWork(double)
	 */
	@Override
	public void internalWork(final double work) {
		LOG.trace(() -> "Updated sub progress monitor with work" + work);
	}

	@Override
	public void cancel() {
		/* do nothing */
	}

	@Override
	public boolean isCanceled() {
		return false;
	}

}
