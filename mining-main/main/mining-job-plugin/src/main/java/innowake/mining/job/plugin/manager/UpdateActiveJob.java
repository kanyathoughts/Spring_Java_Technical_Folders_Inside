/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.manager;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import innowake.lib.core.api.lang.Nullable;

/**
 * This job updates the state of the {@link RemoteJobManager} for all active remote jobs.
 */
public class UpdateActiveJob extends AbstractUpdateJob {

	/**
	 * Constructor.
	 * 
	 * @param remoteJobManager the {@link RemoteJobManager} instance
	 */
	public UpdateActiveJob(final RemoteJobManager remoteJobManager) {
		super(remoteJobManager);
	}

	@Override
	protected IStatus run(@Nullable final IProgressMonitor monitor) {
		remoteJobManager.updateActiveJobInformation();
		
		return Status.OK_STATUS;
	}
}
