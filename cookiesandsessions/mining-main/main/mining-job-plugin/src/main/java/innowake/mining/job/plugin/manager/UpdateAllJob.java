/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.manager;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import innowake.lib.core.api.lang.Nullable;

/**
 * This job updates the state of the {@link RemoteJobManager} for all remote jobs currently known.
 */
public class UpdateAllJob extends AbstractUpdateJob {
	
	/**
	 * Constructor.
	 * 
	 * @param remoteJobManager the {@link RemoteJobManager} instance
	 */
	public UpdateAllJob(final RemoteJobManager remoteJobManager) {
		super(remoteJobManager);
	}

	
	@Override
	protected IStatus run(@Nullable final IProgressMonitor monitor) {
		remoteJobManager.updateJobInformation();
		
		return Status.OK_STATUS;
	}
}
