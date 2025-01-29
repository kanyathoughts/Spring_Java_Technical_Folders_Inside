/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.progress;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.junit.Assert;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager;
import innowake.mining.shared.model.job.JobInformation;

/**
 * This extends the original {@link RemoteProgressJob} to get hold of
 * additional information for testing purposes.
 */
public class TestRemoteProgressJob extends RemoteProgressJob {
	
	@Nullable
	private ProgressMonitorDecorator progressMonitorDecorator;

	/**
	 * Constructor.
	 * 
	 * @param remoteJobManager the {@link RemoteJobManager} instance
	 * @param jobInfo the initial {@link JobInformation} instance
	 */
	public TestRemoteProgressJob(final RemoteJobManager remoteJobManager, final JobInformation jobInfo) {
		super(remoteJobManager, jobInfo);
	}
	
	/**
	 * @return the {@link ProgressMonitorDecorator}
	 */
	@Nullable
	public ProgressMonitorDecorator getProgressMonitorDecorator() {
		return progressMonitorDecorator;
	}
	
	@Override
	protected IStatus run(@Nullable IProgressMonitor monitor) {
		Assert.assertNotNull(monitor);
		progressMonitorDecorator = new ProgressMonitorDecorator(monitor);
		return super.run(progressMonitorDecorator);
	}

}
