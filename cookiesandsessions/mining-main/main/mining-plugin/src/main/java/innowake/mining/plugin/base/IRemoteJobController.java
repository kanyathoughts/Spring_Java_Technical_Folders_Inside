/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.base;

import org.eclipse.core.resources.IProject;

/**
 * This interface correlates with the {@code innowake.mining.plugin.remoteJobExtension} extension
 * point to enable this plugin to interact with the remote job manager without having a direct dependency.
 */
public interface IRemoteJobController {

	/**
	 * Called upon submitting a remote job for execution, providing the unique job Id.
	 * 
	 * @param project the {@link IProject} used to submit the job
	 * @param jobId the unique Id of the submitted job
	 */
	public void submittedJob(IProject project, String jobId);
	
	/**
	 * Returns if there are currently any managed jobs in this workspace for the provided {@code url}.
	 * 
	 * @param url the api-server URL to check for managed jobs
	 * @return {@code true} if there managed jobs for the URL; {@code false} otherwise
	 */
	public boolean hasManagedJobsForUrl(String url);
	
	/**
	 * Refreshes all jobs managed by this workspace. This will also cleanup all jobs not known to the currently
	 * configured api-server URLs.
	 */
	public void refreshManagedJobs();
}
