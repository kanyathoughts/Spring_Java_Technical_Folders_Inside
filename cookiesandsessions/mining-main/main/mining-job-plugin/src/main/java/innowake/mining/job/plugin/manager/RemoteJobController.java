/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.manager;

import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;

import innowake.mining.job.plugin.MiningJobPlugin;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.plugin.base.IRemoteJobController;

/**
 * Implementation of the {@link IRemoteJobController} for other plugins to interact with the {@link RemoteJobManager}.
 */
public class RemoteJobController implements IRemoteJobController {

	@Override
	public void submittedJob(final IProject project, final String jobId) {
		MiningJobPlugin.getDefaultNonNull().getRemoteJobManager().addJob(project, jobId);
	}
	
	@Override
	public boolean hasManagedJobsForUrl(final String url) {
		final Map<String, Set<RemoteJobInfo>> allJobs = MiningJobPlugin.getDefaultNonNull().getRemoteJobManager().getJobs();
		return allJobs.values().stream().flatMap(rji -> rji.stream()).anyMatch(rji -> rji.getApiServer().equals(url));
	}
	
	@Override
	public void refreshManagedJobs() {
		MiningJobPlugin.getDefaultNonNull().getRemoteJobManager().updateJobInformation();
	}

}
