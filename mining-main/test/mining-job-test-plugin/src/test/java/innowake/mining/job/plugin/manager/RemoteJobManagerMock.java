/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.manager;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.equinox.security.storage.StorageException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.client.service.job.JobServiceProviderMock;
import innowake.mining.job.plugin.progress.RemoteProgressJob;
import innowake.mining.job.plugin.progress.TestRemoteProgressJob;
import innowake.mining.shared.model.job.JobInformation;

/**
 * This extends the {@link RemoteJobManager} to be able to "mock" any REST calls during plugin-tests.
 */
public class RemoteJobManagerMock extends RemoteJobManager {
	
	private final Map<IProject, JobServiceProvider> jobServiceProviders = new HashMap<>();
	private final Map<String, TestRemoteProgressJob> remoteProgressJobs = new HashMap<>();

	/**
	 * Constructor.
	 * 
	 * @param storagePath the base {@link IPath}
	 */
	public RemoteJobManagerMock(final IPath storagePath) {
		super(storagePath);
	}
	
	@Override
	public JobServiceProvider getJobServiceProvider(final IProject project) throws CoreException, StorageException {
		return jobServiceProviders.computeIfAbsent(project, p -> {
			return new JobServiceProviderMock(new ConnectionInfo("dummyURL","dummyAccessToken"));
		});
	}
	
	@Override
	public Map<String, Set<RemoteJobInfo>> getJobs() {
		/* Use a sorted map for testing purposes to always have the same order. */
		final Map<String, Set<RemoteJobInfo>> sortedJobs = new TreeMap<>();
		for (Map.Entry<String, Set<RemoteJobInfo>> originalValues : super.getJobs().entrySet()) {
			sortedJobs.put(originalValues.getKey(), new TreeSet<>(originalValues.getValue()));
		}
		return sortedJobs;
	}
	
	@Override
	protected RemoteProgressJob getRemoteProgressJob(final JobInformation jobInfo) {
		final TestRemoteProgressJob job = new TestRemoteProgressJob(this, jobInfo);
		remoteProgressJobs.put(jobInfo.getJobId(), job);
		return job;
	}
	
	@Nullable
	@Override
	public RemoteProgressJob removeRemoteProgressJob(final String jobId) {
		remoteProgressJobs.remove(jobId);
		return super.removeRemoteProgressJob(jobId);
	}
	
	/**
	 * @return the {@link TestRemoteProgressJob} jobs created by this manager
	 */
	public Map<String, TestRemoteProgressJob> getRemoteProgressJobs() {
		return remoteProgressJobs;
	}
}
