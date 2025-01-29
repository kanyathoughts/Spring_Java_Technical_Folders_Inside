/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.test.plugin;

import static org.junit.Assert.assertNotNull;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.equinox.security.storage.StorageException;
import org.junit.After;
import org.junit.Before;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.service.job.GetJobInfoMock;
import innowake.mining.client.service.job.GetJobInfosMock;
import innowake.mining.job.plugin.manager.RemoteJobManager;
import innowake.mining.job.plugin.manager.RemoteJobManagerMock;
import innowake.mining.shared.model.job.JobInformation;

/**
 * Abstract base class for remote job tests.
 */
public abstract class AbstractJobTest {
	
	protected final String projectName;
	
	@Nullable
	protected IProject testProject;
	@Nullable
	protected RemoteJobManager remoteJobManager;
	
	/**
	 * Constructor.
	 * 
	 * @param projectName the name of the test project to create
	 */
	public AbstractJobTest(final String projectName) {
		this.projectName = projectName;
	}

	/**
	 * Creates a plain empty project to store files during the test execution and prepares the {@link RemoteJobManagerMock}.
	 * 
	 * @throws CoreException if there was an error
	 */
	@Before
	public void prepareTest() throws CoreException {
		final IProject project = testProject = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		if (project.exists()) {
			project.delete(true, null);
		}
		project.create(null);
		project.open(null);
		
		/* prepare the REST mocks to return the two JobInformation instances. */
		final RemoteJobManagerMock manager = new RemoteJobManagerMock(project.getLocation());
		initGetJobInfoMock(manager);
		
		this.remoteJobManager = manager;
	}
	
	/**
	 * Cancels all jobs started during the test and deletes the test project.
	 * 
	 * @throws CoreException if the test project could not be deleted
	 */
	@After
	public void cleanUp() throws CoreException {
		if (remoteJobManager != null) {
			remoteJobManager.stop();
		}
		if (testProject != null) {
			testProject.delete(true, null);
		}
	}
	
	/**
	 * Used to initialize the {@link GetJobInfoMock} to mock the initial {@link JobInformation} instances returned by newly added jobs to the test.
	 * 
	 * @param remoteJobManager the {@link RemoteJobManagerMock} instance
	 */
	protected abstract void initGetJobInfoMock(final RemoteJobManagerMock remoteJobManager);
	
	/**
	 * Sets the {@link JobInformation} instances that should be returned by the {@link GetJobInfosMock} mock
	 * 
	 * @param jobInfos the {@link JobInformation} instances
	 * @throws CoreException if anything goes wrong
	 * @throws StorageException if anything goes wrong
	 */
	protected void setGetJobInfosMock(final JobInformation... jobInfos) throws CoreException, StorageException {
		final RemoteJobManager jobManager = remoteJobManager;
		final IProject project = testProject;
		assertNotNull(jobManager);
		assertNotNull(project);
		final GetJobInfosMock getJobInfos = (GetJobInfosMock) ((RemoteJobManagerMock) jobManager).getJobServiceProvider(project).getJobInfos();
		
		getJobInfos.clearJobInfoToReturn();
		for (final JobInformation jobInfo : jobInfos) {
			getJobInfos.addJobInfoToReturn(jobInfo);
		}
	}
}
