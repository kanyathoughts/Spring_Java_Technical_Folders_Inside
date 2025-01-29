/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.action;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.equinox.security.storage.StorageException;
import org.junit.Test;

import innowake.mining.client.service.job.GetJobInfoMock;
import innowake.mining.job.plugin.manager.RemoteJobManager;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.manager.RemoteJobManagerMock;
import innowake.mining.job.test.plugin.AbstractJobTest;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Tests for the {@link ClearAction}.
 *
 * If you encounter a StorageException when executing this test,
 * please try adding
 * -eclipse.password ./src/test/resources/.eclipse_master_password
 * to your run config Program arguments.
 */
public class ClearActionTest extends AbstractJobTest {

	private final JobInformation jobInfoSuccessNoResult;
	private final JobInformation jobInfoFailure;
	private final JobInformation jobInfoRunning;
	private final JobInformation jobInfoCanceling;
	private final JobInformation jobInfoScheduled;
	private final JobInformation jobInfoCanceled;

	/**
	 * Constructor.
	 */
	public ClearActionTest() {
		super("clear-action-project");

		jobInfoSuccessNoResult = new JobInformation.Builder()
				.setJobId("adad5155-f5c8-461f-969f-5716e3077066")
				.setJobName("test")
				.setUserName("admin")
				.setStatus(JobStatus.SUCCESS)
				.build();

		jobInfoFailure = new JobInformation.Builder()
				.setJobId("a6638a47-47c4-40e9-b7ad-b1b8b57d0466")
				.setJobName(jobInfoSuccessNoResult.getJobName())
				.setUserName(jobInfoSuccessNoResult.getUserName())
				.setStatus(JobStatus.FAILURE)
				.build();

		jobInfoRunning = new JobInformation.Builder()
				.setJobId("0705aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccessNoResult.getJobName())
				.setUserName(jobInfoSuccessNoResult.getUserName())
				.setStatus(JobStatus.RUNNING)
				.build();

		jobInfoCanceling = new JobInformation.Builder()
				.setJobId("0905aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccessNoResult.getJobName())
				.setUserName(jobInfoSuccessNoResult.getUserName())
				.setStatus(JobStatus.CANCEL_REQUESTED)
				.build();

		jobInfoScheduled = new JobInformation.Builder()
				.setJobId("0975aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccessNoResult.getJobName())
				.setUserName(jobInfoSuccessNoResult.getUserName())
				.setStatus(JobStatus.SCHEDULED)
				.build();

		jobInfoCanceled = new JobInformation.Builder()
				.setJobId("0945aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccessNoResult.getJobName())
				.setUserName(jobInfoSuccessNoResult.getUserName())
				.setStatus(JobStatus.CANCELED)
				.build();
	}

	@Override
	protected void initGetJobInfoMock(final RemoteJobManagerMock remoteJobManager) {
		/* prepare the REST mocks to return the test JobInformation instances. */
		try {
			assertNotNull(testProject);
			final GetJobInfoMock getJobInfo = (GetJobInfoMock) remoteJobManager.getJobServiceProvider(testProject).getJobInfo();
			getJobInfo.addJobInfoToReturn(jobInfoSuccessNoResult.getJobId(), jobInfoSuccessNoResult);
			getJobInfo.addJobInfoToReturn(jobInfoFailure.getJobId(), jobInfoFailure);
			getJobInfo.addJobInfoToReturn(jobInfoRunning.getJobId(), jobInfoRunning);
			getJobInfo.addJobInfoToReturn(jobInfoCanceling.getJobId(), jobInfoCanceling);
			getJobInfo.addJobInfoToReturn(jobInfoScheduled.getJobId(), jobInfoScheduled);
			getJobInfo.addJobInfoToReturn(jobInfoCanceled.getJobId(), jobInfoCanceled);
		} catch (final CoreException | StorageException e) {
			throw new IllegalStateException(e);
		}
	}

	/**
	 * Tests that the clear action removes all jobs that are not in an active state.
	 */
	@Test
	public void testClearAction() {
		assertNotNull(remoteJobManager);
		final RemoteJobManager jobManager = remoteJobManager;
		final IProject project = testProject;
		assertNotNull(project);

		jobManager.addJob(project, jobInfoSuccessNoResult.getJobId());
		jobManager.addJob(project, jobInfoFailure.getJobId());
		jobManager.addJob(project, jobInfoRunning.getJobId());
		jobManager.addJob(project, jobInfoCanceling.getJobId());
		jobManager.addJob(project, jobInfoScheduled.getJobId());
		jobManager.addJob(project, jobInfoCanceled.getJobId());
		final ClearAction clearAction = new ClearAction(jobManager);

		clearAction.run();
		final Map<String, Set<RemoteJobInfo>> allJobs = jobManager.getJobs();
		final Set<RemoteJobInfo> clearedJobs = allJobs.get(project.getName());
		assertEquals(3, clearedJobs.size());
		assertTrue(clearedJobs.stream().anyMatch(rji -> rji.getJobInfo().getJobId().equals(jobInfoRunning.getJobId())));
		assertTrue(clearedJobs.stream().anyMatch(rji -> rji.getJobInfo().getJobId().equals(jobInfoCanceling.getJobId())));
		assertTrue(clearedJobs.stream().anyMatch(rji -> rji.getJobInfo().getJobId().equals(jobInfoScheduled.getJobId())));

		assertFalse(clearedJobs.stream().anyMatch(rji -> rji.getJobInfo().getJobId().equals(jobInfoSuccessNoResult.getJobId())));
		assertFalse(clearedJobs.stream().anyMatch(rji -> rji.getJobInfo().getJobId().equals(jobInfoCanceled.getJobId())));
		assertFalse(clearedJobs.stream().anyMatch(rji -> rji.getJobInfo().getJobId().equals(jobInfoFailure.getJobId())));
	}

}
