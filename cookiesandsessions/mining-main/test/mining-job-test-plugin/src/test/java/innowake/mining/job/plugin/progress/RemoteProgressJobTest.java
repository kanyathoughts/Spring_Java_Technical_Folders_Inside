/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.progress;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.StorageException;
import org.hamcrest.CoreMatchers;
import org.junit.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.service.job.CancelJobMock;
import innowake.mining.client.service.job.GetJobInfoMock;
import innowake.mining.client.service.job.GetJobInfosMock;
import innowake.mining.job.plugin.manager.RemoteJobManagerMock;
import innowake.mining.job.plugin.manager.UpdateActiveJob;
import innowake.mining.job.test.plugin.AbstractJobTest;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;

/**
 * Tests for the {@link RemoteProgressJob}.
 *
 * If you encounter a StorageException when executing this test,
 * please try adding
 * -eclipse.password ./src/test/resources/.eclipse_master_password
 * to your run config Program arguments.
 */
public class RemoteProgressJobTest extends AbstractJobTest {

	private static final long TWO_MINUTES = 2;

	private final JobInformation jobInfo1;
	private final JobInformation jobInfo2;

	/**
	 * Constructor.
	 */
	public RemoteProgressJobTest() {
		super("remote-job-test-project");

		final Instant time = Instant.now();
		this.jobInfo1 = new JobInformation.Builder()
			.setJobId("21e2f364-115c-48ac-8dc6-76f3630597d7")
			.setJobName("test")
			.setUserName("admin")
			.setScheduledStartTime(time)
			.setSubmitTime(time)
			.setStatus(JobStatus.SCHEDULED)
			.build();

		this.jobInfo2 = new JobInformation.Builder()
				.setJobId("22e2f364-115c-48ac-8dc6-76f3630597d7")
				.setJobName("test")
				.setUserName("admin")
				.setScheduledStartTime(time.plus(1, ChronoUnit.HOURS))
				.setSubmitTime(time)
				.setStatus(JobStatus.SCHEDULED)
				.build();
	}

	@Override
	protected void initGetJobInfoMock(final RemoteJobManagerMock remoteJobManager) {
		try {
			assertNotNull(testProject);
			final GetJobInfoMock getJobInfo = (GetJobInfoMock) remoteJobManager.getJobServiceProvider(testProject).getJobInfo();
			getJobInfo.addJobInfoToReturn(jobInfo1.getJobId(), jobInfo1);
			getJobInfo.addJobInfoToReturn(jobInfo2.getJobId(), jobInfo2);
		} catch (final CoreException | StorageException e) {
			throw new IllegalStateException(e);
		}
	}

	/**
	 * Tests that the name of the job contains the information that the remote job is scheduled to be executed at a future time.
	 *
	 * @throws Exception if anything goes wrong
	 */
	@Test
	public void testScheduledInFuture() throws Exception {
		/* Add new job to the manager. addJob() internally requests the mock data provided at initGetJobInfoMock */
		assertNotNull(remoteJobManager);
		final RemoteJobManagerMock jobManager = (RemoteJobManagerMock) remoteJobManager;
		final IProject project = testProject;
		assertNotNull(project);
		jobManager.addJob(project, jobInfo2.getJobId());

		/* Wait until the RemoteProgressJob is running */
		final TestRemoteProgressJob remoteProgressJob = waitForRemoteProgressJob(jobManager, jobInfo2.getJobId());
		assertNotNull(remoteProgressJob);
		waitForJobNameContains(remoteProgressJob, "Starts");

		assertJobName(remoteProgressJob, "Remote job", "(Starts at");
		assertProgressMonitor(remoteProgressJob, null);
	}

	/**
	 * Tests that the name of the job contains the information that the job has been remotely requested to cancel.
	 *
	 * @throws Exception if anything goes wrong
	 */
	@Test
	public void testJobCanceledRemotely() throws Exception {
		/* Add new job to the manager. addJob() internally requests the mock data provided at initGetJobInfoMock */
		assertNotNull(remoteJobManager);
		final RemoteJobManagerMock jobManager = (RemoteJobManagerMock) remoteJobManager;
		final IProject project = testProject;
		assertNotNull(project);
		jobManager.addJob(project, jobInfo1.getJobId());

		/* Wait until the RemoteProgressJob is running */
		final TestRemoteProgressJob remoteProgressJob = waitForRemoteProgressJob(jobManager, jobInfo1.getJobId());
		assertNotNull(remoteProgressJob);

		/* Prepare REST mock with a job requested to cancel */
		JobInformation modifiedJobInfo = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription("My job description")
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(Instant.now())
				.setStatus(JobStatus.CANCEL_REQUESTED)
				.setTotalWorkUnits(100)
				.setWorked(20)
				.build();
		setGetJobInfosMock(modifiedJobInfo);

		/* Wait until the UpdateActiveJob updates the internal state with the mock data */
		waitForJobNameContains(remoteProgressJob, "Canceled");

		assertJobName(remoteProgressJob, "My job description", "(Canceled remotely)");
	}

	/**
	 * Tests that when explicitly canceling the eclipse job by a user action, that it sends the cancel request to
	 * the remote job and then terminates itself after receiving the information that the remote job has actually
	 * cancelled.
	 *
	 * @throws Exception if anything goes wrong
	 */
	@Test
	public void testJobCanceled() throws Exception {
		/* Add new job to the manager. addJob() internally requests the mock data provided at initGetJobInfoMock */
		assertNotNull(remoteJobManager);
		final RemoteJobManagerMock jobManager = (RemoteJobManagerMock) remoteJobManager;
		final IProject project = testProject;
		assertNotNull(project);
		jobManager.addJob(project, jobInfo1.getJobId());

		/* Wait until the RemoteProgressJob is running */
		final TestRemoteProgressJob remoteProgressJob = waitForRemoteProgressJob(jobManager, jobInfo1.getJobId());
		assertNotNull(remoteProgressJob);
		waitForJobNameContains(remoteProgressJob, "Scheduled");

		/* Simulates the user clicking the cancel button */
		remoteProgressJob.cancel();

		/* Wait until the job triggers the REST call to cancel the job execution */
		final Instant start = Instant.now();
		final CancelJobMock cancelJobRequest = (CancelJobMock) jobManager.getJobServiceProvider(project).cancelJob();
		while (Duration.between(start, Instant.now()).toMinutes() < TWO_MINUTES) {
			if (cancelJobRequest.getExecutionAmount() != 0) {
				break;
			}
		}
		assertEquals(1, cancelJobRequest.getExecutionAmount());

		/* Prepare REST mock to reflect that the job is now canceled */
		final JobInformation modifiedJobInfo = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription("My job description")
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(Instant.now())
				.setStatus(JobStatus.CANCELED)
				.setTotalWorkUnits(100)
				.setWorked(20)
				.build();
		setGetJobInfosMock(modifiedJobInfo);

		/* Eclipse job should stop itself now */
		waitUntilJobIsGone(remoteProgressJob);
		assertEquals(Job.NONE, remoteProgressJob.getState());
		assertEquals(0, jobManager.getRemoteProgressJobs().size());
	}

	/**
	 * Tests that the eclipse job will be marked as offline if any of the REST call fail. After the calls
	 * are possible again, the offline information will be removed and the job updated accordingly to the newest state.
	 *
	 * @throws Exception if anything goes wrong
	 */
	@Test
	public void testServerOffline() throws Exception {
		/* Add new job to the manager. addJob() internally requests the mock data provided at initGetJobInfoMock */
		assertNotNull(remoteJobManager);
		final RemoteJobManagerMock jobManager = (RemoteJobManagerMock) remoteJobManager;
		final IProject project = testProject;
		assertNotNull(project);
		jobManager.addJob(project, jobInfo1.getJobId());

		/* Wait until the RemoteProgressJob is running */
		final TestRemoteProgressJob remoteProgressJob = waitForRemoteProgressJob(jobManager, jobInfo1.getJobId());
		assertNotNull(remoteProgressJob);

		/* Simulates that no connection can be established to the REST server */
		final GetJobInfosMock getJobInfosRequest = (GetJobInfosMock) jobManager.getJobServiceProvider(project).getJobInfos();
		getJobInfosRequest.simulateOffline(true);

		/* Prepare REST mock to reflect that the job has updated progress */
		final JobInformation modifiedJobInfo = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription("Job testing offline mode")
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(Instant.now())
				.setStatus(JobStatus.RUNNING)
				.setTotalWorkUnits(100)
				.setWorked(50)
				.setEta(Instant.now().plus(1, ChronoUnit.HOURS))
				.build();
		setGetJobInfosMock(modifiedJobInfo);

		/* Job must be marked as offline and should not have received any updates */
		waitForJobNameContains(remoteProgressJob, "Offline");
		assertJobName(remoteProgressJob, "Remote job", "(Offline)");
		assertProgressMonitor(remoteProgressJob, null);

		/* Switch back to online mode. After one minute the background job should trigger, transitioning to online mode again. */
		getJobInfosRequest.simulateOffline(false);
		waitForJobNameContains(remoteProgressJob, "(Remaining");
		waitForProgressMonitorChange(remoteProgressJob, null, null, Integer.valueOf(50));
		assertJobName(remoteProgressJob, "Job testing offline mode", "(Running");
		assertProgressMonitor(remoteProgressJob, null);
	}

	/**
	 * Tests that the {@link RemoteProgressJob} is properly updated according to the {@link JobInformation}
	 * instances provided by the REST mocks. The background job {@link UpdateActiveJob} should pick up the mocked
	 * information, which is then available to the {@link RemoteProgressJob}.
	 *
	 * @throws Exception if anything goes wrong
	 */
	@Test
	public void testFullFlow() throws Exception {
		/* Add new job to the manager. addJob() internally requests the mock data provided at initGetJobInfoMock */
		assertNotNull(remoteJobManager);
		final RemoteJobManagerMock jobManager = (RemoteJobManagerMock) remoteJobManager;
		final IProject project = testProject;
		assertNotNull(project);
		jobManager.addJob(project, jobInfo1.getJobId());

		/* Wait until the RemoteProgressJob is running */
		final TestRemoteProgressJob remoteProgressJob = waitForRemoteProgressJob(jobManager, jobInfo1.getJobId());
		assertNotNull(remoteProgressJob);
		waitForJobNameContains(remoteProgressJob, "Scheduled");

		assertJobName(remoteProgressJob, "Remote job", "(Scheduled)");
		assertProgressMonitor(remoteProgressJob, null);

		/* Prepare REST mock to reflect a running job with indeterministic progress */
		JobInformation modifiedJobInfo = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription("My job description")
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(Instant.now())
				.setStatus(JobStatus.RUNNING)
				.setTotalWorkUnits(-1)
				.build();
		setGetJobInfosMock(modifiedJobInfo);

		/* Wait until the UpdateActiveJob updates the internal state with the mock data */
		waitForJobNameContains(remoteProgressJob, "Running");

		assertJobName(remoteProgressJob, "Remote job '" + jobInfo1.getJobId() + "': My job description", "(Running since");
		assertProgressMonitor(remoteProgressJob, null);

		/* Prepare REST mock with the job now switched in deterministic mode and additional step description */
		modifiedJobInfo = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription(modifiedJobInfo.getJobDescription())
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(modifiedJobInfo.getStartTime())
				.setStatus(JobStatus.RUNNING)
				.setTotalWorkUnits(100)
				.setStepDescription("My step description")
				.build();
		setGetJobInfosMock(modifiedJobInfo);

		/* Wait until the UpdateActiveJob updates the internal state with the mock data */
		waitForProgressMonitorChange(remoteProgressJob, null, Integer.valueOf(100), null);

		assertJobName(remoteProgressJob, "Remote job '" + jobInfo1.getJobId() + "': My job description", "(Running since");
		assertProgressMonitor(remoteProgressJob, "My step description");

		/* Prepare REST mock with a job that has progress and an ETA */
		modifiedJobInfo = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription(modifiedJobInfo.getJobDescription())
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(modifiedJobInfo.getStartTime())
				.setStatus(JobStatus.RUNNING)
				.setTotalWorkUnits(100)
				.setWorked(50)
				.setEta(Instant.now().plus(1, ChronoUnit.HOURS))
				.build();
		setGetJobInfosMock(modifiedJobInfo);

		/* Wait until the UpdateActiveJob updates the internal state with the mock data */
		waitForJobNameContains(remoteProgressJob, "Remaining");
		waitForProgressMonitorChange(remoteProgressJob, null, null, Integer.valueOf(50));

		assertJobName(remoteProgressJob, "Remote job '" + jobInfo1.getJobId() + "': My job description", "(Running");
		assertProgressMonitor(remoteProgressJob, null);

		/* Prepare REST mock with a finished job */
		modifiedJobInfo = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription(modifiedJobInfo.getJobDescription())
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(modifiedJobInfo.getStartTime())
				.setStatus(JobStatus.SUCCESS)
				.setTotalWorkUnits(100)
				.setWorked(100)
				.build();
		setGetJobInfosMock(modifiedJobInfo);

		/* Eclipse job should stop itself now */
		waitUntilJobIsGone(remoteProgressJob);
		assertEquals(Job.NONE, remoteProgressJob.getState());
		assertEquals(0, jobManager.getRemoteProgressJobs().size());
	}

	/**
	 * Tests that jobs are properly cleaned up when they're no longer known to the api-server.
	 *
	 * @throws Exception if anything goes wrong
	 */
	@Test
	public void testCleanupOfUnknownJobs() throws Exception {
		/* Add new job to the manager. addJob() internally requests the mock data provided at initGetJobInfoMock */
		assertNotNull(remoteJobManager);
		final RemoteJobManagerMock jobManager = (RemoteJobManagerMock) remoteJobManager;
		final IProject project = testProject;
		assertNotNull(project);
		jobManager.addJob(project, jobInfo1.getJobId());
		jobManager.addJob(project, jobInfo2.getJobId());

		/* Wait until the two RemoteProgressJobs are running */
		final TestRemoteProgressJob remoteProgressJob1 = waitForRemoteProgressJob(jobManager, jobInfo1.getJobId());
		assertNotNull(remoteProgressJob1);
		waitForJobNameContains(remoteProgressJob1, "Scheduled");

		final TestRemoteProgressJob remoteProgressJob2 = waitForRemoteProgressJob(jobManager, jobInfo2.getJobId());
		assertNotNull(remoteProgressJob2);
		waitForJobNameContains(remoteProgressJob2, "Starts at");

		assertEquals(2, jobManager.getRemoteProgressJobs().size());
		assertEquals(2, jobManager.getJobs().values().stream().flatMap(rji -> rji.stream()).collect(Collectors.toSet()).size());

		/* Prepare REST mock to reflect that both jobs are now in a running state */
		JobInformation modifiedJobInfo1 = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription("My job description")
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(Instant.now())
				.setStatus(JobStatus.RUNNING)
				.setTotalWorkUnits(-1)
				.build();
		JobInformation modifiedJobInfo2 = new JobInformation.Builder()
				.setJobId(jobInfo2.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo2.getUserName())
				.setDescription("My job description")
				.setScheduledStartTime(jobInfo2.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo2.getSubmitTime()))
				.setStartTime(Instant.now())
				.setStatus(JobStatus.RUNNING)
				.setTotalWorkUnits(-1)
				.build();
		/* This will effectively make both jobs known to the api-server GetJobInfos mock. */
		setGetJobInfosMock(modifiedJobInfo1, modifiedJobInfo2);

		/* Wait until the UpdateActiveJob updates the internal state with the mock data */
		waitForJobNameContains(remoteProgressJob1, "Running");
		waitForJobNameContains(remoteProgressJob2, "Running");

		/* Prepare REST mock to reflect that the first job has been requested to cancel, so that we can check the UpdateActiveJob. */
		modifiedJobInfo1 = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription(modifiedJobInfo1.getJobDescription())
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(modifiedJobInfo1.getStartTime())
				.setStatus(JobStatus.CANCEL_REQUESTED)
				.setTotalWorkUnits(-1)
				.build();
		/* This will effectively make the second job unknown to the api-server GetJobInfos mock (i.e. caused due to URL change). */
		setGetJobInfosMock(modifiedJobInfo1);

		/* The default background refresh via UpdateActiveJob will not remove the second job. */
		waitForJobNameContains(remoteProgressJob1, "Cancel");
		assertEquals(2, jobManager.getRemoteProgressJobs().size());
		assertEquals(2, jobManager.getJobs().values().stream().flatMap(rji -> rji.stream()).collect(Collectors.toSet()).size());

		/* This will be called by refresh and cleanup operations and filters all jobs that are unknown to the api-server.
		 * The second job must be gone afterwards. */
		jobManager.updateJobInformation();
		waitUntilJobIsGone(remoteProgressJob2);
		assertEquals(1, jobManager.getRemoteProgressJobs().size());
		assertEquals(1, jobManager.getJobs().values().stream().flatMap(rji -> rji.stream()).collect(Collectors.toSet()).size());
	}

	private void assertJobName(final TestRemoteProgressJob remoteProgressJob, final String expectedNameBegin, final String expectedNameEnd) {
		final String jobName = remoteProgressJob.getName();
		assertThat(jobName, CoreMatchers.containsString(expectedNameBegin));
		assertThat(jobName, CoreMatchers.containsString(expectedNameEnd));
	}

	private void assertProgressMonitor(final TestRemoteProgressJob remoteProgressJob, @Nullable final String expectedSubTaskName) {
		final ProgressMonitorDecorator progressMonitor = remoteProgressJob.getProgressMonitorDecorator();
		assertNotNull(progressMonitor);
		assertEquals(expectedSubTaskName, progressMonitor.getLastSubTaskName());
		assertEquals(IProgressMonitor.UNKNOWN, progressMonitor.getLastTotalWork());
		assertEquals(0, progressMonitor.getLastWorked());
	}

	@Nullable
	private TestRemoteProgressJob waitForRemoteProgressJob(final RemoteJobManagerMock remoteJobManager, final String jobId) {
		Instant start = Instant.now();
		while (Duration.between(start, Instant.now()).toMinutes() < TWO_MINUTES) {
			final Map<String, TestRemoteProgressJob> remoteProgressJobs = remoteJobManager.getRemoteProgressJobs();
			if ( ! remoteProgressJobs.isEmpty()) {
				final TestRemoteProgressJob job = remoteProgressJobs.get(jobId);
				if (job != null && job.getState() == Job.RUNNING) {
					return job;
				}
			}
		}
		return null;
	}

	private void waitForJobNameContains(final TestRemoteProgressJob remoteProgressJob, final String textToContain) {
		final Instant start = Instant.now();
		while (Duration.between(start, Instant.now()).toMinutes() < TWO_MINUTES) {
			if (remoteProgressJob.getName().contains(textToContain)) {
				break;
			}
		}
	}

	private void waitForProgressMonitorChange(final TestRemoteProgressJob remoteProgressJob, @Nullable final String changedSubTaskName,
			@Nullable final Integer changedTotalWork, @Nullable final Integer changedWorked) {
		final Instant start = Instant.now();
		final ProgressMonitorDecorator monitor = remoteProgressJob.getProgressMonitorDecorator();
		assertNotNull(monitor);
		while (Duration.between(start, Instant.now()).toMinutes() < TWO_MINUTES) {
			final String lastSubTaskName = monitor.getLastSubTaskName();
			if ((changedSubTaskName != null && lastSubTaskName != null && lastSubTaskName.contains(changedSubTaskName))
					|| (changedTotalWork != null && monitor.getLastTotalWork() == changedTotalWork.intValue())
					|| (changedWorked != null && monitor.getLastWorked() == changedWorked.intValue())) {
				break;
			}
		}
	}

	private void waitUntilJobIsGone(final TestRemoteProgressJob remoteProgressJob) {
		final Instant start = Instant.now();
		while (Duration.between(start, Instant.now()).toMinutes() < TWO_MINUTES) {
			if (remoteProgressJob.getState() == Job.NONE) {
				break;
			}
		}
	}
}
