/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.manager;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.equinox.security.storage.StorageException;
import org.hamcrest.MatcherAssert;
import org.junit.Test;

import com.fasterxml.jackson.core.type.TypeReference;

import innowake.base.eclipse.common.core.util.ResourceUtil;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.service.job.GetJobInfoMock;
import innowake.mining.client.service.job.GetJobInfosMock;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.test.plugin.AbstractJobTest;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.model.JobInfoFieldName;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;
import innowake.mining.shared.model.job.Message.Severity;
import innowake.mining.shared.model.job.ResultStatus;

/**
 * Tests for the {@link RemoteJobManager}.
 *
 * If you encounter a StorageException when executing this test,
 * please try adding
 * -eclipse.password ./src/test/resources/.eclipse_master_password
 * to your run config Program arguments.
 */
public class RemoteJobManagerTest extends AbstractJobTest {

	private static final String JOB_ID_1 = "21e2f364-115c-48ac-8dc6-76f3630597d7";
	private static final String JOB_ID_2 = "d448d56f-f8c9-4a42-a70e-4a58aa072acf";

	private final JobInformation jobInfo1;
	private final JobInformation jobInfo2;

	/**
	 * Constructor.
	 */
	public RemoteJobManagerTest() {
		super("remote-job-manager-project");

		final Instant time = Instant.parse("2007-12-03T10:15:30.00Z");
		this.jobInfo1 = new JobInformation.Builder()
				.setJobId(JOB_ID_1)
				.setJobName("test")
				.setUserName("admin")
				.setDescription("test job 1")
				.setScheduledStartTime(time)
				.setSubmitTime(time)
				.setStartTime(time)
				.setStatus(JobStatus.RUNNING)
				.setEta(time.plus(1, ChronoUnit.HOURS))
				.setTotalWorkUnits(100)
				.setWorked(30)
				.build();

		final List<Message> messages = new ArrayList<>();
		messages.add(new Message(Severity.WARNING, "Some warning occured"));
		this.jobInfo2 = new JobInformation.Builder()
				.setJobId(JOB_ID_2)
				.setJobName("test")
				.setUserName("admin")
				.setDescription("test job 2")
				.setScheduledStartTime(time)
				.setSubmitTime(time)
				.setStartTime(time)
				.setFinishTime(time.plus(1, ChronoUnit.HOURS))
				.setStatus(JobStatus.SUCCESS)
				.setTotalWorkUnits(100)
				.setWorked(100)
				.setMessages(messages)
				.setResultStatus(new ResultStatus(ResultStatus.Severity.WARNING, null, null, true, false))
				.build();
	}

	@Override
	protected void initGetJobInfoMock(final RemoteJobManagerMock remoteJobManager) {
		/* prepare the REST mocks to return the two JobInformation instances. */
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
	 * Tests that the current state of the {@link RemoteJobManager} is properly serialized.
	 *
	 * @throws Exception if there was an error
	 */
	@Test
	public void testPersistJobInformation() throws Exception {
		assertNotNull(remoteJobManager);
		final RemoteJobManager jobManager = remoteJobManager;
		final IProject project = testProject;
		assertNotNull(project);
		jobManager.addJob(project, jobInfo1.getJobId());
		jobManager.addJob(project, jobInfo2.getJobId());
		jobManager.stop();

		project.refreshLocal(1, null);
		final IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(project.getFullPath().append("jobInformation.json"));
		final String content = ResourceUtil.readFileToString(file, "UTF-8", true);

		/* For better comparison purposes the actual content is deserialized and serialized again while using a pretty printer. */
		final Map<String, Set<RemoteJobInfo>> actualValues = PojoMapper.jsonReaderFor(new TypeReference<Map<String, Set<RemoteJobInfo>>>() {}).readValue(content);
		final String actualPrettyPrinted = PojoMapper.jsonWriter().withDefaultPrettyPrinter().writeValueAsString(actualValues);
		assertEquals(normalizeLineEndings(readExpectedFile()), normalizeLineEndings(actualPrettyPrinted));
	}

	/**
	 * Tests that the {@link RemoteJobManager} can properly deserialize any previous state.
	 *
	 * @throws Exception if there was an error
	 */
	@Test
	public void testLoadJobInformation() throws Exception {
		assertNotNull(remoteJobManager);
		final RemoteJobManager jobManager = remoteJobManager;

		/* read the JSON and copy it into the workspace project for deserialization. */
		final String expectedContent = readExpectedFile();
		final IProject project = testProject;
		assertNotNull(project);
		final IFile workspaceFile = ResourcesPlugin.getWorkspace().getRoot().getFile(project.getFullPath().append("jobInformation.json"));
		ResourceUtil.writeStringToFile(workspaceFile, expectedContent, "UTF-8", null, false, true);

		setGetJobInfosMock(jobInfo1, jobInfo2);
		jobManager.init();
		assertJobInformations(jobInfo1, jobInfo2);
	}

	/**
	 * Tests that the internal {@link JobInformation} is properly updated with the values returned by the mocked REST calls.
	 *
	 * @throws Exception if there was an error
	 */
	@Test
	public void testUpdateJobInformation() throws Exception {
		/* populate initial state */
		assertNotNull(remoteJobManager);
		final RemoteJobManagerMock jobManager = (RemoteJobManagerMock) remoteJobManager;
		final IProject project = testProject;
		assertNotNull(project);
		jobManager.addJob(project, jobInfo1.getJobId());
		jobManager.addJob(project, jobInfo2.getJobId());

		/* prepare REST mock with new data to return */
		JobInformation modifiedJobInfo1 = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription(jobInfo1.getJobDescription())
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(jobInfo1.getStartTime())
				.setStatus(jobInfo1.getStatus())
				.setEta(Assert.assertNotNull(jobInfo1.getEta()).plus(1, ChronoUnit.HOURS))
				.setTotalWorkUnits(100)
				.setWorked(80)
				.build();
		setGetJobInfosMock(modifiedJobInfo1, jobInfo2);

		/* update internal state, which should reflect the newest data */
		jobManager.updateJobInformation();
		assertJobInformations(modifiedJobInfo1, jobInfo2);

		/* prepare REST mock with new data to return */
		modifiedJobInfo1 = new JobInformation.Builder()
				.setJobId(jobInfo1.getJobId())
				.setJobName(jobInfo1.getJobName())
				.setUserName(jobInfo1.getUserName())
				.setDescription(jobInfo1.getJobDescription())
				.setScheduledStartTime(jobInfo1.getScheduledStartTime())
				.setSubmitTime(Assert.assertNotNull(jobInfo1.getSubmitTime()))
				.setStartTime(jobInfo1.getStartTime())
				.setStatus(JobStatus.FAILURE)
				.setTotalWorkUnits(100)
				.setWorked(90)
				.setResultStatus(new ResultStatus(ResultStatus.Severity.ERROR, "error", null, false, false))
				.build();
		setGetJobInfosMock(modifiedJobInfo1, jobInfo2);

		/* update internal state, which should reflect the newest data */
		jobManager.updateJobInformation();
		assertJobInformations(modifiedJobInfo1, jobInfo2);

		final GetJobInfosMock getJobInfos = (GetJobInfosMock) jobManager.getJobServiceProvider(project).getJobInfos();
		final Map<JobInfoFieldName, Map<String, Object>> filterObject = getJobInfos.getUsedFilters();
		assertNotNull("Job filter must not be null", filterObject);
		final Map<String, Object> jobIdFilter = filterObject.get(JobInfoFieldName.ID);
		assertNotNull("Job id filter must not be null", jobIdFilter);
		final List<?> jobIds = (List<?>) jobIdFilter.get(FilterOperators.OPERATOR_IN);
		assertNotNull("Job ids must not be null", jobIds);
		assertTrue(jobIds.contains(JOB_ID_1));
		assertTrue(jobIds.contains(JOB_ID_2));
	}

	private String normalizeLineEndings(String lines) {
		return lines.replaceAll("\\r\\n|\\r", "\n");
	}

	private String readExpectedFile() throws IOException {
		final InputStream expectedStream = getClass().getClassLoader().getResourceAsStream("innowake/mining/job/plugin/manager/jobInformation.json.expected");
		return IOUtils.toString(expectedStream, StandardCharsets.UTF_8);
	}

	private void assertJobInformations(final JobInformation expectedJobInfo1, final JobInformation expectedJobInfo2) {
		assertNotNull(remoteJobManager);
		final Map<String, Set<RemoteJobInfo>> allJobs = remoteJobManager.getJobs();
		final IProject project = testProject;
		assertNotNull(project);

		assertEquals(1, allJobs.size());
		final Set<RemoteJobInfo> projectJobs = allJobs.get(project.getName());
		assertEquals(2, projectJobs.size());
		final Set<JobInformation> jobInfos = projectJobs.stream().map(rji -> rji.getJobInfo()).collect(Collectors.toSet());
		MatcherAssert.assertThat(jobInfos, hasItems(new JobInformation[] { expectedJobInfo1, expectedJobInfo2 }));

		final Optional<JobInformation> actualJobInfo1 = jobInfos.stream().filter(jobInfo -> jobInfo.getJobId().equals(expectedJobInfo1.getJobId())).findFirst();
		assertTrue(actualJobInfo1.isPresent());
		assertEquals(expectedJobInfo1, actualJobInfo1.get());

		final Optional<JobInformation> actualJobInfo2 = jobInfos.stream().filter(jobInfo -> jobInfo.getJobId().equals(expectedJobInfo2.getJobId())).findFirst();
		assertTrue(actualJobInfo2.isPresent());
		assertEquals(expectedJobInfo2, actualJobInfo2.get());
	}
}
