/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;

import org.hamcrest.CoreMatchers;
import org.junit.Test;

import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.job.plugin.view.JobViewContentProvider.ParentAwareMessage;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;
import innowake.mining.shared.model.job.Message.Severity;

/**
 * Tests for the {@link JobViewContentProvider}.
 */
public class JobViewContentProviderTest {

	private final RemoteJobInfo jobInfoScheduled1;
	private final RemoteJobInfo jobInfoScheduled2;
	private final RemoteJobInfo jobInfoRunning;
	private final RemoteJobInfo jobInfoCanceling;
	private final RemoteJobInfo jobInfoSuccess;
	private final RemoteJobInfo jobInfoFailure;

	/**
	 * Constructor.
	 */
	public JobViewContentProviderTest() {
		final Instant fixedTime = Instant.parse("2020-03-25T09:02:33Z");
		jobInfoSuccess = createRemoteJobInfo(new JobInformation.Builder()
			.setJobId("adad5155-f5c8-461f-969f-5716e3077066")
			.setJobName("test")
			.setUserName("admin")
			.setDescription("Identifying candidates for the project with Id '2'")
			.setScheduledStartTime(fixedTime)
			.setSubmitTime(fixedTime)
			.setStartTime(fixedTime)
			.setFinishTime(fixedTime.plus(1, ChronoUnit.HOURS).plus(5, ChronoUnit.MINUTES).plus(3, ChronoUnit.SECONDS))
			.setStatus(JobStatus.SUCCESS)
			.setTotalWorkUnits(100)
			.setWorked(100)
			.build());

		final List<Message> failureMessages = new ArrayList<>();
		failureMessages.add(new Message(Severity.INFO, "Some info message"));
		failureMessages.add(new Message(Severity.WARNING, "Some warning message"));
		failureMessages.add(new Message(Severity.ERROR, "Some error message"));
		jobInfoFailure = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId("a6638a47-47c4-40e9-b7ad-b1b8b57d0466")
				.setJobName(jobInfoSuccess.getJobInfo().getJobName())
				.setUserName(jobInfoSuccess.getJobInfo().getUserName())
				.setDescription(jobInfoSuccess.getJobInfo().getJobDescription())
				.setScheduledStartTime(fixedTime)
				.setSubmitTime(fixedTime)
				.setStartTime(fixedTime)
				.setFinishTime(fixedTime.plus(1, ChronoUnit.HOURS).plus(10, ChronoUnit.MINUTES).plus(3, ChronoUnit.SECONDS))
				.setStatus(JobStatus.FAILURE)
				.setMessages(failureMessages)
				.setTotalWorkUnits(100)
				.setWorked(50)
				.build());

		jobInfoRunning = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId("0705aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccess.getJobInfo().getJobName())
				.setUserName(jobInfoSuccess.getJobInfo().getUserName())
				.setDescription(jobInfoSuccess.getJobInfo().getJobDescription())
				.setScheduledStartTime(fixedTime)
				.setSubmitTime(fixedTime)
				.setStartTime(fixedTime)
				.setEta(fixedTime.plus(1, ChronoUnit.HOURS).plus(10, ChronoUnit.MINUTES).plus(55, ChronoUnit.SECONDS))
				.setStatus(JobStatus.RUNNING)
				.setTotalWorkUnits(1311)
				.setWorked(377)
				.build());

		jobInfoCanceling = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId("0905aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccess.getJobInfo().getJobName())
				.setUserName(jobInfoSuccess.getJobInfo().getUserName())
				.setDescription(jobInfoSuccess.getJobInfo().getJobDescription())
				.setScheduledStartTime(fixedTime)
				.setSubmitTime(fixedTime)
				.setStartTime(fixedTime.plus(1, ChronoUnit.MINUTES))
				.setEta(fixedTime.plus(2, ChronoUnit.HOURS).plus(20, ChronoUnit.MINUTES).plus(55, ChronoUnit.SECONDS))
				.setStatus(JobStatus.CANCEL_REQUESTED)
				.setTotalWorkUnits(1311)
				.setWorked(377)
				.build());

		jobInfoScheduled1 = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId("0805aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccess.getJobInfo().getJobName())
				.setUserName(jobInfoSuccess.getJobInfo().getUserName())
				.setScheduledStartTime(fixedTime)
				.setSubmitTime(fixedTime)
				.setStatus(JobStatus.SCHEDULED)
				.build());

		jobInfoScheduled2 = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId("0885aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccess.getJobInfo().getJobName())
				.setUserName(jobInfoSuccess.getJobInfo().getUserName())
				.setScheduledStartTime(fixedTime.plus(1, ChronoUnit.HOURS))
				.setSubmitTime(fixedTime)
				.setStatus(JobStatus.SCHEDULED)
				.build());
	}

	/**
	 * Tests that all {@link JobInformation} elements are ordered the following way:
	 * scheduled > running (including canceling) > finished (no matter if successful or not).
	 * Elements with the same state are ordered according to {@link Instant#compareTo(Instant)}.
	 */
	@Test
	public void testElementOrdering() {
		final JobViewContentProvider provider = new JobViewContentProvider();
		final RemoteJobInfo[] jobInfos = new RemoteJobInfo[]
				{ jobInfoFailure, jobInfoCanceling, jobInfoSuccess, jobInfoScheduled2, jobInfoScheduled1, jobInfoRunning };
		final Object[] elements = provider.getElements(jobInfos);
		assertNotNull(elements);
		assertEquals(6, elements.length);
		assertThat(elements, CoreMatchers.instanceOf(RemoteJobInfo[].class));

		final RemoteJobInfo[] sortedJobInfos = (RemoteJobInfo[]) elements;
		assertEquals(jobInfoScheduled1.getJobInfo(), sortedJobInfos[0].getJobInfo());
		assertEquals(jobInfoScheduled2.getJobInfo(), sortedJobInfos[1].getJobInfo());
		assertEquals(jobInfoRunning.getJobInfo(), sortedJobInfos[2].getJobInfo());
		assertEquals(jobInfoCanceling.getJobInfo(), sortedJobInfos[3].getJobInfo());
		assertEquals(jobInfoSuccess.getJobInfo(), sortedJobInfos[4].getJobInfo());
		assertEquals(jobInfoFailure.getJobInfo(), sortedJobInfos[5].getJobInfo());
	}

	/**
	 * Tests that only the failure job contains children/messages of all three severity kinds.
	 */
	@Test
	public void testChildren() {
		final JobViewContentProvider provider = new JobViewContentProvider();
		assertFalse(provider.hasChildren(jobInfoScheduled1));
		assertFalse(provider.hasChildren(jobInfoScheduled2));
		assertFalse(provider.hasChildren(jobInfoRunning));
		assertFalse(provider.hasChildren(jobInfoCanceling));
		assertFalse(provider.hasChildren(jobInfoSuccess));

		assertTrue(provider.hasChildren(jobInfoFailure));
		final Object[] children = provider.getChildren(jobInfoFailure);
		assertNotNull(children);
		assertEquals(3, children.length);

		assertThat(children[0], CoreMatchers.instanceOf(ParentAwareMessage.class));
		ParentAwareMessage message = (ParentAwareMessage) children[0];
		assertEquals(Severity.INFO, message.getSeverity());
		assertEquals("Some info message", message.getText());
		assertEquals(jobInfoFailure, message.getParent());

		assertThat(children[1], CoreMatchers.instanceOf(ParentAwareMessage.class));
		message = (ParentAwareMessage) children[1];
		assertEquals(Severity.WARNING, message.getSeverity());
		assertEquals("Some warning message", message.getText());
		assertEquals(jobInfoFailure, message.getParent());

		assertThat(children[2], CoreMatchers.instanceOf(ParentAwareMessage.class));
		message = (ParentAwareMessage) children[2];
		assertEquals(Severity.ERROR, message.getSeverity());
		assertEquals("Some error message", message.getText());
		assertEquals(jobInfoFailure, message.getParent());
	}

	protected RemoteJobInfo createRemoteJobInfo(final JobInformation jobInfo) {
		return new RemoteJobInfo(jobInfo, "http://localhost:8080", "mining-project");
	}

}
