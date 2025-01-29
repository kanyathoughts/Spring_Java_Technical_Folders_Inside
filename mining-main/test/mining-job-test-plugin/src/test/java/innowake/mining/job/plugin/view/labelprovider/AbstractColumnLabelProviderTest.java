/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.FormatStyle;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider;
import org.eclipse.jface.viewers.StyledString;
import org.hamcrest.CoreMatchers;
import org.hamcrest.MatcherAssert;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.Message;
import innowake.mining.shared.model.job.Message.Severity;

/**
 * Abstract base class for label provider tests.
 */
public abstract class AbstractColumnLabelProviderTest {

	/* for example: Mar 25, 2020 10:02:33 AM */
	private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM).withZone(ZoneId.systemDefault());

	@Nullable
	protected Map<String, JobInformation> jobs;

	protected final RemoteJobInfo jobInfoSuccess;
	protected final RemoteJobInfo jobInfoFailure;
	protected final RemoteJobInfo jobInfoCanceled;
	protected final RemoteJobInfo jobInfoRunning;
	protected final RemoteJobInfo jobInfoRunningNoEta;
	protected final RemoteJobInfo jobInfoScheduled;
	protected final RemoteJobInfo jobInfoCanceling;

	/**
	 * Constructor.
	 */
	protected AbstractColumnLabelProviderTest() {
		final Instant fixedTime = Instant.parse("2020-03-25T09:02:33Z");
		final Instant time = Instant.now();
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

		jobInfoCanceled = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId("0605aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccess.getJobInfo().getJobName())
				.setUserName(jobInfoSuccess.getJobInfo().getUserName())
				.setDescription(jobInfoSuccess.getJobInfo().getJobDescription())
				.setScheduledStartTime(fixedTime)
				.setSubmitTime(fixedTime)
				.setStartTime(fixedTime)
				.setFinishTime(fixedTime.plus(1, ChronoUnit.HOURS).plus(22, ChronoUnit.MINUTES).plus(3, ChronoUnit.SECONDS))
				.setStatus(JobStatus.CANCELED)
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
				/* has to use system time here, as the remaining time calculation is based on the system time and therefore
				 * requires a proper time not based in the past. */
				.setStartTime(time)
				.setEta(time.plus(1, ChronoUnit.HOURS).plus(10, ChronoUnit.MINUTES).plus(55, ChronoUnit.SECONDS))
				.setStatus(JobStatus.RUNNING)
				.setTotalWorkUnits(1311)
				.setWorked(377)
				.build());

		jobInfoRunningNoEta = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId("0705aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccess.getJobInfo().getJobName())
				.setUserName(jobInfoSuccess.getJobInfo().getUserName())
				.setDescription(jobInfoSuccess.getJobInfo().getJobDescription())
				.setScheduledStartTime(fixedTime)
				.setSubmitTime(fixedTime)
				/* has to use system time here, as the runtime calculation is based on the system time and therefore
				 * requires a proper time not based in the past. */
				.setStartTime(time.minus(1, ChronoUnit.HOURS).minus(11, ChronoUnit.MINUTES).minus(10, ChronoUnit.SECONDS))
				.setStatus(JobStatus.RUNNING)
				.setTotalWorkUnits(-1)
				.build());

		jobInfoScheduled = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId("0805aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccess.getJobInfo().getJobName())
				.setUserName(jobInfoSuccess.getJobInfo().getUserName())
				.setScheduledStartTime(fixedTime)
				.setSubmitTime(fixedTime)
				.setStatus(JobStatus.SCHEDULED)
				.build());

		jobInfoCanceling = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId("0905aee6-8aa3-4396-b1ef-b77ae722635f")
				.setJobName(jobInfoSuccess.getJobInfo().getJobName())
				.setUserName(jobInfoSuccess.getJobInfo().getUserName())
				.setDescription(jobInfoSuccess.getJobInfo().getJobDescription())
				.setScheduledStartTime(fixedTime)
				.setSubmitTime(fixedTime)
				 /* has to use system time here, as the remaining time calculation is based on the system time and therefore
				  * requires a proper time not based in the past. */
				.setStartTime(time)
				.setEta(time.plus(2, ChronoUnit.HOURS).plus(20, ChronoUnit.MINUTES).plus(55, ChronoUnit.SECONDS))
				.setStatus(JobStatus.CANCEL_REQUESTED)
				.setTotalWorkUnits(1311)
				.setWorked(377)
				.build());
	}

	/**
	 * Asserts that the label returned by the {@code labelProvider} matches exactly the provided {@code expectedLabel}.
	 *
	 * @param labelProvider the {@link IStyledLabelProvider}
	 * @param jobInfo the {@link JobInformation} to pass to the {@code labelProvider}
	 * @param expectedLabel the expected label to be returned by the {@code labelProvider}
	 */
	protected void assertLabel(@Nullable final AbstractColumnLabelProvider labelProvider, final RemoteJobInfo jobInfo, final String expectedLabel) {
		assertNotNull(labelProvider);
		final StyledString styledString = labelProvider.getStyledText(jobInfo);
		assertEquals(expectedLabel, styledString.getString());
	}

	/**
	 * Asserts that the label returned by the {@code labelProvider} starts with the provided {@code expectedLabel}.
	 *
	 * @param labelProvider the {@link IStyledLabelProvider}
	 * @param jobInfo the {@link JobInformation} to pass to the {@code labelProvider}
	 * @param expectedLabel the starting label content returned by the {@code labelProvider}
	 */
	protected void assertLabelStartsWith(@Nullable final IStyledLabelProvider labelProvider, final RemoteJobInfo jobInfo, final String expectedLabel) {
		assertNotNull(labelProvider);
		final StyledString styledString = labelProvider.getStyledText(jobInfo);
		MatcherAssert.assertThat(styledString.getString(), CoreMatchers.startsWith(expectedLabel));
	}

	/**
	 * @param time the {@link Instant}
	 * @return human readable format of the {@link Instant}
	 */
	protected String toReadableDateTime(@Nullable final Instant time) {
		if (time != null) {
			return DATE_TIME_FORMATTER.format(time);
		}
		return "";
	}

	protected RemoteJobInfo createRemoteJobInfo(final JobInformation jobInfo) {
		return new RemoteJobInfo(jobInfo, "http://localhost:8080", "mining-project");
	}
}
