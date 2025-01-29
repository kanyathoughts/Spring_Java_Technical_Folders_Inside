/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.junit.Test;
import innowake.mining.job.plugin.manager.RemoteJobManager.RemoteJobInfo;
import innowake.mining.shared.model.job.JobInformation;
import innowake.mining.shared.model.job.JobStatus;
import innowake.mining.shared.model.job.ResultStatus;
import innowake.mining.shared.model.job.ResultStatus.Severity;

/**
 * Tests for the {@link ResultColumnLabelProvider}.
 */
public class ResultColumnLabelProviderTest extends AbstractColumnLabelProviderTest {

	private static final String ADMIN = "admin";
	private static final String JOB_NAME = "test";
	private static final String JOB_ID = "adad5155-f5c8-461f-969f-5716e3077067";
	private final ResultColumnLabelProvider labelProvider;

	/**
	 * Constructor.
	 */
	public ResultColumnLabelProviderTest() {
		labelProvider = new ResultColumnLabelProvider();
	}

	/**
	 * Expects 'Get result' label string as there is a collectable result.
	 */
	@Test
	public void testLabelForCollectableResult() {
		final RemoteJobInfo jobInfo1 = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId(JOB_ID)
				.setJobName(JOB_NAME)
				.setUserName(ADMIN)
				.setStatus(JobStatus.SUCCESS)
				.setResultStatus(new ResultStatus(Severity.OK, null, null, true, false))
				.build());

		assertLabel(labelProvider, jobInfo1, ResultColumnLabelProvider.GET_RESULT);
	}

	/**
	 * Expects empty string as there is only an internal collectable result.
	 */
	@Test
	public void testLabelForInternalCollectableResult() {
		final RemoteJobInfo jobInfo2 = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId(JOB_ID)
				.setJobName(JOB_NAME)
				.setUserName(ADMIN)
				.setStatus(JobStatus.SUCCESS)
				.setResultStatus(new ResultStatus(Severity.OK, null, null, true, true))
				.build());

		assertLabel(labelProvider, jobInfo2, "");
	}

	/**
	 * Expects empty string as there is no collectable result.
	 */
	@Test
	public void testLabelForSuccessfulJobWithoutResult() {
		final RemoteJobInfo jobInfo1 = createRemoteJobInfo(new JobInformation.Builder()
				.setJobId(JOB_ID)
				.setJobName(JOB_NAME)
				.setUserName(ADMIN)
				.setStatus(JobStatus.SUCCESS)
				.setResultStatus(new ResultStatus(Severity.OK, null, null, false, false))
				.build());

		assertLabel(labelProvider, jobInfo1, "");
	}

}
