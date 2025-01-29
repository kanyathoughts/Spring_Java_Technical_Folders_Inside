/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.junit.Test;

/**
 * Tests for the {@link DateColumnLabelProvider}.
 */
public class DateColumnLabelProviderTest extends AbstractColumnLabelProviderTest {
	
	private final DateColumnLabelProvider labelProvider;
	
	/**
	 * Constructor.
	 */
	public DateColumnLabelProviderTest() {
		labelProvider = new DateColumnLabelProvider();
	}

	/**
	 * Expects 'Scheduled: Mar 25, 2020 10:02:33 AM'
	 */
	@Test
	public void testLabelForScheduledJob() {
		assertLabel(labelProvider, jobInfoScheduled, "Scheduled: " + toReadableDateTime(jobInfoScheduled.getJobInfo().getScheduledStartTime()));
	}
	
	/**
	 * Expects 'Finished: Mar 25, 2020 3:40:41 PM'
	 */
	@Test
	public void testLabelForSuccessfulJob() {
		assertLabel(labelProvider, jobInfoSuccess, "Finished: " + toReadableDateTime(jobInfoSuccess.getJobInfo().getFinishTime()));
	}
	
	/**
	 * Expects 'Finished: Mar 25, 2020 3:45:41 PM'
	 */
	@Test
	public void testLabelForFailedJob() {
		assertLabel(labelProvider, jobInfoFailure, "Finished: " + toReadableDateTime(jobInfoFailure.getJobInfo().getFinishTime()));
	}
	
	/**
	 * Expects 'Finished: Mar 25, 2020 3:57:41 PM'
	 */
	@Test
	public void testLabelForCanceledJob() {
		assertLabel(labelProvider, jobInfoCanceled, "Finished: " + toReadableDateTime(jobInfoCanceled.getJobInfo().getFinishTime()));
	}
	
	/**
	 * Expects 'Started: <current system date time>'
	 */
	@Test
	public void testLabelForRunningJob() {
		assertLabel(labelProvider, jobInfoRunning, "Started: " + toReadableDateTime(jobInfoRunning.getJobInfo().getStartTime()));
	}
	
	/**
	 * Expects 'Started: <current system date time>'
	 */
	@Test
	public void testLabelForCancelingJob() {
		assertLabel(labelProvider, jobInfoCanceling, "Started: " + toReadableDateTime(jobInfoRunning.getJobInfo().getStartTime()));
	}
	
}
