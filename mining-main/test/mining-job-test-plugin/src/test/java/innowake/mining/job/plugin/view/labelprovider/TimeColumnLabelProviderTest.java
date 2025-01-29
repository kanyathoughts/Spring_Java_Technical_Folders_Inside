/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.junit.Test;

/**
 * Tests for {@link TimeColumnLabelProvider}.
 */
public class TimeColumnLabelProviderTest extends AbstractColumnLabelProviderTest {
	
	private final TimeColumnLabelProvider labelProvider;

	/**
	 * Constructor.
	 */
	public TimeColumnLabelProviderTest() {
		labelProvider = new TimeColumnLabelProvider();
	}
	
	/**
	 * A scheduled job should not display any time information.
	 */
	@Test
	public void testLabelForScheduledJob() {
		assertLabel(labelProvider, jobInfoScheduled, "");
	}
	
	/**
	 * A successful job shows the time required to finish the job.
	 */
	@Test
	public void testLabelForSuccessfulJob() {
		assertLabel(labelProvider, jobInfoSuccess, "01:05:03");
	}
	
	/**
	 * A failed job shows the time required to finish the job.
	 */
	@Test
	public void testLabelForFailedJob() {
		assertLabel(labelProvider, jobInfoFailure, "01:10:03");
	}
	
	/**
	 * A canceled job shows the time required to finish the job.
	 */
	@Test
	public void testLabelForCanceledJob() {
		assertLabel(labelProvider, jobInfoCanceled, "01:22:03");
	}
	
	/**
	 * A running job shows the remaining time. As the calculation is based on the difference
	 * of the current system time and the ETA, only the hour is checked to avoid sporadic failures.
	 */
	@Test
	public void testLabelForRunningJob() {
		labelProvider.setOffline(false);
		assertLabelStartsWith(labelProvider, jobInfoRunning, "Running since");
		labelProvider.setOffline(true);
		assertLabelStartsWith(labelProvider, jobInfoRunning, "Running since");
	}
	
	/**
	 * A running job with no ETA shows the time since it's running. As the calculation is based on the difference
	 * of the job start time and the current system time, only the hour is checked to avoid sporadic failures.
	 */
	@Test
	public void testLabelForRunningNoEtaJob() {
		assertLabelStartsWith(labelProvider, jobInfoRunningNoEta, "Running since 01:");
	}
	
	/**
	 * A canceling job shows the remaining time. As the calculation is based on the difference
	 * of the current system time and the ETA, only the hour is checked to avoid sporadic failures.
	 */
	@Test
	public void testLabelForCancelingJob() {
		labelProvider.setOffline(false);
		assertLabelStartsWith(labelProvider, jobInfoCanceling, "Running since");
		labelProvider.setOffline(true);
		assertLabelStartsWith(labelProvider, jobInfoCanceling, "Running since");
	}
	
}
