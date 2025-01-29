/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.junit.Test;

/**
 * Tests for {@link StateColumnLabelProvider}.
 */
public class StateColumnLabelProviderTest extends AbstractColumnLabelProviderTest {
	
	private final StateColumnLabelProvider labelProvider;

	/**
	 * Constructor.
	 */
	public StateColumnLabelProviderTest() {
		labelProvider = new StateColumnLabelProvider();
	}
	
	/**
	 * For scheduled jobs "Scheduled" will be printed.
	 */
	@Test
	public void testLabelForScheduledJob() {
		assertLabel(labelProvider, jobInfoScheduled, "Scheduled");
	}
	
	/**
	 * For successful jobs "Success" will be printed.
	 */
	@Test
	public void testLabelForSuccessfulJob() {
		assertLabel(labelProvider, jobInfoSuccess, "Success");
	}
	
	/**
	 * For failed jobs "Error" will be printed.
	 */
	@Test
	public void testLabelForFailedJob() {
		assertLabel(labelProvider, jobInfoFailure, "Error");
	}
	
	/**
	 * For canceled jobs "Canceled" will be printed.
	 */
	@Test
	public void testLabelForCanceledJob() {
		assertLabel(labelProvider, jobInfoCanceled, "Canceled");
	}
	
	/**
	 * For running jobs with an ETA "Running" with the actual progress will be printed.
	 */
	@Test
	public void testLabelForRunningJob() {
		assertLabel(labelProvider, jobInfoRunning, "Running");
	}
	
	/**
	 * For canceling jobs "Canceling" will be printed.
	 */
	@Test
	public void testLabelForCancelingJob() {
		assertLabel(labelProvider, jobInfoCanceling, "Canceling");
	}
	
}
