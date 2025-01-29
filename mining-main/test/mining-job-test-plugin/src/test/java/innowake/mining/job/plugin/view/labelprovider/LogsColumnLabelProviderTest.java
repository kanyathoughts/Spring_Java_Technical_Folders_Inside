/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.junit.Test;

/**
 * Tests for the {@link LogsColumnLabelProvider}.
 */
public class LogsColumnLabelProviderTest extends AbstractColumnLabelProviderTest {
	
	private final LogsColumnLabelProvider labelProvider;

	/**
	 * Constructor.
	 */
	public LogsColumnLabelProviderTest() {
		labelProvider = new LogsColumnLabelProvider();
	}
	
	@Test
	public void testLogsLabel() {
		assertLabel(labelProvider, jobInfoRunning, "Logs");
	}
}
