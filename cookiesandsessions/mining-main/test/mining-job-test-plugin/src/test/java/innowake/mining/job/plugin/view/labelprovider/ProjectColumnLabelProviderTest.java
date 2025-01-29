/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.labelprovider;

import org.junit.Test;

/**
 * Tests for the {@link ProjectColumnLabelProvider}.
 */
public class ProjectColumnLabelProviderTest extends AbstractColumnLabelProviderTest {
	
	private final ProjectColumnLabelProvider labelProvider;

	/**
	 * Constructor.
	 */
	public ProjectColumnLabelProviderTest() {
		labelProvider = new ProjectColumnLabelProvider();
	}
	
	@Test
	public void testLabel() {
		assertLabel(labelProvider, jobInfoRunning, "mining-project (http://localhost:8080)");
	}
	
}
