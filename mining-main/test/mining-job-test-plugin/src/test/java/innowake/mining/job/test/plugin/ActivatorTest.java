/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.test.plugin;

import org.junit.Assert;
import org.junit.Test;

import innowake.mining.job.plugin.MiningJobPlugin;

/**
 * Tests that the plugin is properly activated during startup.
 */
public class ActivatorTest {

	@Test
	public void testActivator() {
		final MiningJobPlugin miningPlugin = MiningJobPlugin.getDefault();
		Assert.assertNotNull("Mining job plugin is null", miningPlugin);
	}
	
}
