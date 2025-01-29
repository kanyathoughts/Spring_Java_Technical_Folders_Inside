/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import org.junit.Assert;
import org.junit.Test;

public class ActivatorTest {

	@Test
	public void testActivator() {
		final MiningPlugin miningPlugin = MiningPlugin.getDefault();
		Assert.assertNotNull("Mining plugin is null", miningPlugin);
	}
	
}
