/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin;

import org.eclipse.ui.IStartup;

/**
 * Early startup for {@link MiningJobPlugin}
 */
public class Startup implements IStartup {
	
	@Override
	public void earlyStartup() {
		/* ensure the plugin is loaded */
		MiningJobPlugin.getDefault();
	}
}
