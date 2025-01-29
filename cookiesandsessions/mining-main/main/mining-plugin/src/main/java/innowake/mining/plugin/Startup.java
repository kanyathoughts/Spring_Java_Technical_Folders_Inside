/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import org.eclipse.ui.IStartup;

/**
 * Early startup for {@link MiningPlugin}
 */
public class Startup implements IStartup {
	
	@Override
	public void earlyStartup() {
		/* ensure the plugin is loaded */
		MiningPlugin.getDefault();
	}
}
