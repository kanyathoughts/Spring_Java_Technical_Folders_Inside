/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences.ui;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;

import innowake.mining.plugin.preferences.MiningPreferences;

/**
 * Called if the workbench defaults should be set.
 * For example by the button "Restore Defaults" in the preference page.
 */
public class MiningWorkbenchPreferenceInitializer extends AbstractPreferenceInitializer {

	@Override
	public void initializeDefaultPreferences() {
		MiningPreferences.setStoreDefaults();
	}

}
