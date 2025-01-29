/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.view.action;

import java.net.URL;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.osgi.framework.Bundle;

import innowake.mining.job.plugin.MiningJobPlugin;
import innowake.mining.job.plugin.manager.RemoteJobManager;
import innowake.mining.job.plugin.view.JobView;

/**
 * This is a toolbar action for the {@link JobView} that refreshes all job information of the {@link RemoteJobManager}.
 * If the {@link RemoteJobManager} is currently in offline mode, a manual refresh will not be possible.
 */
public class RefreshAction extends Action {
	
	private static final String TOOLTIP_NO_CONNECTION = "Not connected to job server";
	private static final String TOOLTIP_CONNECTED = "Manually sync jobs with the job server";

	/**
	 * Constructor.
	 */
	public RefreshAction() {
		final Bundle bundle = Platform.getBundle(MiningJobPlugin.ID);
		final URL refreshUrl = bundle.getResource("icons/refresh.png");
		final URL refreshDisabledUrl = bundle.getResource("icons/refresh_disabled.png");
		
		setImageDescriptor(ImageDescriptor.createFromURL(refreshUrl));
		setDisabledImageDescriptor(ImageDescriptor.createFromURL(refreshDisabledUrl));
	}
	
	@Override
	public void setEnabled(final boolean enabled) {
		super.setEnabled(enabled);
		setToolTipText(enabled ? TOOLTIP_CONNECTED : TOOLTIP_NO_CONNECTION);
	}
	
	@Override
	public void run() {
		MiningJobPlugin.getDefaultNonNull().getRemoteJobManager().updateJobInformation();
	}
}
