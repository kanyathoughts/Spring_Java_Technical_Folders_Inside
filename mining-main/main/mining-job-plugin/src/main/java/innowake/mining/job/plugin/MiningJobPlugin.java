/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin;

import static innowake.lib.core.lang.Assert.assertNotNull;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.osgi.framework.BundleContext;

import innowake.base.eclipse.common.ui.AbstractUIPlugin;
import innowake.base.eclipse.common.ui.util.WorkbenchUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.job.plugin.manager.RemoteJobManager;

/**
 * Activator class for the mining job plugin.
 */
public class MiningJobPlugin extends AbstractUIPlugin {

	/**
	 * Plugin ID of the Mining Job Plugin
	 */
	public static final String ID = "innowake.mining.job.plugin";
	
	@Nullable
	private static MiningJobPlugin plugin;
	
	@Nullable
	private RemoteJobManager remoteJobManager;
	
	/**
	 * Called if a new instance of the mining plugin is created.
	 * Used to store the singleton instance.
	 */
	public MiningJobPlugin() {
		super(ID);
	}

	/**
	 * Return the shared instance (if set) for this plugin.
	 *
	 * @return The shared plugin instance or {@code null}.
	 */
	@Nullable
	public static final synchronized MiningJobPlugin getDefault() {
		return plugin;
	}
	
	/**
	 * Return the shared instance (if set) for this plugin. If not set this method throws an {@code IllegalStateException}.
	 * 
	 * @return the shared plugin instance; not {@code null}
	 * @throws IllegalStateException if not set
	 */
	public static final MiningJobPlugin getDefaultNonNull() {
		if (plugin != null) {
			return plugin;
		}
		throw new IllegalStateException("Plugin " + MiningJobPlugin.ID + " is null.");
	}
	
	/**
	 * Logs the error and optionally shows an error dialog.
	 * 
	 * @param title the title of the error
	 * @param e the {@link Throwable}
	 * @param showErrorDialog {@code true} to show an error dialog; {@code false} to only log
	 */
	public static void handleError(final String title, final Throwable e, final boolean showErrorDialog) {
		Logging.error(title, e);
		
		if (showErrorDialog) {
			Display.getDefault().asyncExec(() -> MessageDialog.openError(WorkbenchUtil.getActiveShell(), title, e.getLocalizedMessage()));
		}
	}
	
	/**
	 * Logs the error and optionally shows an error dialog.
	 * 
	 * @param title the title of the error
	 * @param message the error message
	 * @param showErrorDialog {@code true} to show an error dialog; {@code false} to only log
	 */
	public static void handleError(final String title, final String message, final boolean showErrorDialog) {
		Logging.error(message);
		
		if (showErrorDialog) {
			Display.getDefault().asyncExec(() -> MessageDialog.openError(WorkbenchUtil.getActiveShell(), title, message));
		}
	}

	@Override
	protected void startInternal(@Nullable final BundleContext context) throws Exception {
		setPlugin(this);
		getRemoteJobManager().init();
	}

	@Override
	protected void stopInternal(@Nullable final BundleContext context) throws Exception {
		setPlugin(null);
		getRemoteJobManager().stop();
	}
	
	/**
	 * @return the {@link RemoteJobManager} instance
	 */
	public RemoteJobManager getRemoteJobManager() {
		if (remoteJobManager == null) {
			/* This points to the plugin metadata directory. */
			final IPath storagePath = MiningJobPlugin.getDefaultNonNull().getStateLocation();
			remoteJobManager = new RemoteJobManager(storagePath);
		}
		return assertNotNull(remoteJobManager);
	}
	
	private static synchronized void setPlugin(@Nullable final MiningJobPlugin miningJobPlugin) {
		plugin = miningJobPlugin;
	}
}
