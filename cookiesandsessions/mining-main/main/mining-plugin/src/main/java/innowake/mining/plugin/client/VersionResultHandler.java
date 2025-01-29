/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import static innowake.mining.plugin.client.VersionChecker.VERSION_MISMATCH_MESSAGE_PATTERN;
import static innowake.mining.plugin.client.VersionChecker.VERSION_MISMATCH_TITLE;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.swt.widgets.Display;

import innowake.base.eclipse.common.ui.util.WorkbenchUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.client.VersionChecker.VersionResult;
import innowake.mining.plugin.preferences.MiningPreferences;

/**
 * Handles the {@link VersionResult result} of a {@link VersionChecker version check}.
 */
public class VersionResultHandler {
	
	private VersionResultHandler() {}

	/**
	 * Log and display an error dialog if the given version result does not match.
	 * <p>
	 * A message is always logged in case of a mismatch, but the error dialog is only shown conditionally. 
	 *
	 * @param project the project which was checked, if {@code null} it's assumed that the workspace-configured server was checked
	 * @param serverUrl the URL of the server for which the result is applicable
	 * @param result the result of the version check
	 * @return the error message, which can be shown to the user; the caller must check if the result did not match before showing the message to the user
	 */
	public static String handle(@Nullable final IProject project, final String serverUrl, final VersionResult result) {
		final String serverVersion = result.serverVersion;
		final String target = project != null ? "project '" + project.getName() + "'" : "the workspace";
		final String message = String.format(VERSION_MISMATCH_MESSAGE_PATTERN, target, serverVersion, serverUrl, result.pluginVersion);

		/* In case of a mismatch always log the message */
		if ( ! result.match) {
			Logging.error(message);
		} else if ( ! serverUrl.isEmpty()) {
			/* Reset the current URL if the versions matched */
			MiningPreferences.removeIgnoredUrl(serverUrl);
		}

		/* Show dialog in case of mismatch, the URL with the version was not already ignored and no other version mismatch dialog is shown. */
		if ( ! result.match 
				&& MiningPreferences.shouldIgnoreUrl(serverUrl, serverVersion) 
				&& VersionChecker.DIALOG_ALREADY_SHOWING.compareAndSet(false, true)) {
			Display.getDefault().asyncExec(() -> {
				final MessageDialogWithToggle dialogWithToggle = MessageDialogWithToggle.openWarning(
						WorkbenchUtil.getActiveShell(),
						VERSION_MISMATCH_TITLE,
						message,
						"Do not remind me again for this server version",
						false,
						null,
						null);

				VersionChecker.DIALOG_ALREADY_SHOWING.set(false);

				if (dialogWithToggle.getToggleState()) {
					MiningPreferences.addIgnoredUrl(serverUrl, serverVersion);
				}
			});
		}
		return message;
	}

}
