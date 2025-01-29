/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.preferences.ui;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.keycloak.adapters.ServerRequest.HttpFailure;

import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ui.WorkbenchUtil;
/**
 * Helper class for Mining preference pages.
 */
final class MiningPreferencePageHelper {
	
	private MiningPreferencePageHelper () {}

	/**
	 * Logs and displays an appropriate error message to the user.
	 *
	 * @param exception the exception which occurred during the logon process
	 */
	static void handleLogonException(final Exception exception) {
		final Throwable rootCause = ExceptionUtils.getRootCause(exception);
		final String message;
		if (rootCause instanceof HttpFailure) {
			message = ((HttpFailure) rootCause).getError();
		} else {
			message = ExceptionUtils.getRootCauseMessage(exception);
		}
		Logging.error(message, exception);
		/* Using setErrorMessage is not possible due to being called from different threads
		 * and potentially having the preference page already closed, so we will display an error dialog. */
		Display.getDefault().asyncExec(() -> MessageDialog.openError(WorkbenchUtil.getActiveShell(), "Error on Logon", message));
	}
}
