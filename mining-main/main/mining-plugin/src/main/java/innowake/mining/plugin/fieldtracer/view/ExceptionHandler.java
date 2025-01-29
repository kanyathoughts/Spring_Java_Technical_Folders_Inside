/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.view;

import java.util.concurrent.CancellationException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.statushandlers.StatusManager;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

/**
 * Used to display a error message via dialog to the user.
 * The exception information are formatted and shown as details.
 */
class ExceptionHandler {
	
	private final Shell shell;
	
	private static final Logger LOG = LoggerFactory.getLogger(ExceptionHandler.class);
	
	/**
	 * Create a new instance with shell access.
	 * @param shell The shell instance. Must not be null.
	 */
	ExceptionHandler(final Shell shell) {
		this.shell = shell;
	}
	
	void handle(final String message, final Exception exception) {
		if (exception instanceof CancellationException) {
			return;
		}
		LOG.error("Exception occured: " + message, exception);
		MessageDialog.openError(shell, message, getDetailMessage(exception));
		StatusManager.getManager().handle(buildStatus(message, exception));
	}
	
	private String getDetailMessage(final Exception exception) {
		final StringBuilder msgBuilder = new StringBuilder(getErrorHeaderText(exception));
		msgBuilder.append("See the console or eclipse error log for details.").append("\n\r");
		return msgBuilder.toString();
	}
	
	private IStatus buildStatus(final String message, final Exception exception) {
		return new Status(IStatus.ERROR, FieldtracerView.ID , String.format("%s: %s", message, getErrorHeaderText(exception)), exception);
	}

	private String getErrorHeaderText(final Exception ex) {
		final StringBuilder msgBuilder = new StringBuilder(50);
		msgBuilder.append("An error has occurred.").append("\n\r");
		msgBuilder.append("Message: ").append(ex.getMessage()).append("\n\r");
		
		if (ex.getCause() != null) {
			msgBuilder.append("Cause:\n\r");
			msgBuilder.append(ex.getCause().getMessage());
			msgBuilder.append("\n\r");
		}
		return msgBuilder.toString();
	}
}
