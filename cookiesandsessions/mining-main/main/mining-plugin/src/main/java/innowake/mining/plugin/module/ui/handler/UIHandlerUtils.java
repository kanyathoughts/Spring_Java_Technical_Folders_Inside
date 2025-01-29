/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiFunction;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * Utilities for Handling UI.
 */
final class UIHandlerUtils {

	private UIHandlerUtils() {
		throw new IllegalStateException("Can't instantiate this class");
	}
	
	/**
	 * Returns a Binary function that opens a dialog for showing messages to the user and get the response. 
	 * The BiFunction takes two input parameters first one is the message for the user and second one is title for message dialog.
	 * 
	 * @param shell The Active shell instance.
	 * @return A Binary function.
	 */
	public static BiFunction<String, String, Boolean> getUserResponseFunction(final Shell shell) {
		return (message, title) -> {
			final AtomicBoolean answer = new AtomicBoolean();
			Display.getDefault().syncExec(() -> answer.set(MessageDialog.openQuestion(shell, title, message)));
			return Boolean.valueOf(answer.get());
		};
	}
	
	/**
	 * Opens a modal error dialog with the specified title and message.
	 *
	 * @param shell The active shell instance
	 * @param title Title of the error dialog
	 * @param message Message of the error dialog
	 */
	public static void showErrorToUser(final Shell shell, final String title, final String message) {
		Display.getDefault().syncExec(() -> MessageDialog.openError(shell, title, message));
	}
}
